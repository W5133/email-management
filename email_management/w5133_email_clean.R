
# email_validation.R
#
# Purpose:
# Clean and validate an email list without sending email.
#
# Input:
# A CSV file with either "email" or local/domain columns
#
# Output:
# A CSV with validation flags and an overall status,
# plus a cleaned email list
setwd("~/Documents")
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(curl)
})

# ----------------------------
# User settings
# ----------------------------
input_file  <- "w5133_emails_2026.csv"
output_file <- "w5133_emails_validated.csv"
clean_output_file <- "w5133_emails_cleaned.csv"

# Optional: add or remove disposable domains here
disposable_domains <- c(
  "mailinator.com", "guerrillamail.com", "10minutemail.com",
  "temp-mail.org", "yopmail.com", "trashmail.com"
)

# Common role-based inboxes; not necessarily invalid, but often less useful
role_prefixes <- c(
  "admin", "info", "support", "contact", "office", "hello",
  "billing", "sales", "webmaster", "noreply", "no-reply", "help"
)

# ----------------------------
# Helper functions
# ----------------------------

normalize_email <- function(x) {
  pattern <- "[A-Za-z0-9.!#$%&'*+/=?^_`{|}~-]+@[A-Za-z0-9-]+(?:\\.[A-Za-z0-9-]+)+"
  x_chr <- as.character(x)
  x_trim <- str_squish(x_chr)

  # Handle display-name formats like: "Last, First" <user@domain.com>
  extracted <- ifelse(
    str_detect(x_trim, "<[^<>]+>"),
    str_match(x_trim, "<\\s*([^<>\\s]+)\\s*>")[, 2],
    x_trim
  )

  extracted |>
    str_remove("^mailto:") |>
    str_remove('^"') |>
    str_remove('"$') |>
    str_extract(pattern) |>
    str_to_lower()
}

normalize_local_part <- function(x) {
  x |>
    str_trim() |>
    str_to_lower() |>
    str_remove("@.*$")
}

normalize_domain <- function(x) {
  x |>
    str_trim() |>
    str_to_lower() |>
    str_remove("^@+")
}

reassemble_email <- function(local_part, domain) {
  local_norm <- normalize_local_part(local_part)
  domain_norm <- normalize_domain(domain)

  ifelse(
    is.na(local_norm) | local_norm == "" | is.na(domain_norm) | domain_norm == "",
    NA_character_,
    paste0(local_norm, "@", domain_norm)
  )
}

is_valid_syntax <- function(x) {
  # Reasonable practical regex; not a full RFC parser
  pattern <- "^[A-Za-z0-9.!#$%&'*+/=?^_`{|}~-]+@[A-Za-z0-9-]+(?:\\.[A-Za-z0-9-]+)+$"
  !is.na(x) & str_detect(x, pattern)
}

get_local_part <- function(x) {
  ifelse(str_detect(x, "@"), str_extract(x, "^[^@]+"), NA_character_)
}

get_domain <- function(x) {
  ifelse(str_detect(x, "@"), str_extract(x, "(?<=@).+$"), NA_character_)
}

has_bad_pattern <- function(x) {
  # Flag obvious junk/test entries
  bad_patterns <- c(
    "^test@", "^asdf@", "^example@", "^fake@", "^none@",
    "^noemail@", "^unknown@", "^na@", "^n/a@"
  )
  if (is.na(x)) return(TRUE)
  any(str_detect(x, bad_patterns))
}

check_domain_a <- function(domain) {
  if (is.na(domain) || domain == "") return(FALSE)
  out <- tryCatch(curl::nslookup(domain), error = function(e) NULL)
  length(out) > 0
}

check_domain_mx <- function(domain) {
  if (is.na(domain) || domain == "") return(FALSE)
  out <- tryCatch(curl::nslookup(domain, type = "MX"), error = function(e) NULL)
  length(out) > 0
}

is_role_account <- function(local_part) {
  if (is.na(local_part)) return(FALSE)
  local_part %in% role_prefixes
}

is_disposable_domain <- function(domain) {
  if (is.na(domain)) return(FALSE)
  domain %in% disposable_domains
}

# ----------------------------
# Read input
# ----------------------------

df <- read_csv(input_file, show_col_types = FALSE)

email_col <- if ("email" %in% names(df)) "email" else NA_character_
local_col <- dplyr::coalesce(
  intersect(c("local_part", "local", "email_local", "user", "username"), names(df))[1],
  NA_character_
)
domain_col <- dplyr::coalesce(
  intersect(c("domain", "email_domain"), names(df))[1],
  NA_character_
)

if (is.na(email_col) && (is.na(local_col) || is.na(domain_col))) {
  stop("Input file must contain either 'email' or both local/domain columns (e.g., local_part + domain).")
}

email_raw <- if (!is.na(email_col)) as.character(df[[email_col]]) else rep(NA_character_, nrow(df))
email_from_parts <- if (!is.na(local_col) && !is.na(domain_col)) {
  reassemble_email(df[[local_col]], df[[domain_col]])
} else {
  rep(NA_character_, nrow(df))
}

# Prefer reconstructed addresses when available; otherwise use the original email field.
df <- df %>%
  mutate(
    email_source_raw = email_raw,
    email_reassembled = email_from_parts,
    email = ifelse(
      !is.na(email_reassembled) & email_reassembled != "",
      email_reassembled,
      email_source_raw
    )
  )

# ----------------------------
# Clean + basic validation
# ----------------------------

results <- df %>%
  mutate(
    email_raw        = email,
    email            = normalize_email(email),
    email_missing    = is.na(email) | email == "",
    valid_syntax     = is_valid_syntax(email),
    local_part       = get_local_part(email),
    domain           = get_domain(email),
    obvious_junk     = map_lgl(email, has_bad_pattern),
    role_account     = map_lgl(local_part, is_role_account),
    disposable       = map_lgl(domain, is_disposable_domain)
  )

# ----------------------------
# Duplicate detection
# ----------------------------

results <- results %>%
  add_count(email, name = "email_count") %>%
  mutate(
    duplicate_email = !email_missing & email_count > 1
  ) %>%
  select(-email_count)

# ----------------------------
# Domain checks
# Only do DNS checks for syntactically valid emails
# and cache unique domains for speed
# ----------------------------

domains_to_check <- results %>%
  filter(valid_syntax, !is.na(domain), domain != "") %>%
  distinct(domain) %>%
  pull(domain)

domain_checks <- tibble(domain = domains_to_check) %>%
  mutate(
    domain_resolves = map_lgl(domain, check_domain_a),
    has_mx_record   = map_lgl(domain, check_domain_mx)
  )

results <- results %>%
  left_join(domain_checks, by = "domain") %>%
  mutate(
    domain_resolves = ifelse(is.na(domain_resolves), FALSE, domain_resolves),
    has_mx_record   = ifelse(is.na(has_mx_record), FALSE, has_mx_record)
  )

# ----------------------------
# Overall classification
# ----------------------------

results <- results %>%
  mutate(
    validation_status = case_when(
      email_missing ~ "missing",
      !valid_syntax ~ "invalid_syntax",
      obvious_junk ~ "obvious_junk",
      !domain_resolves ~ "domain_not_found",
      !has_mx_record ~ "no_mx_record",
      TRUE ~ "likely_valid"
    ),
    review_flag = case_when(
      validation_status != "likely_valid" ~ TRUE,
      duplicate_email ~ TRUE,
      disposable ~ TRUE,
      role_account ~ TRUE,
      TRUE ~ FALSE
    )
  )

# ----------------------------
# Reorder useful columns
# ----------------------------

results <- results %>%
  select(
    everything(),
    email_raw,
    email,
    validation_status,
    review_flag,
    duplicate_email,
    valid_syntax,
    domain_resolves,
    has_mx_record,
    role_account,
    disposable,
    obvious_junk,
    local_part,
    domain
  ) %>%
  relocate(
    email_raw, email, validation_status, review_flag,
    duplicate_email, valid_syntax, domain_resolves, has_mx_record,
    role_account, disposable, obvious_junk, local_part, domain
  )

# ----------------------------
# Write output
# ----------------------------

write_csv(results, output_file)

# Keep one row per cleaned address for downstream use.
clean_emails <- results %>%
  filter(!email_missing, valid_syntax) %>%
  distinct(email) %>%
  arrange(email) %>%
  rename(clean_email = email)

#Have LLM extract emails from pdf of member list
members <- read_csv("w5133_member_emails.csv") %>%
  mutate(member=T)

members %>%
  full_join(clean_emails,by=c("email"="clean_email")) %>%
  mutate(member = ifelse(is.na(member),F,T)) %>%
  write_csv(clean_output_file)


# ----------------------------
# Summary to console
# ----------------------------

cat("\nValidation complete.\n")
cat("Input rows:", nrow(df), "\n")
cat("Output file:", output_file, "\n\n")
cat("Clean email list:", clean_output_file, "\n\n")

summary_table <- results %>%
  count(validation_status, sort = TRUE)

print(summary_table)

cat("\nRows flagged for review:", sum(results$review_flag, na.rm = TRUE), "\n")
