#Read member list from googlesheet and send out

# Load the pre-mail sync definitions from beside this script.
.mailer_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
.mailer_dir <- if (is.null(.mailer_file)) {
  if (file.exists("sync_member_list.R")) "." else "email_management"
} else dirname(.mailer_file)
source(file.path(.mailer_dir, "sync_member_list.R"))

# Packages
library(googlesheets4)
library(dplyr)
library(glue)
library(Microsoft365R)
library(blastula)
library(htmltools)

# -----------------------------
# 1) SETTINGS
# -----------------------------

# Recipient selection (subscription and email checks always apply)
# "all", "members" (member == TRUE), or "nonmembers" (member == FALSE)
member_filter <- "all"

# "all", "not_submitted" (NA), or "submitted" (any nonmissing value)
# This preserves the sheet's existing convention: NA means not submitted.
report_filter <- "all"
# For a meeting announcement to everyone, set both filters to "all".

# Optional: send drafts only to yourself while testing
test_mode <- F
test_email <- "jbayham@colostate.edu"

#Send emails or just draft (FALSE)
send_now <- T

# Google Sheet URL or Sheet ID
sheet_url <- "https://docs.google.com/spreadsheets/d/1JEyDDqVjKpPppmDvRs0W17pux8bbUMBEzPLHNS_rH-8"

# Name of the worksheet tab
sheet_tab <- "members"

# "apply" updates and verifies the list before Outlook is opened.
# "preview" reads only and stops the mailer before any drafts or sends.
member_sync_mode <- "apply"

# Published Google Form; opening this link does not unsubscribe anyone.
base_unsub_url <- "https://docs.google.com/forms/d/e/1FAIpQLSe0hY9npf83q2eBP15zk7RUa4hDVQfzwpj9oqe3-kQaW2onwA/viewform"
unsub_email_entry <- "entry.2066026793"

build_unsubscribe_url <- function(email) {
  paste0(base_unsub_url, "?usp=pp_url&", unsub_email_entry, "=",
         utils::URLencode(trimws(email), reserved = TRUE, repeated = TRUE))
}

# Optional: cap number of drafts created while testing
draft_limit <- 5

# Subject line
email_subject <- "W5133 2027 Meeting"

cc_email = "wx133.org@gmail.com"

# External HTML email template file
#email_template_file <- "e1_post_meeting.txt"
email_template_file <- "e3_2027_meeting_announcement.txt"

# -----------------------------
# 2) AUTHENTICATE
# -----------------------------

# Reject misspelled filter settings before connecting to either service.
stopifnot(
  length(member_filter) == 1L,
  member_filter %in% c("all", "members", "nonmembers"),
  length(report_filter) == 1L,
  report_filter %in% c("all", "not_submitted", "submitted")
)

# Google Sheets auth
# First run will usually open a browser for OAuth
gs4_auth(email = "wx133.org@gmail.com")

# A failed sync stops execution before Outlook or recipient selection.
member_sync_result <- sync_member_list(sheet_url, sheet_tab, mode = member_sync_mode)
if (member_sync_mode == "preview") {
  stop("Member-list preview complete. Review it, then set member_sync_mode to 'apply' to continue.")
}

# Outlook / Microsoft 365 auth
# First run will usually open a browser for OAuth
outl <- get_business_outlook()

# -----------------------------
# 3) READ MEMBER LIST
# -----------------------------

members <- read_sheet(sheet_url, sheet = sheet_tab) %>%
  rename_with(tolower) %>%
  mutate(
    row_id = row_number() + 1,   # +1 because sheet row 1 is header
    email = trimws(email),
    cc_email = cc_email,
    status = tolower(trimws(status))
  )

# Always require subscription and email. Tokens are no longer used.
recipients <- members %>%
  filter(
    !is.na(email), email != "",
    status == "subscribed"
  )

# Apply optional audience filters selected in SETTINGS.
if (member_filter != "all") {
  recipients <- recipients %>%
    filter(member == (member_filter == "members"))
}
if (report_filter != "all") {
  recipients <- recipients %>%
    filter(is.na(report_submitted) == (report_filter == "not_submitted"))
}

# Optional testing behavior
if (test_mode) {
  recipients <- recipients %>%
    slice_head(n = draft_limit) %>%
    mutate(actual_recipient = email,
           email = test_email)
} else {
  recipients <- recipients %>%
    mutate(actual_recipient = email)
}

# Build after test redirection so test messages prefill only the test address.
recipients <- recipients %>%
  mutate(unsubscribe_url = build_unsubscribe_url(email))

# Safety check
if (nrow(recipients) == 0) {
  stop("No eligible subscribed recipients were found.")
}

# -----------------------------
# 4) DRY-RUN TABLE
# -----------------------------

dry_run <- recipients %>%
  transmute(
    row_id,
    actual_recipient,
    draft_to = email,
    status,
    unsubscribe_url
  )

print(dry_run, n = nrow(dry_run))

# Optional safety stop:
# Uncomment this if you always want to inspect first and then rerun
#stop("Dry-run complete. Review recipients above, then comment out this line to create drafts.")



# -----------------------------
# 5) EMAIL BODY FUNCTION
# -----------------------------


build_email_html <- function(unsub_url) {
  candidate_paths <- c(
    email_template_file,
    file.path("email_management", email_template_file)
  )
  template_path <- candidate_paths[file.exists(candidate_paths)][1]

  if (is.na(template_path)) {
    stop("Template file not found. Checked: ", paste(candidate_paths, collapse = ", "))
  }

  template_html <- paste(readLines(template_path, warn = FALSE), collapse = "\n")
  unsub_url <- htmltools::htmlEscape(unsub_url, attribute = TRUE)
  glue(template_html)
}

# -----------------------------
# 5) CREATE DRAFTS IN OUTLOOK
# -----------------------------

draft_results <- vector("list", nrow(recipients))

for (i in seq_len(nrow(recipients))) {
  to_email <- recipients$email[i]
  unsub_url <- recipients$unsubscribe_url[i]

  html_body <- build_email_html(unsub_url)

  email_obj <- compose_email(
    body = htmltools::HTML(html_body)
  )

  #Construct message
  msg <- outl$create_email(
    email_obj,
    subject = email_subject,
    to = to_email,
    cc = cc_email
  )

  #Toggle above to send
  if (send_now) {
    msg$send()
    action <- "sent"
    Sys.sleep(10)
    message("Sent to ",to_email)
  } else {
    action <- "drafted"
  }

  draft_results[[i]] <- tibble(
    row_id = recipients$row_id[i],
    actual_recipient = recipients$actual_recipient[i],
    draft_to = to_email,
    action = action,
    action_time = as.character(Sys.time())
  )
}

message("Finished")
