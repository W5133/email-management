# Source to define functions; use preview_nimss_members(), then apply_nimss_members().
# Packages: googlesheets4, rvest, xml2. No mail is sent by this script.
NIMSS_URL <- "https://nimss.org/projects/view/participant_list/18858"
NIMSS_SHEET <- "1JEyDDqVjKpPppmDvRs0W17pux8bbUMBEzPLHNS_rH-8"
nimss_email <- function(x) { x[is.na(x)] <- ""; tolower(trimws(x)) }

read_nimss_members <- function(source = NIMSS_URL) {
  doc <- xml2::read_html(source)
  tables <- rvest::html_elements(doc, "table")
  tables <- tables[vapply(tables, function(t) all(c("Participant Name", "Contact Info") %in%
                       trimws(rvest::html_text2(rvest::html_elements(t, "th")))), logical(1))]
  if (length(tables) != 1L) stop("Expected one NIMSS participant table; no changes permitted.")
  rows <- rvest::html_elements(tables[[1]], "tbody tr")
  people <- lapply(rows, function(row) {
    links <- rvest::html_elements(row, 'a[href^="mailto:"]')
    email <- nimss_email(utils::URLdecode(sub("\\?.*$", "", sub("^mailto:", "",
                                                 rvest::html_attr(links, "href")))))
    if (length(email) != 1L || !grepl("^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$", email)) {
      stop("A participant row has a missing/ambiguous email; review NIMSS before changing membership.")
    }
    name <- rvest::html_text2(rvest::html_element(row, 'a[href^="/users/"]'))
    data.frame(email = email, name = ifelse(is.na(name), "", trimws(name)))
  })
  if (!length(people)) stop("NIMSS roster is empty; refusing to clear membership.")
  people <- do.call(rbind, people)
  people[!duplicated(people$email), , drop = FALSE]
}
nimss_read_sheet <- function(ss) {
  as.data.frame(googlesheets4::read_sheet(ss, sheet = "members", col_types = "c",
                                         trim_ws = FALSE, .name_repair = "minimal"))
}
nimss_plan <- function(members, roster) {
  headers <- tolower(trimws(names(members)))
  if (anyDuplicated(headers) || !all(c("email", "member", "status") %in% headers)) {
    stop("members requires unique email, member, and status columns.")
  }
  names(members) <- headers
  emails <- nimss_email(members$email)
  desired <- emails %in% roster$email & nzchar(emails)
  current <- toupper(trimws(members$member))
  changed <- which(nzchar(emails) & (is.na(current) | current != as.character(desired)))
  additions <- roster[!roster$email %in% emails, , drop = FALSE]
  list(changes = data.frame(row = changed + 1L, email = emails[changed],
                           old_member = current[changed], member = desired[changed]),
       additions = additions, headers = headers)
}
preview_nimss_members <- function(ss = NIMSS_SHEET) {
  roster <- read_nimss_members()
  members <- nimss_read_sheet(ss)
  plan <- nimss_plan(members, roster)
  cat(nrow(roster), "unique NIMSS addresses. Membership changes:\n")
  print(plan$changes, row.names = FALSE)
  cat("New addresses (will be added as subscribed members):\n")
  print(plan$additions, row.names = FALSE)
  invisible(list(ss = ss, members = members, roster = roster, plan = plan))
}
apply_nimss_members <- function(preview) {
  current <- nimss_read_sheet(preview$ss)
  if (!identical(current, preview$members) || !identical(read_nimss_members(), preview$roster)) {
    stop("Sheet or NIMSS roster changed. Preview again before applying.")
  }
  plan <- nimss_plan(current, preview$roster)
  if (!nrow(plan$changes) && !nrow(plan$additions)) return(invisible(message("Already up to date.")))
  meta <- googlesheets4::gs4_get(preview$ss)
  id <- meta$sheets$id[match("members", meta$sheets$name)]
  cell <- function(x) list(userEnteredValue = if (is.logical(x)) list(boolValue = x) else list(stringValue = x))
  requests <- lapply(seq_len(nrow(plan$changes)), function(i) list(updateCells = list(
    start = list(sheetId = id, rowIndex = plan$changes$row[i] - 1L,
                 columnIndex = match("member", plan$headers) - 1L),
    rows = list(list(values = list(cell(plan$changes$member[i])))), fields = "userEnteredValue")))
  if (nrow(plan$additions)) {
    # Grow the grid, then write fixed rows rather than appendCells: HTTP retries
    # cannot append duplicate members. Existing cells/formulas are not rewritten.
    requests <- c(requests, list(list(appendDimension = list(sheetId = id, dimension = "ROWS",
                                                            length = nrow(plan$additions)))))
    added_rows <- lapply(seq_len(nrow(plan$additions)), function(i) {
      values <- setNames(rep(list(""), length(plan$headers)), plan$headers)
      values$email <- plan$additions$email[i]; values$member <- TRUE; values$status <- "subscribed"
      if ("name" %in% names(values)) values$name <- plan$additions$name[i]
      if ("added_at" %in% names(values)) values$added_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      list(values = unname(lapply(values, cell)))
    })
    requests <- c(requests, list(list(updateCells = list(
      start = list(sheetId = id, rowIndex = nrow(current) + 1L, columnIndex = 0L),
      rows = added_rows, fields = "userEnteredValue"))))
  }
  request <- googlesheets4::request_generate("sheets.spreadsheets.batchUpdate", params = list(
    spreadsheetId = as.character(googlesheets4::as_sheets_id(preview$ss)), requests = requests))
  error <- tryCatch({googlesheets4::request_make(request); NULL}, error = identity)
  after <- nimss_read_sheet(preview$ss)
  remaining <- nimss_plan(after, preview$roster)
  # Verify convergence AND preservation of every existing cell except member.
  existing_columns <- which(plan$headers != "member")
  if (nrow(remaining$changes) || nrow(remaining$additions) ||
      nrow(after) != nrow(current) + nrow(plan$additions) ||
      !identical(after[seq_len(nrow(current)), existing_columns, drop = FALSE],
                 current[, existing_columns, drop = FALSE])) {
    stop("Update could not be verified. Inspect the sheet and make a fresh preview before retrying.",
         if (!is.null(error)) paste(" Write error:", conditionMessage(error)))
  }
  message("Verified: NIMSS membership synchronized; existing subscriptions and other fields preserved.")
  invisible(after)
}
