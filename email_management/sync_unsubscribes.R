# Source this file to define functions only. No authentication, writes, or email
# sending occurs until you explicitly call preview/apply.
UNSUB_SHEET <- "1JEyDDqVjKpPppmDvRs0W17pux8bbUMBEzPLHNS_rH-8"
UNSUB_CONFIRMATION <- "Please unsubscribe this email address from W5133 emails."
UNSUB_LOG_COLUMNS <- c("request_id", "fingerprint", "email", "submitted_at",
                       "processed_at", "outcome", "member_rows")

unsub_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}
unsub_email <- function(x) tolower(trimws(unsub_text(x)))
unsub_frame <- function(x) {
  as.data.frame(lapply(x, unsub_text), check.names = FALSE, stringsAsFactors = FALSE)
}
unsub_empty_log <- function() {
  as.data.frame(setNames(rep(list(character()), length(UNSUB_LOG_COLUMNS)),
                         UNSUB_LOG_COLUMNS), check.names = FALSE)
}
unsub_require <- function(x, columns, label) {
  if (anyDuplicated(names(x)) || !all(columns %in% names(x))) {
    stop(label, " must have unique headers including: ", paste(columns, collapse = ", "))
  }
}

# Keep response rows append-only. Row identity prevents edited requests replaying;
# fingerprints detect edits, sorting, or deletion of previously processed rows.
unsub_plan <- function(members, responses, log = unsub_empty_log(), response_id = 0L,
                       aliases = character()) {
  members <- unsub_frame(members)
  responses <- unsub_frame(responses)
  log <- unsub_frame(log)
  names(members) <- tolower(trimws(names(members)))
  response_cols <- c("Timestamp", "Email address to unsubscribe",
                     "Reason for unsubscribing (optional)", "Confirm your unsubscribe request")
  unsub_require(members, c("email", "status", "unsubscribed_at"), "members")
  unsub_require(responses, response_cols, "Form responses")
  unsub_require(log, UNSUB_LOG_COLUMNS, "Processing log")
  if (anyDuplicated(log$request_id) || any(!nzchar(log$request_id))) stop("Invalid processing log IDs.")
  ids <- if (nrow(responses)) paste(response_id, seq_len(nrow(responses)) + 1L, sep = ":") else character()
  fingerprints <- vapply(seq_len(nrow(responses)), function(i) {
    fields <- unlist(responses[i, response_cols], use.names = FALSE)
    digest::digest(paste0(nchar(fields, type = "bytes"), ":", fields, collapse = ""),
                   algo = "sha256", serialize = FALSE)
  }, character(1))
  old <- match(log$request_id, ids)
  if (anyNA(old) || any(log$fingerprint != fingerprints[old])) {
    stop("Processed response rows changed, moved, or disappeared. Restore the response history before syncing.")
  }
  emails <- unsub_email(responses[[response_cols[2]]])
  member_emails <- unsub_email(members$email)
  status <- unsub_email(members$status)
  preview <- data.frame(request_id = ids, response_row = seq_len(nrow(responses)) + 1L,
                        email = emails, submitted_at = responses[[response_cols[1]]],
                        outcome = rep("", length(ids)), member_rows = rep("", length(ids)),
                        duplicate_member = rep(FALSE, length(ids)), stringsAsFactors = FALSE)
  updates <- data.frame(row = integer(), old_status = character(), old_unsubscribed_at = character(),
                        status = character(), unsubscribed_at = character())
  records <- unsub_empty_log()
  for (i in seq_along(ids)) {
    resolved <- if (emails[i] %in% names(aliases)) unname(aliases[emails[i]]) else emails[i]
    matched <- which(member_emails %in% c(emails[i], resolved) & nzchar(member_emails))
    preview$member_rows[i] <- paste(matched + 1L, collapse = ",")
    preview$duplicate_member[i] <- length(matched) > 1L
    outcome <- if (ids[i] %in% log$request_id) "processed" else if (
      responses[[response_cols[4]]][i] != UNSUB_CONFIRMATION) "unconfirmed" else if (
      !nzchar(trimws(preview$submitted_at[i])) ||
      !grepl("^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$", emails[i])) "invalid" else if (
      !length(matched)) "unmatched" else if (all(status[matched] == "unsubscribed")) "already_unsubscribed" else "unsubscribe"
    preview$outcome[i] <- outcome
    if (!outcome %in% c("unsubscribe", "already_unsubscribed")) next
    for (r in matched[status[matched] != "unsubscribed"]) {
      # Multiple requests for one address result in just one member update.
      updates <- rbind(updates, data.frame(row = r + 1L, old_status = members$status[r],
                         old_unsubscribed_at = members$unsubscribed_at[r], status = "unsubscribed",
                         unsubscribed_at = preview$submitted_at[i]))
      status[r] <- "unsubscribed"
    }
    records <- rbind(records, data.frame(request_id = ids[i], fingerprint = fingerprints[i],
                      email = emails[i], submitted_at = preview$submitted_at[i], processed_at = "",
                      outcome = outcome, member_rows = preview$member_rows[i]))
  }
  list(preview = preview, updates = updates, records = records, members = members)
}

unsub_read <- function(ss, members_tab, responses_tab, log_tab) {
  meta <- googlesheets4::gs4_get(ss)
  sheets <- meta$sheets
  read_tab <- function(tab) unsub_frame(googlesheets4::read_sheet(
    ss, sheet = tab, col_types = "c", trim_ws = FALSE, .name_repair = "minimal"))
  if (!all(c(members_tab, responses_tab) %in% sheets$name)) stop("Required sheet tabs are missing.")
  list(members = read_tab(members_tab), responses = read_tab(responses_tab),
       log = if (log_tab %in% sheets$name) read_tab(log_tab) else unsub_empty_log(),
       member_id = sheets$id[match(members_tab, sheets$name)],
       response_id = sheets$id[match(responses_tab, sheets$name)],
       log_id = if (log_tab %in% sheets$name) sheets$id[match(log_tab, sheets$name)] else NA_integer_,
       sheet_ids = sort(sheets$id))
}

preview_unsubscribes <- function(ss = UNSUB_SHEET, members_tab = "members",
                                 responses_tab = "Form Responses 3",
                                 log_tab = "unsubscribe_sync_log", .read = unsub_read) {
  snapshot <- .read(ss, members_tab, responses_tab, log_tab)
  plan <- unsub_plan(snapshot$members, snapshot$responses, snapshot$log, snapshot$response_id)
  result <- structure(list(ss = ss, members_tab = members_tab, responses_tab = responses_tab,
                           log_tab = log_tab, snapshot = snapshot, plan = plan),
                      class = "unsubscribe_preview")
  print(result)
  invisible(result)
}
print.unsubscribe_preview <- function(x, ...) {
  print(x$plan$preview, row.names = FALSE)
  cat(nrow(x$plan$updates), "member row(s) to change;", nrow(x$plan$records), "request(s) to mark processed.\n")
  cat("Unmatched, invalid, and unconfirmed requests remain pending. Duplicate members are flagged above.\n")
  invisible(x)
}

unsub_cell <- function(value) list(userEnteredValue = list(stringValue = value))
unsub_update_cell <- function(sheet_id, row, column, value) {
  list(updateCells = list(start = list(sheetId = sheet_id, rowIndex = row - 1L,
                                      columnIndex = column - 1L),
                         rows = list(list(values = list(unsub_cell(value)))),
                         fields = "userEnteredValue"))
}
unsub_requests <- function(preview, processed_at) {
  p <- preview$plan
  s <- preview$snapshot
  records <- p$records
  records$processed_at <- processed_at
  requests <- list()
  log_id <- s$log_id
  if (is.na(log_id)) {
    log_id <- max(c(0L, s$sheet_ids)) + 1L
    requests <- list(list(addSheet = list(properties = list(sheetId = log_id,
                                                          title = preview$log_tab))))
    requests <- c(requests, list(list(updateCells = list(
      start = list(sheetId = log_id, rowIndex = 0L, columnIndex = 0L),
      rows = list(list(values = lapply(UNSUB_LOG_COLUMNS, unsub_cell))), fields = "userEnteredValue"))))
  }
  for (i in seq_len(nrow(p$updates))) {
    u <- p$updates[i, ]
    for (column in c("status", "unsubscribed_at")) {
      requests <- c(requests, list(unsub_update_cell(s$member_id, u$row,
                              match(column, names(p$members)), u[[column]])))
    }
  }
  # Fixed row positions make a retry of the same HTTP batch idempotent. Never
  # append blindly: a lost HTTP response must not create duplicate log entries.
  requests <- c(requests, list(list(updateCells = list(
    start = list(sheetId = log_id, rowIndex = nrow(s$log) + 1L, columnIndex = 0L),
    rows = lapply(seq_len(nrow(records)), function(i) list(values = lapply(
      unlist(records[i, UNSUB_LOG_COLUMNS], use.names = FALSE), unsub_cell))),
    fields = "userEnteredValue"))))
  list(requests = requests, records = records)
}
unsub_write <- function(ss, requests) {
  request <- googlesheets4::request_generate("sheets.spreadsheets.batchUpdate",
    params = list(spreadsheetId = as.character(googlesheets4::as_sheets_id(ss)), requests = requests))
  googlesheets4::request_make(request)
  invisible(NULL)
}

verify_unsubscribes <- function(receipt, .read = unsub_read) {
  p <- receipt$preview
  now <- .read(p$ss, p$members_tab, p$responses_tab, p$log_tab)
  # Also validates the fingerprints and ensures none of these requests is pending.
  checked <- unsub_plan(now$members, now$responses, now$log, now$response_id)
  now$members <- checked$members
  rows <- match(receipt$records$request_id, now$log$request_id)
  if (anyNA(rows) || !identical(unsub_frame(now$log[rows, UNSUB_LOG_COLUMNS, drop = FALSE]) |>
                              unname(), unsub_frame(receipt$records) |> unname())) {
    stop("Verification failed: processing log does not match. Do not send mail; inspect the sheet.")
  }
  # Verify even already-unsubscribed and duplicate member rows.
  for (id in receipt$records$request_id) {
    r <- match(id, p$plan$preview$request_id)
    member_rows <- as.integer(strsplit(p$plan$preview$member_rows[r], ",", fixed = TRUE)[[1]]) - 1L
    if (anyNA(member_rows) || any(member_rows > nrow(now$members)) ||
        any(unsub_email(now$members$email[member_rows]) != p$plan$preview$email[r]) ||
        any(unsub_email(now$members$status[member_rows]) != "unsubscribed")) {
      stop("Verification failed: member rows changed or status was not applied. Do not send mail.")
    }
  }
  for (i in seq_len(nrow(p$plan$updates))) {
    u <- p$plan$updates[i, ]
    if (unsub_text(now$members$unsubscribed_at[u$row - 1L]) != u$unsubscribed_at) {
      stop("Verification failed: unsubscribe timestamp differs. Do not send mail.")
    }
  }
  message("Verified: member statuses, timestamps, and processing log match.")
  invisible(TRUE)
}

apply_unsubscribes <- function(preview, .read = unsub_read, .write = unsub_write) {
  stopifnot(inherits(preview, "unsubscribe_preview"))
  now <- .read(preview$ss, preview$members_tab, preview$responses_tab, preview$log_tab)
  if (!identical(now, preview$snapshot)) stop("Preview is stale. Run preview_unsubscribes() again and review it.")
  # Recompute rather than trusting a manually edited preview plan.
  preview$plan <- unsub_plan(now$members, now$responses, now$log, now$response_id)
  if (!nrow(preview$plan$records)) {
    message("No confirmed, matched, unprocessed requests to apply.")
    return(invisible(NULL))
  }
  batch <- unsub_requests(preview, format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  receipt <- list(preview = preview, records = batch$records)
  error <- tryCatch({ .write(preview$ss, batch$requests); NULL }, error = identity)
  # Read back even after an HTTP error: the server may have committed the batch.
  verified <- tryCatch(verify_unsubscribes(receipt, .read = .read), error = identity)
  if (inherits(verified, "error")) {
    stop("Apply could not be verified. Do not send mail. Read a fresh preview before retrying. ",
         if (!is.null(error)) conditionMessage(error) else conditionMessage(verified))
  }
  if (!is.null(error)) message("The write returned an error, but read-back verified it committed successfully.")
  invisible(receipt)
}
