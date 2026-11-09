# Definitions only: source this file, then sync_member_list(mode = "preview"/"apply").
# Uses the same atomic Sheets writer and unsubscribe ledger as the standalone sync.
.member_sync_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
.member_sync_dir <- if (is.null(.member_sync_file)) "email_management" else dirname(.member_sync_file)
source(file.path(.member_sync_dir, "sync_unsubscribes.R"), local = TRUE)

MEMBER_FORM_TABS <- c(join = "Form Responses 1", update = "Form Responses 2")
MEMBER_FORM_COLUMNS <- list(
  join = c("Timestamp", "Email", "I consent to receive email from the W5133"),
  update = c("Timestamp", "Enter the email address currently associated with your W5133 membership.", "New Email"))
MEMBER_SYNC_COLUMNS <- c("request_id", "fingerprint", "source", "email", "new_email",
                         "submitted_at", "processed_at", "outcome")
member_empty_log <- function() as.data.frame(setNames(rep(list(character()),
  length(MEMBER_SYNC_COLUMNS)), MEMBER_SYNC_COLUMNS), check.names = FALSE)
member_valid_email <- function(x) grepl("^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$", x)
member_fingerprint <- function(row) {
  fields <- unlist(row, use.names = FALSE)
  digest::digest(paste0(nchar(fields, type = "bytes"), ":", fields, collapse = ""),
                 algo = "sha256", serialize = FALSE)
}
# Follow completed address changes so a new unsubscribe for an old address also
# suppresses the replacement address. Ambiguous/cyclic histories stop mailing.
member_resolve_email <- function(email, log) {
  moves <- log[log$source == "update" & log$outcome == "updated" & log$email != log$new_email, ]
  seen <- character()
  while (email %in% moves$email) {
    if (email %in% seen) stop("Cyclic email-change history; reconcile before mailing.")
    seen <- c(seen, email)
    next_email <- unique(moves$new_email[moves$email == email])
    if (length(next_email) != 1L) stop("Ambiguous email-change history; reconcile before mailing.")
    email <- next_email
  }
  email
}
member_sync_read <- function(ss, members_tab = "members") {
  s <- unsub_read(ss, members_tab, "Form Responses 3", "unsubscribe_sync_log")
  meta <- googlesheets4::gs4_get(ss)$sheets
  read <- function(tab) unsub_frame(googlesheets4::read_sheet(ss, sheet = tab,
    col_types = "c", trim_ws = FALSE, .name_repair = "minimal"))
  if (!all(MEMBER_FORM_TABS %in% meta$name)) stop("Join/update response tabs missing.")
  s$forms <- lapply(MEMBER_FORM_TABS, read)
  s$form_ids <- setNames(meta$id[match(MEMBER_FORM_TABS, meta$name)], names(MEMBER_FORM_TABS))
  s$member_log <- if ("member_sync_log" %in% meta$name) read("member_sync_log") else member_empty_log()
  s$member_log_id <- if ("member_sync_log" %in% meta$name) meta$id[match("member_sync_log", meta$name)] else NA_integer_
  s
}
member_sync_plan <- function(snapshot) {
  members <- unsub_frame(snapshot$members)
  names(members) <- tolower(trimws(names(members)))
  unsub_require(members, c("email", "status", "member", "added_at", "unsubscribed_at"), "members")
  log <- unsub_frame(snapshot$member_log)
  unsub_require(log, MEMBER_SYNC_COLUMNS, "Member sync log")
  if (anyDuplicated(log$request_id) || any(!nzchar(log$request_id)) ||
      any(!log$source %in% names(MEMBER_FORM_TABS))) stop("Invalid member sync log.")
  records <- member_empty_log()
  review <- data.frame(source = character(), row = integer(), email = character(),
                       new_email = character(), outcome = character())
  for (source in names(MEMBER_FORM_TABS)) {
    responses <- unsub_frame(snapshot$forms[[source]])
    columns <- MEMBER_FORM_COLUMNS[[source]]
    unsub_require(responses, columns, paste(source, "responses"))
    ids <- if (nrow(responses)) paste(snapshot$form_ids[[source]], seq_len(nrow(responses)) + 1L, sep = ":") else character()
    fingerprints <- vapply(seq_len(nrow(responses)), function(i) member_fingerprint(responses[i, , drop = FALSE]), character(1))
    old <- log[log$source == source, , drop = FALSE]
    matched <- match(old$request_id, ids)
    if (anyNA(matched) || any(old$fingerprint != fingerprints[matched])) stop("Processed join/update history changed; restore before syncing.")
    for (i in seq_len(nrow(responses))) {
      if (ids[i] %in% log$request_id) next
      values <- unlist(responses[i, ], use.names = FALSE)
      if (all(!nzchar(trimws(values)))) next # Preserve blank physical rows in request IDs.
      email <- unsub_email(responses[[columns[2]]][i])
      target <- if (source == "update") unsub_email(responses[[columns[3]]][i]) else email
      # Empty new email is an institution-only update; no mailing-list change.
      if (source == "update" && !nzchar(target)) target <- email
      outcome <- ""
      if (!nzchar(trimws(responses$Timestamp[i])) || !member_valid_email(email) || !member_valid_email(target)) {
        outcome <- "invalid"
      } else if (source == "join" && !tolower(trimws(responses[[columns[3]]][i])) %in% c("yes", "subscribe")) {
        outcome <- "unconfirmed"
      } else {
        history <- rbind(log, records)
        resolved <- member_resolve_email(email, history)
        rows <- which(unsub_email(members$email) == resolved)
        if (source == "join") {
          if (length(rows)) {
            # Never revive an opt-out from an old join submission (or overwrite
            # any other existing member metadata).
            outcome <- "already_listed"
          } else {
            row <- setNames(as.list(rep("", ncol(members))), names(members))
            row$email <- email; row$status <- "subscribed"; row$member <- "FALSE"
            row$added_at <- responses$Timestamp[i]
            members <- rbind(members, as.data.frame(row, check.names = FALSE))
            outcome <- "added"
          }
        } else if (!length(rows)) {
          outcome <- "unmatched"
        } else if (length(rows) != 1L) {
          outcome <- "duplicate_source"
        } else if (target == resolved || target == email) {
          outcome <- "unchanged"
        } else if (resolved != email || target %in% unsub_email(members$email) ||
                   target %in% history$email[history$source == "update"]) {
          outcome <- "address_conflict"
        } else {
          members$email[rows] <- target
          outcome <- "updated"
        }
      }
      review <- rbind(review, data.frame(source = source, row = i + 1L, email = email,
                                         new_email = target, outcome = outcome))
      if (outcome %in% c("added", "already_listed", "updated", "unchanged")) {
        records <- rbind(records, data.frame(request_id = ids[i], fingerprint = fingerprints[i],
          source = source, email = email, new_email = target, submitted_at = responses$Timestamp[i],
          processed_at = "", outcome = outcome))
      }
    }
  }
  history <- rbind(log, records)
  aliases <- unique(history$email[history$source == "update" & history$outcome == "updated"])
  aliases <- setNames(vapply(aliases, member_resolve_email, character(1), log = history), aliases)
  unsub <- unsub_plan(members, snapshot$responses, snapshot$log, snapshot$response_id, aliases = aliases)
  for (i in seq_len(nrow(unsub$updates))) {
    u <- unsub$updates[i, ]
    members$status[u$row - 1L] <- u$status
    members$unsubscribed_at[u$row - 1L] <- u$unsubscribed_at
  }
  blocked <- any(!review$outcome %in% c("added", "already_listed", "updated", "unchanged")) ||
    any(unsub$preview$outcome %in% c("invalid", "unmatched", "unconfirmed"))
  # Duplicate active addresses would otherwise receive duplicate messages.
  active <- unsub_email(members$email[unsub_email(members$status) == "subscribed"])
  if (anyDuplicated(active)) blocked <- TRUE
  list(members = members, review = review, records = records, unsub = unsub, blocked = blocked)
}
preview_member_list <- function(ss = UNSUB_SHEET, members_tab = "members", .read = member_sync_read) {
  snapshot <- .read(ss, members_tab)
  plan <- member_sync_plan(snapshot)
  print(plan$review, row.names = FALSE)
  print(plan$unsub$preview, row.names = FALSE)
  cat(nrow(plan$members) - nrow(snapshot$members), "new member-list rows;",
      nrow(plan$records), "join/update requests;", nrow(plan$unsub$records), "unsubscribe requests.\n")
  if (plan$blocked) message("BLOCKED: resolve pending requests or duplicate subscribed addresses before mailing.")
  invisible(list(ss = ss, members_tab = members_tab, snapshot = snapshot, plan = plan))
}
member_sync_requests <- function(preview, processed_at) {
  s <- preview$snapshot; p <- preview$plan
  requests <- list()
  # Only change individual email/status/timestamp cells; preserve all other cells
  # and formulas. New rows use fixed positions, not appendCells.
  before <- unsub_frame(s$members)
  names(before) <- names(p$members)
  if (nrow(p$members) > nrow(before)) requests <- list(list(appendDimension = list(
    sheetId = s$member_id, dimension = "ROWS", length = nrow(p$members) - nrow(before))))
  for (r in seq_len(nrow(p$members))) for (c in seq_len(ncol(p$members))) {
    value <- p$members[r, c]
    if (r > nrow(before) || value != before[r, c]) {
      request <- unsub_update_cell(s$member_id, r + 1L, c, value)
      if (names(p$members)[c] == "member") {
        request$updateCells$rows[[1]]$values[[1]] <- list(userEnteredValue = list(boolValue = value == "TRUE"))
      }
      requests <- c(requests, list(request))
    }
  }
  logs <- list(list(id = s$member_log_id, title = "member_sync_log", before = s$member_log,
                   records = p$records, columns = MEMBER_SYNC_COLUMNS),
               list(id = s$log_id, title = "unsubscribe_sync_log", before = s$log,
                    records = p$unsub$records, columns = UNSUB_LOG_COLUMNS))
  next_id <- max(c(0L, s$sheet_ids)) + 1L
  for (log in logs) {
    if (!nrow(log$records)) next
    if (is.na(log$id)) {
      log$id <- next_id; next_id <- next_id + 1L
      requests <- c(requests, list(list(addSheet = list(properties = list(sheetId = log$id, title = log$title)))))
      requests <- c(requests, list(list(updateCells = list(start = list(sheetId = log$id, rowIndex = 0L, columnIndex = 0L),
        rows = list(list(values = lapply(log$columns, unsub_cell))), fields = "userEnteredValue"))))
    }
    # Grow existing log grids when necessary; fixed row writes remain retry-safe.
    requests <- c(requests, list(list(appendDimension = list(sheetId = log$id, dimension = "ROWS", length = nrow(log$records)))))
    log$records$processed_at <- processed_at
    requests <- c(requests, list(list(updateCells = list(
      start = list(sheetId = log$id, rowIndex = nrow(log$before) + 1L, columnIndex = 0L),
      rows = lapply(seq_len(nrow(log$records)), function(i) list(values = lapply(
        unlist(log$records[i, log$columns], use.names = FALSE), unsub_cell))), fields = "userEnteredValue"))))
  }
  requests
}
verify_member_list <- function(receipt, .read = member_sync_read) {
  p <- receipt$preview
  now <- .read(p$ss, p$members_tab)
  actual <- unsub_frame(now$members); names(actual) <- tolower(trimws(names(actual)))
  equal <- function(a, b) identical(unname(as.matrix(unsub_frame(a))), unname(as.matrix(unsub_frame(b))))
  if (!identical(names(actual), names(p$plan$members)) || !equal(actual, p$plan$members)) stop("Member verification failed; do not send mail.")
  expected_log <- function(old, records) {
    if (nrow(records)) records$processed_at <- receipt$processed_at
    rbind(old, records)
  }
  if (!equal(now$member_log, expected_log(p$snapshot$member_log, p$plan$records)) ||
      !equal(now$log, expected_log(p$snapshot$log, p$plan$unsub$records))) stop("Processing log verification failed; do not send mail.")
  checked <- member_sync_plan(now) # Also detects history edits/new requests during apply.
  if (checked$blocked || nrow(checked$records) || nrow(checked$unsub$records)) stop("New or unresolved requests; sync again before mailing.")
  message("Verified member list and both processing logs. No pending requests.")
  invisible(TRUE)
}
apply_member_list <- function(preview, .read = member_sync_read, .write = unsub_write) {
  now <- .read(preview$ss, preview$members_tab)
  if (!identical(now, preview$snapshot)) stop("Preview is stale; preview again before applying.")
  preview$plan <- member_sync_plan(now)
  if (preview$plan$blocked) stop("Resolve pending requests/duplicate subscribed addresses before applying or mailing.")
  receipt <- list(preview = preview, processed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  requests <- member_sync_requests(preview, receipt$processed_at)
  error <- if (length(requests)) tryCatch({ .write(preview$ss, requests); NULL }, error = identity) else NULL
  verified <- tryCatch(verify_member_list(receipt, .read), error = identity)
  if (inherits(verified, "error")) stop("Sync could not be verified. Do not send mail. Preview again. ", conditionMessage(verified))
  if (!is.null(error)) message("Write returned an error, but read-back confirmed the batch committed.")
  invisible(receipt)
}
sync_member_list <- function(ss = UNSUB_SHEET, members_tab = "members", mode = c("preview", "apply")) {
  mode <- match.arg(mode)
  preview <- preview_member_list(ss, members_tab)
  if (mode == "preview") return(invisible(preview))
  apply_member_list(preview)
}
