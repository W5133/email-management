source('email_management/sync_member_list.R')
expect_error <- function(expr, pattern) {
  error <- tryCatch({force(expr); NULL}, error = identity)
  stopifnot(inherits(error, 'error'), grepl(pattern, conditionMessage(error)))
}
form <- function(kind, ...) {
  x <- data.frame(..., check.names = FALSE, stringsAsFactors = FALSE)
  names(x) <- MEMBER_FORM_COLUMNS[[kind]]
  x
}
state <- list(
  members = data.frame(email = c('old@example.org','optout@example.org'),
    member = c('TRUE','FALSE'), status = c('subscribed','unsubscribed'),
    added_at = c('original','original'), unsubscribed_at = c('','earlier'), token = c('keep1','keep2')),
  responses = data.frame(Timestamp = character(), email = character(), reason = character(), confirm = character()),
  log = unsub_empty_log(), member_id = 0L, response_id = 3L, log_id = NA_integer_, sheet_ids = 0:3,
  forms = list(join = form('join', c('t1','t2','','t3'),
    c('New@Example.org','optout@example.org','','new@example.org'), c('Yes','subscribe','','Yes')),
    update = form('update', 't4', 'old@example.org', 'replacement@example.org')),
  form_ids = c(join = 1L, update = 2L), member_log = member_empty_log(), member_log_id = NA_integer_)
names(state$responses) <- c('Timestamp','Email address to unsubscribe',
  'Reason for unsubscribing (optional)','Confirm your unsubscribe request')
state$responses[1,] <- c('t5','old@example.org','Other',UNSUB_CONFIRMATION)
read_fake <- function(...) state
writes <- 0L
write_fake <- function(ss, requests) {
  pending <- state
  for (request in requests) {
    if (!is.null(request$appendDimension)) next
    if (!is.null(request$addSheet)) {
      props <- request$addSheet$properties
      field <- if (props$title == 'member_sync_log') 'member_log_id' else 'log_id'
      pending[[field]] <- props$sheetId
      pending$sheet_ids <- sort(c(pending$sheet_ids, props$sheetId))
      next
    }
    u <- request$updateCells
    field <- if (u$start$sheetId == pending$member_id) 'members' else if (
      u$start$sheetId == pending$member_log_id) 'member_log' else 'log'
    for (i in seq_along(u$rows)) {
      r <- u$start$rowIndex + i - 1L
      values <- vapply(u$rows[[i]]$values, function(x) {
        v <- x$userEnteredValue
        if (!is.null(v$boolValue)) as.character(v$boolValue) else v$stringValue
      }, character(1))
      if (r == 0L) {stopifnot(identical(values, names(pending[[field]]))); next}
      if (r > nrow(pending[[field]])) pending[[field]][r,] <- ''
      cols <- seq_along(values) + u$start$columnIndex
      pending[[field]][r,cols] <- values
    }
  }
  state <<- pending
  writes <<- writes + 1L
}
p <- preview_member_list(.read = read_fake)
stopifnot(!p$plan$blocked, nrow(p$plan$members) == 3,
  p$plan$members$email[1] == 'replacement@example.org',
  p$plan$members$status[1] == 'unsubscribed', p$plan$members$status[2] == 'unsubscribed',
  p$plan$members$member[3] == 'FALSE', p$plan$members$added_at[3] == 't1',
  identical(p$plan$members$token[1:2], state$members$token), writes == 0)
receipt <- apply_member_list(p, .read = read_fake, .write = write_fake)
stopifnot(writes == 1L, nrow(state$member_log) == 4, nrow(state$log) == 1)
p2 <- preview_member_list(.read = read_fake)
apply_member_list(p2, .read = read_fake, .write = write_fake)
stopifnot(writes == 1L, nrow(p2$plan$records) == 0)
expect_error(apply_member_list(p, .read = read_fake, .write = write_fake), 'stale')
# Do not replay an old opt-out after a deliberate resubscription.
state$members$status[1] <- 'subscribed'
stopifnot(member_sync_plan(state)$members$status[1] == 'subscribed')
# A new unsubscribe of the old address resolves through persisted history.
state$responses[2,] <- c('t6','old@example.org','Other',UNSUB_CONFIRMATION)
p3 <- preview_member_list(.read = read_fake)
apply_member_list(p3, .read = read_fake, .write = function(...) {write_fake(...); stop('timeout')})
stopifnot(writes == 2L, state$members$status[1] == 'unsubscribed')
# New email carries existing status, membership, and timestamps through a chain.
saved <- state
state$forms$update[2,] <- c('t7','replacement@example.org','third@example.org')
p4 <- preview_member_list(.read = read_fake)
stopifnot(p4$plan$members$email[1] == 'third@example.org',
  p4$plan$members$status[1] == 'unsubscribed', p4$plan$members$member[1] == 'TRUE')
expect_error(apply_member_list(p4, .read = read_fake, .write = function(...) stop('offline')), 'could not be verified')
stopifnot(writes == 2L)
# Reject ambiguous merges, unmatched requests, invalid consent, and history edits.
state$forms$update[2,] <- c('t7','replacement@example.org','new@example.org')
stopifnot(member_sync_plan(state)$blocked)
state$forms$update[2,] <- c('t7','missing@example.org','else@example.org')
stopifnot(member_sync_plan(state)$blocked)
state <- saved
state$forms$join[5,] <- c('t8','another@example.org','No')
stopifnot(member_sync_plan(state)$blocked)
state <- saved
state$forms$join[1,1] <- 'edited'
expect_error(member_sync_plan(state), 'history changed')
state <- saved
state$forms$update[2,] <- c('t9','replacement@example.org','')
p5 <- preview_member_list(.read = read_fake)
stopifnot(!p5$plan$blocked, tail(p5$plan$review$outcome, 1) == 'unchanged')
# A submission arriving after preview stops apply, before writes.
state$forms$join[5,] <- c('t10','another@example.org','Yes')
expect_error(apply_member_list(p5, .read = read_fake, .write = write_fake), 'stale')
state <- saved
state$members <- rbind(state$members, state$members[3,])
stopifnot(member_sync_plan(state)$blocked)
# Empty histories and institution-only updates are valid.
state <- saved
state$forms <- lapply(state$forms, function(x) x[0,])
state$member_log <- member_empty_log(); state$log <- unsub_empty_log()
state$responses <- state$responses[0,]
stopifnot(!member_sync_plan(state)$blocked)
cat('PASS: combined join/update/unsubscribe sync, atomic writes, aliases, reruns, conflicts, stale previews and failed writes.\n')
# Blocked previews cannot write even if apply is called directly.
state <- saved
state$forms$update[2,] <- c('t11','missing@example.org','else@example.org')
blocked <- preview_member_list(.read = read_fake)
expect_error(apply_member_list(blocked, .read = read_fake, .write = write_fake), 'Resolve pending')
stopifnot(writes == 2L)
# Read-back rejects unrelated changes, not just changed email/status cells.
state <- saved
p6 <- preview_member_list(.read = read_fake)
r6 <- list(preview = p6, processed_at = 'test')
state$members$token[1] <- 'unexpected concurrent edit'
expect_error(verify_member_list(r6, .read = read_fake), 'Member verification failed')
# A new submission arriving after the batch prevents mailing until another sync.
state <- saved
p7 <- preview_member_list(.read = read_fake)
r7 <- list(preview = p7, processed_at = 'test')
state$forms$join[5,] <- c('t12','newer@example.org','Yes')
expect_error(verify_member_list(r7, .read = read_fake), 'New or unresolved')
cat('PASS: fail-closed apply, full-member verification and requests arriving during apply.\n')
