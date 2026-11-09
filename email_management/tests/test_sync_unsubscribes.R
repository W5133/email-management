# Run from repository root: Rscript email_management/tests/test_sync_unsubscribes.R
source('email_management/sync_unsubscribes.R')
expect_error <- function(expr, pattern) {
  error <- tryCatch({force(expr); NULL}, error = identity)
  stopifnot(inherits(error, 'error'), grepl(pattern, conditionMessage(error)))
}
members <- data.frame(email = c(' One@Example.org ', 'one@example.org', 'two@example.org'),
                      status = c('subscribed', 'subscribed', 'unsubscribed'),
                      unsubscribed_at = c('', '', 'old date'), name = c('One', 'Duplicate', 'Two'))
responses <- data.frame(Timestamp = c('11/8/2026 10:00:00', '11/8/2026 10:01:00',
                                      '11/8/2026 10:02:00', '11/8/2026 10:03:00', '11/8/2026 10:04:00'),
  email = c('one@example.org', 'two@example.org', 'unknown@example.org', 'one@example.org', 'bad'),
  reason = rep('No longer a member', 5), confirm = c(rep(UNSUB_CONFIRMATION, 3), '', UNSUB_CONFIRMATION),
  check.names = FALSE)
names(responses) <- c('Timestamp', 'Email address to unsubscribe', 'Reason for unsubscribing (optional)',
                     'Confirm your unsubscribe request')
state <- list(members=members, responses=responses, log=unsub_empty_log(),
              member_id=0L, response_id=10L, log_id=NA_integer_, sheet_ids=c(0L,10L))
read_fake <- function(...) state
writes <- 0L
write_fake <- function(ss, requests) {
  # Emulate the exact updateCells requests in a temporary copy, then commit all.
  pending <- state
  for (request in requests) {
    if (!is.null(request$addSheet)) {
      pending$log_id <- request$addSheet$properties$sheetId
      pending$sheet_ids <- sort(c(pending$sheet_ids, pending$log_id))
      next
    }
    u <- request$updateCells
    stopifnot(identical(u$fields, 'userEnteredValue'))
    if (u$start$sheetId == pending$member_id) {
      r <- u$start$rowIndex
      c <- u$start$columnIndex + 1L
      stopifnot(names(pending$members)[c] %in% c('status','unsubscribed_at'))
      pending$members[r,c] <- u$rows[[1]]$values[[1]]$userEnteredValue$stringValue
    } else {
      stopifnot(u$start$sheetId == pending$log_id)
      for (i in seq_along(u$rows)) {
        r <- u$start$rowIndex + i - 1L
        values <- vapply(u$rows[[i]]$values, function(x) x$userEnteredValue$stringValue, character(1))
        if (r == 0L) stopifnot(identical(values, UNSUB_LOG_COLUMNS)) else pending$log[r,] <- values
      }
    }
  }
  state <<- pending
  writes <<- writes + 1L
}
p <- preview_unsubscribes(.read=read_fake)
stopifnot(writes==0L, nrow(p$plan$updates)==2L, nrow(p$plan$records)==2L,
          identical(p$plan$preview$outcome,c('unsubscribe','already_unsubscribed','unmatched','unconfirmed','invalid')),
          p$plan$preview$duplicate_member[1])
receipt <- apply_unsubscribes(p, .read=read_fake, .write=write_fake)
stopifnot(writes==1L, all(state$members$status=='unsubscribed'),
          identical(state$members$name,members$name), state$members$unsubscribed_at[3]=='old date')
verify_unsubscribes(receipt,.read=read_fake)
# Repeated use of an old preview must fail, even if all intended changes committed.
expect_error(apply_unsubscribes(p,.read=read_fake,.write=write_fake),'stale')
# A later resubscription is preserved and an old request is not replayed.
state$members$status[1] <- 'subscribed'
p2 <- preview_unsubscribes(.read=read_fake)
stopifnot(nrow(p2$plan$records)==0L)
apply_unsubscribes(p2,.read=read_fake,.write=write_fake)
stopifnot(writes==1L, state$members$status[1]=='subscribed')
# A genuinely new submission from the same address can unsubscribe again.
state$responses <- rbind(state$responses, responses[1,])
state$responses$Timestamp[6] <- '11/9/2026 09:00:00'
p3 <- preview_unsubscribes(.read=read_fake)
receipt3 <- apply_unsubscribes(p3,.read=read_fake,.write=write_fake)
stopifnot(writes==2L, nrow(state$log)==3L, state$members$unsubscribed_at[1]=='11/9/2026 09:00:00')
# Edits and reordered processed response history are refused.
saved <- state
state$responses$Timestamp[1] <- 'changed'
expect_error(preview_unsubscribes(.read=read_fake),'Processed response rows changed')
state <- saved
state$responses <- state$responses[c(2,1,3:6),]
expect_error(preview_unsubscribes(.read=read_fake),'Processed response rows changed')
state <- saved
# New responses between preview and apply invalidate preview.
p4 <- preview_unsubscribes(.read=read_fake)
state$responses <- rbind(state$responses,responses[1,])
expect_error(apply_unsubscribes(p4,.read=read_fake,.write=write_fake),'stale')
# Lost HTTP response after commit is recovered by read-back, without reapplying.
p5 <- preview_unsubscribes(.read=read_fake)
apply_unsubscribes(p5,.read=read_fake,.write=function(...) {write_fake(...); stop('timeout')})
stopifnot(writes==3L, nrow(state$log)==4L)
# Failed write with no commit is detected; no request marked processed.
state$responses <- rbind(state$responses,responses[2,])
p6 <- preview_unsubscribes(.read=read_fake)
expect_error(apply_unsubscribes(p6,.read=read_fake,.write=function(...) stop('offline')),'could not be verified')
stopifnot(nrow(state$log)==4L)
# Empty response sheet, malformed schema, corrupt log.
stopifnot(nrow(unsub_plan(members,responses[0,],response_id=10L)$updates)==0L)
expect_error(unsub_plan(members[,1:2],responses),'unique headers')
expect_error(unsub_plan(state$members,state$responses,rbind(state$log,state$log[1,]),10L),'Invalid processing log')
cat('PASS: preview/apply/verify, duplicates, reruns, resubscription, stale previews, history edits, and failed/uncertain writes.\n')
