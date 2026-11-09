source('email_management/sync_nimss_members.R')
members <- data.frame(email=c(' ONE@example.org ', 'gone@example.org', 'one@example.org', ''),
                      member=c('FALSE','TRUE','TRUE',''),
                      status=c('unsubscribed','subscribed','subscribed',''))
roster <- data.frame(email=c('one@example.org','new@example.org'), name=c('One','New'))
p <- nimss_plan(members,roster)
stopifnot(identical(p$changes$row,c(2L,3L)), identical(p$changes$member,c(TRUE,FALSE)),
          identical(p$additions$email,'new@example.org'))
# Applying the membership plan converges without changing existing subscription status.
updated <- members
updated$member[p$changes$row-1L] <- as.character(p$changes$member)
updated <- rbind(updated,data.frame(email='new@example.org',member='TRUE',status='subscribed'))
stopifnot(nrow(nimss_plan(updated,roster)$changes)==0L,
          nrow(nimss_plan(updated,roster)$additions)==0L,
          updated$status[1]=='unsubscribed', updated$status[2]=='subscribed')
# Parse only participant mailto links, not unrelated page links; deduplicate case.
page <- tempfile(fileext='.html')
writeLines('<a href="mailto:ignore@example.org">Outside table</a><table><thead><tr><th>Participant Name</th><th>Contact Info</th></tr></thead><tbody><tr><td><a href="/users/1">One</a><a href="mailto:One@Example.org?subject=Hello">Email</a></td></tr><tr><td><a href="/users/2">One again</a><a href="mailto:one@example.org">Email</a></td></tr></tbody></table>',page)
stopifnot(identical(read_nimss_members(page)$email,'one@example.org'))
writeLines('<table><thead><tr><th>Participant Name</th><th>Contact Info</th></tr></thead><tbody><tr><td>No email</td></tr></tbody></table>',page)
stopifnot(inherits(try(read_nimss_members(page),silent=TRUE),'try-error'))
writeLines('<html><body>Please log in</body></html>',page)
stopifnot(inherits(try(read_nimss_members(page),silent=TRUE),'try-error'))
unlink(page)
cat('PASS: additions, departures, case matching, duplicate emails, opt-out preservation, reruns, and malformed sources.\n')
