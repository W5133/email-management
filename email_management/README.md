# Sending W5133 emails with R

This folder contains an Outlook mailer, Google Sheets sync workflows, and a separate CSV cleaning utility. The mailer automatically synchronizes join, email-change, and unsubscribe forms before selecting recipients. The CSV cleaner is not needed for each mailing.

## Files

| File | Purpose |
| --- | --- |
| `W5133_email.R` | Reads eligible recipients from Google Sheets and creates Outlook drafts or sends individual messages with personalized Google Form unsubscribe links. |
| `sync_member_list.R` | Combined preview/apply/verify workflow for join, email-change, and unsubscribe requests; automatically invoked by the mailer. |
| `sync_nimss_members.R` | Previews and synchronizes NIMSS membership, appending new addresses with an `added_at` timestamp. |
| `sync_unsubscribes.R` | Defines the preview, apply, and verify workflow for Google Form unsubscribe requests. Sourcing it does not write or send anything. |
| `w5133_email_clean.R` | Cleans and checks local CSV email lists, flags potential problems, and joins a separate member list to produce a CSV with a membership flag. Does not send email or update Google Sheets. |
| `e1_post_meeting.txt` | HTML template for the post-meeting reporting request. |
| `e2_reminder1.txt` | HTML template for the reporting reminder. |
| `e3_2027_meeting_announcement.txt` | HTML template for the 2027 meeting announcement; currently selected by the sending script. |

## Quick start: prepare and send an email

### 1. Open the project and install packages if needed

Open `email-management.Rproj` in RStudio. Run this once if the packages are not already installed:

```r
install.packages(c(
  "googlesheets4", "dplyr", "glue",
  "Microsoft365R", "blastula", "htmltools", "digest"
))
```

Open `email_management/W5133_email.R`.

### 2. Set up a draft test before running

The checked-in script currently has `test_mode <- FALSE` and `send_now <- TRUE`, which sends to real recipients. Change the settings first:

```r
test_mode <- TRUE
test_email <- "jbayham@colostate.edu"
send_now <- FALSE
draft_limit <- 1
```

| Setting | Behavior |
| --- | --- |
| `test_mode <- TRUE` | Uses the first `draft_limit` eligible rows and redirects their To addresses to `test_email`. |
| `test_mode <- FALSE` | Uses all eligible rows and their real addresses. |
| `send_now <- FALSE` | Creates Outlook drafts. |
| `send_now <- TRUE` | Sends messages, with a 10-second pause after each send. |
| `draft_limit` | Limits messages only in test mode, whether drafting or sending. |
| `cc_email` | CC address on every message, including tests; currently `wx133.org@gmail.com`. |

Test mode redirects the To address and prefills the test email address in the unsubscribe form. The CC remains active. Opening the form does not submit an unsubscribe request.

### 3. Edit the subject and template

Set the subject and choose the template near the top of the script:

```r
email_subject <- "W5133 Annual Reporting"
email_template_file <- "e2_reminder1.txt"
```

Edit the email wording in the selected `.txt` file. These files contain HTML, despite their extension. Update dates and the Google Form link for the new mailing, and preserve the `{unsub_url}` placeholder in unsubscribe links.

The script searches for the template in the current working directory and then in `email_management/`, so it works from either the project root or that folder. The `build_email_html()` function reads the template and substitutes the recipient's unsubscribe URL.

### 4. Check the member sheet and recipient filter

The configured [Google Sheet](https://docs.google.com/spreadsheets/d/1JEyDDqVjKpPppmDvRs0W17pux8bbUMBEzPLHNS_rH-8) belongs to the `wx133.org@gmail.com` workflow. The script reads its `members` tab and requires these columns:

| Column | Requirement for inclusion |
| --- | --- |
| `email` | Nonmissing and nonempty after trimming whitespace. |
| `status` | `subscribed`, after trimming whitespace and converting to lowercase. |
| `token` | No longer required or used. Existing values can remain in the sheet. |
| `member` | Used only when `member_filter` is not `"all"`; logical `TRUE` for members or `FALSE` for nonmembers. Missing values are excluded when filtering. |
| `report_submitted` | Used only when `report_filter` is not `"all"`; missing (`NA`) means not submitted. |

Choose the audience in SETTINGS at the top of `W5133_email.R`; no edits to the filtering code are needed:

```r
member_filter <- "members"         # "all", "members", or "nonmembers"
report_filter <- "not_submitted"   # "all", "not_submitted", or "submitted"
```

The example above selects the reporting-reminder audience. The current script selects everyone subscribed (`member_filter <- "all"`, `report_filter <- "all"`). `"submitted"` means any nonmissing value, including `FALSE` or text, matching the existing sheet convention. For an announcement to everyone subscribed, set both filters to `"all"`. For all subscribed members regardless of reporting, use `member_filter <- "members"` and `report_filter <- "all"`. Subscription and email requirements always apply.

The pre-mail sync stops if duplicate subscribed addresses exist. Resolve these in the member list before mailing.

### 5. Run and authenticate

From the project root, run:

```r
source("email_management/W5133_email.R")
```

Or use Source in RStudio with the project root as the working directory.

The script authenticates using:

```r
gs4_auth(email = "wx133.org@gmail.com")
outl <- get_business_outlook()
```

Complete any Google and Microsoft browser sign-in prompts. Use the Microsoft 365 account you intend to send from; the Google account provides Sheet access, not the sending mailbox.

Before opening Outlook, the script previews, applies, and verifies form requests. A failed or unresolved sync stops execution. It then prints a recipient table and immediately proceeds to creating drafts or sending messages. **The table is not an interactive confirmation or a pause.** To stop after inspecting recipients, uncomment this existing line before running:

```r
stop("Dry-run complete. Review recipients above, then comment out this line to create drafts.")
```

Comment it out again when ready to create messages.

### 6. Review the Outlook draft and send one test

With `send_now <- FALSE`, inspect Outlook Drafts for the subject, formatting, dates, form link, To and CC addresses, and unsubscribe URL. Open the unsubscribe link and verify the prefilled test email address; leave the confirmation unchecked and do not submit a request for a real member.

To send one test message:

```r
test_mode <- TRUE
send_now <- TRUE
draft_limit <- 1
```

Run the script again and check the delivered message. This creates a new message; it does not send the earlier draft.

### 7. Send the mailing

After reviewing the content and recipient filter, set:

```r
test_mode <- FALSE
send_now <- TRUE
```

Run the script. It sends one message per eligible row and CCs `cc_email` on each message. To prepare drafts addressed to all eligible recipients instead, use `test_mode <- FALSE` and `send_now <- FALSE`.

There is no persistent sent-message log or automatic resume/deduplication. Results are collected in the R object `draft_results`, but are not saved to a file. Rerunning the script can send duplicate messages, including after a partially completed run. Check Outlook Sent Items before retrying a failed mailing.

## The separate email-cleaning utility

`w5133_email_clean.R` is for preparing local mailing-list data. It is independent of the Google Sheet/Outlook sending workflow.

The script currently changes the working directory to `~/Documents` and uses these files:

| File | Role |
| --- | --- |
| `w5133_emails_2026.csv` | Input address list, with an `email` column or separate local-part and domain columns. |
| `w5133_member_emails.csv` | Separate member list with an `email` column, used to assign membership. |
| `w5133_emails_validated.csv` | Output with normalized addresses, validation results, and review flags. |
| `w5133_emails_cleaned.csv` | Output combining unique cleaned addresses with the member list and a `member` flag. |

Before running, review the working directory and filenames. Existing output files at those paths will be overwritten. Install its packages if needed:

```r
install.packages(c("readr", "dplyr", "stringr", "purrr", "tibble", "curl"))
```

The utility:

1. Extracts and normalizes addresses, or reconstructs them from local-part/domain columns.
2. Checks syntax and flags missing addresses, obvious junk, duplicates, role accounts, and disposable domains.
3. Attempts DNS checks for domain resolution and MX records, and assigns a validation status.
4. Writes the validation CSV.
5. Keeps unique, syntactically valid addresses from the main input, then full-joins the separate member list and writes the cleaned CSV with membership flags.

The cleaned output is **not restricted to addresses classified as `likely_valid`**: the main-input filter checks only presence and syntax, and the full join can add addresses from the separate member list. Review the validation output before using the result. DNS checks do not prove an individual mailbox exists.

The cleaning script does not create subscription statuses or unsubscribe tokens, upload its output to Google Sheets, or send email. Managing those fields in the member sheet is a separate step.

## Troubleshooting

- **Package not found:** Install the packages listed for the script you are using.
- **Google authentication fails:** Confirm access to the configured sheet as `wx133.org@gmail.com` and retry authentication.
- **Outlook authentication fails:** Confirm the intended Microsoft 365 account and that organizational settings allow API access.
- **No eligible recipients:** Check `email`, `status`, `member`, and the current `report_submitted` filter.
- **Missing `report_submitted` or `member` column:** Add the column if filtering on it, or set the corresponding filter to `"all"`.
- **Template file not found:** Run from the project root or `email_management/`, and verify `email_template_file` matches an existing file.
- **More drafts than expected:** `draft_limit` applies only when `test_mode <- TRUE`; each rerun creates additional messages.

## Google Form unsubscribe workflow

The published unsubscribe form is configured by `base_unsub_url` and `unsub_email_entry` in the mailer. Each footer opens the form with the destination email address URL-encoded in `entry.2066026793`. Reasons and confirmation are not prefilled; the recipient must submit the form. The HTML renderer escapes the URL for use in an HTML attribute.

Responses populate **Form Responses 3** in the existing member-list spreadsheet. The mailer invokes `sync_member_list.R` automatically; it includes unsubscribe processing and shares the existing unsubscribe ledger. The standalone workflow below remains available, but use the combined workflow for old-address matching after an email change.

Previously sent emails still contain the old Apps Script token links. On November 8, 2026, the original endpoint was updated in place to version 6 of `member_management`. Its `doGet` handler now ignores tokens and displays a link to the new form without reading or changing member records. People following an old link must enter their email and submit the form. The new R email links prefill the email directly.

## Sync unsubscribe requests before mailing

From the repository root in RStudio:

```r
# One-time installation, only if missing:
# install.packages(c("googlesheets4", "digest"))
source("email_management/sync_unsubscribes.R")
googlesheets4::gs4_auth(email = "wx133.org@gmail.com")

# 1. PREVIEW: reads only. Review this table before proceeding.
unsubscribe_preview <- preview_unsubscribes()
unsubscribe_preview$plan$updates  # proposed status/timestamp changes

# 2. APPLY: explicitly commits the reviewed changes and verifies by re-reading.
unsubscribe_receipt <- apply_unsubscribes(unsubscribe_preview)

# 3. VERIFY: optional repeat of the verification already performed by apply.
if (!is.null(unsubscribe_receipt)) verify_unsubscribes(unsubscribe_receipt)
```

Only proceed to the normal email workflow after verification succeeds and you
have reviewed unmatched/invalid requests. No Outlook drafts or messages are
created by this script. `apply_unsubscribes()` returns `NULL` if there is nothing
to process. If new submissions or sheet edits arrive after preview, apply stops;
make a fresh preview and review it again.

The script reads `members` and `Form Responses 3` in the existing member-list
spreadsheet. `members` must include `email`, `status`, and `unsubscribed_at`.
Missing or ambiguous headers stop the workflow rather than guessing columns.

- Addresses are matched after trimming whitespace and ignoring case.
- Only the exact affirmative confirmation from the form is accepted.
- Every matching member row is updated, with duplicates flagged in the preview.
- `unsubscribed_at` receives the request's original submission timestamp as text,
  preserving its Sheet representation without guessing a timezone.
- Already-unsubscribed members retain their existing timestamp. Their new request
  is still recorded as processed so it cannot override a later resubscription.
- Unmatched, invalid, and unconfirmed requests stay pending; they are not logged
  as processed. Correct or resolve these deliberately before mailing.
- Multiple requests for one email are all logged, but each member row is changed
  at most once in that batch, using the first pending request in response order.

### How processing once is enforced

The first apply creates `unsubscribe_sync_log` in the same spreadsheet. It records
response-sheet ID plus original row number, a SHA-256 fingerprint of the four
response fields, normalized email, submission time, processing time in UTC,
outcome, and matching member row numbers. Member updates and log entries are
submitted together in one atomic Sheets `batchUpdate`: either the entire batch
is accepted or none of it is. Only the status and unsubscribe timestamp cells
are changed on `members`; other columns are preserved.

**Keep the response tab append-only and preserve the processing log.** Do not
sort, delete, insert, or edit previously processed response rows, replace the
response tab, or clear the log. Use filter views to inspect responses. The sync
refuses to continue if a processed response no longer matches its recorded
fingerprint. This intentionally requires restoring/reconciling history rather
than treating moved or edited responses as new unsubscribe requests.

Run only one sync session/operator at a time, and avoid editing the member list
or log during apply. Sheets does not offer a transaction spanning reads and
writes: the stale-preview check detects prior changes, but cannot lock out a
concurrent edit during the brief write window. This is a single-operator workflow,
not a distributed exactly-once service.

After a network error, apply reads the sheet again. If the log and member values
match the intended batch, it reports success. Otherwise it stops: inspect a fresh
preview before retrying and do not send mail until resolved. Fixed log positions
make retries of the same batch safe from duplicate log appends. A fresh run skips
processed IDs even if the member has since intentionally resubscribed; a genuinely
new form submission gets a new ID and can unsubscribe them again.

### Local verification

```sh
Rscript email_management/tests/test_sync_unsubscribes.R
```

Tests use an in-memory Sheets adapter and never authenticate, change live data,
or send email. They cover duplicate members, already-unsubscribed members,
unmatched/unconfirmed/invalid requests, empty responses, repeated runs,
resubscription, edited history, stale previews, atomic batch shape, and writes
that fail before or after committing.

## Refresh membership from NIMSS

`sync_nimss_members.R` reads the [project participant list](https://nimss.org/projects/view/participant_list/18858),
extracts and deduplicates participant email addresses, and compares them with `members`.
This is a separate, manually reviewed membership refresh, not part of the automatic pre-mail form sync:

```r
# If missing: install.packages(c("googlesheets4", "rvest", "xml2"))
source("email_management/sync_nimss_members.R")
googlesheets4::gs4_auth(email = "wx133.org@gmail.com")
nimss_preview <- preview_nimss_members()  # read-only; review additions and departures
apply_nimss_members(nimss_preview)        # write, then verify
```

Existing addresses on NIMSS get `member = TRUE`; addresses absent from NIMSS get
`member = FALSE`. Membership changes do **not** change existing email subscription
statuses, so an opted-out member stays unsubscribed and a former member can still
receive general announcements. Missing addresses are appended with `member = TRUE`,
`status = "subscribed"`, and a name/`added_at` timestamp if those columns exist. Tokens
are not needed. All existing duplicate rows for an address receive the same member flag.
Blank-email rows are left alone.

Apply rechecks both sources against the preview, writes only changed member cells
and new rows in one batch, and verifies the result. Run one sync at a time, with no
concurrent manual edits. A malformed/empty roster or a participant without a usable
email aborts the sync. Review unusually large departures before applying: matching
is by email, so an address change appears as a departure plus a new address. After
an error, inspect the sheet and create a fresh preview rather than blindly retrying.

Run `Rscript email_management/tests/test_sync_nimss_members.R` for local tests.

## Automatic pre-mail form sync

`W5133_email.R` sources `sync_member_list.R` near the beginning and calls it after
Google authentication, before Outlook authentication or recipient selection.
`member_sync_mode <- "apply"` is the default: preview, apply, and verify run in
sequence. Set it to `"preview"` to inspect changes and stop without updating Sheets,
creating drafts, or sending mail. Test mode redirects email delivery only; it does
not make the member sync read-only. Use preview mode for a fully read-only run.

To run only the sync (no Outlook activity):

```r
source("email_management/sync_member_list.R")
googlesheets4::gs4_auth(email = "wx133.org@gmail.com")
p <- preview_member_list()
receipt <- apply_member_list(p)  # includes read-back verification
verify_member_list(receipt)     # optional repeat
# Or run all three stages automatically:
# sync_member_list(mode = "apply")
```

Processing order:

1. **join_org / Form Responses 1:** Accept consent values `Yes` and legacy
   `subscribe`. Append genuinely new addresses as subscribed, with `added_at`
   from the submission and `member = FALSE`. Joining the mailing list does not
   establish formal NIMSS membership. Existing addresses retain all fields,
   including opt-outs; a historical join never resubscribes someone. Deliberate
   resubscriptions of existing opted-out addresses require separate review.
2. **update_info / Form Responses 2:** Match the old address and replace only
   `email`. Preserve membership, subscription status, timestamps, and other cells.
   Institution answers remain in form responses and are not copied to members.
   An empty new email is recorded as an institution-only/no-change request.
   Unknown addresses, duplicate source rows, conflicting destinations, and
   ambiguous address histories stop the workflow for review; it never guesses
   which records to merge.
3. **Unsubscribe / Form Responses 3:** Apply confirmed requests last. Preserve
   rows and set `status = "unsubscribed"`; the mailer excludes them. Completed
   address changes allow requests for an old address to suppress the replacement
   address as well. Unsubscribe takes precedence within the batch.

The member edits and both processing logs are written in a **single atomic Sheets
batch**. `member_sync_log` tracks join/update requests, and `unsubscribe_sync_log`
tracks unsubscribes. Row IDs and fingerprints prevent replay and detect edits to
processed responses. Existing submissions already represented in the member list
are recorded as no-ops on the first run. Blank response rows are skipped without
renumbering following requests. Keep response tabs and logs append-only; do not
sort, delete, or edit processed rows. Extra form columns are included in join/update
fingerprints, so changing the form schema after processing requires reconciliation.

Invalid/unconfirmed/unmatched requests and duplicate subscribed addresses block
apply and mailing. Resolve the underlying issue and preview again. Apply refuses
stale previews, and read-back verification checks the complete resulting member
list and ledgers, including whether new requests arrived during the write. The `member_management` Apps Script project showed no installed triggers when
checked on November 8, 2026. Use one operator at a time and avoid adding competing
legacy form-processing triggers;
Sheets cannot lock out another writer or submissions after the final read.

**NIMSS remains a separate reviewed refresh.** Its roster may still contain an old
institutional email after an address change. The standalone NIMSS importer matches
by email and could re-add that obsolete address, so reconcile it with completed
email changes before applying a NIMSS preview. The automatic pre-mail sync does not
fetch NIMSS or change existing formal membership flags.

Local tests (no live writes or email):

```sh
Rscript email_management/tests/test_sync_member_list.R
Rscript email_management/tests/test_sync_unsubscribes.R
Rscript email_management/tests/test_sync_nimss_members.R
```
