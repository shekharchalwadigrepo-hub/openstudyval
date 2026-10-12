#' Write an HTML and/or CSV validation report.
#'
#' @param report An `osv_report` from [validate_submission()].
#' @param file Path ending in `.html` or `.csv`. If it ends in `.html`, a
#'   sibling `.csv` is also written.
#' @return The HTML or CSV path, invisibly.
#' @export
write_validation_report <- function(report, file) {
  stopifnot(inherits(report, "osv_report"))
  ext <- tolower(tools::file_ext(file))
  if (ext == "csv") {
    utils::write.csv(report$issues, file, row.names = FALSE)
    return(invisible(file))
  }
  if (ext != "html") stop("file must end in .html or .csv", call. = FALSE)
  html <- osv_html(report)
  writeLines(html, file, useBytes = TRUE)
  csv <- sub("\\.html$", ".csv", file, ignore.case = TRUE)
  utils::write.csv(report$issues, csv, row.names = FALSE)
  invisible(file)
}

osv_esc <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&", x, fixed = TRUE)
  x <- gsub("<", "<", x, fixed = TRUE)
  x <- gsub(">", ">", x, fixed = TRUE)
  x
}

osv_html <- function(report) {
  issues <- report$issues
  counts <- if (nrow(issues)) table(factor(issues$severity, levels = severity_levels())) else {
    stats::setNames(integer(4), severity_levels())
  }
  sum_df <- summary(report)
  domain_rows <- paste(sprintf(
    "<tr><td>%s</td><td>%s</td><td>%s</td></tr>",
    osv_esc(report$domains$domain),
    report$domains$records,
    report$domains$variables
  ), collapse = "")
  if (nrow(sum_df)) {
    issue_rows <- paste(apply(sum_df, 1, function(r) {
      sprintf("<tr class='sev-%s'><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
              osv_esc(r[["severity"]]), osv_esc(r[["rule_id"]]), osv_esc(r[["severity"]]),
              osv_esc(r[["domains"]]), osv_esc(r[["records"]]), osv_esc(r[["message"]]))
    }), collapse = "")
  } else {
    issue_rows <- "<tr><td colspan='5'>No issues.</td></tr>"
  }
  detail <- utils::head(issues, 500)
  if (nrow(detail)) {
    detail_rows <- paste(apply(detail, 1, function(r) {
      sprintf("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
              osv_esc(r[["rule_id"]]), osv_esc(r[["severity"]]), osv_esc(r[["domain"]]),
              osv_esc(r[["variable"]]), osv_esc(r[["record"]]), osv_esc(r[["count"]]),
              osv_esc(r[["message"]]))
    }), collapse = "")
  } else {
    detail_rows <- "<tr><td colspan='7'>No issues.</td></tr>"
  }
  sprintf("<!DOCTYPE html>
<html lang='en'><head><meta charset='utf-8'>
<title>openstudyval report</title>
<style>
body { font-family: Georgia, serif; margin: 2rem; color: #1c1917; background: #faf7f2; }
h1 { font-weight: 500; letter-spacing: -0.02em; }
.banner { background: #1c1917; color: #faf7f2; padding: 1.2rem 1.4rem; border-radius: 8px; }
.banner p { margin: 0.2rem 0; }
.cards { display: flex; gap: 0.8rem; margin: 1rem 0; }
.card { background: white; border: 1px solid #e7e5e4; padding: 0.8rem 1rem; min-width: 6rem; border-radius: 6px; }
.card b { display: block; font-size: 1.4rem; }
table { border-collapse: collapse; width: 100%%; background: white; margin-bottom: 1.5rem; }
th, td { border-bottom: 1px solid #e7e5e4; text-align: left; padding: 0.45rem 0.5rem; font-size: 0.92rem; vertical-align: top; }
th { background: #f5f5f4; }
.sev-Reject td:nth-child(2) { color: #9f1239; font-weight: 700; }
.sev-Error td:nth-child(2) { color: #b45309; font-weight: 700; }
.note { font-size: 0.85rem; color: #57534e; }
</style></head><body>
<div class='banner'>
<h1>Study data validation report</h1>
<p>Standard: %s &middot; Engine: %s</p>
<p>Generated: %s</p>
</div>
<p class='note'>Independent open-source checks based on public FDA technical-rejection criteria and structural CDISC expectations. Not a Pinnacle 21 report and not an FDA certification.</p>
<div class='cards'>
<div class='card'><b>%s</b>Reject</div>
<div class='card'><b>%s</b>Error</div>
<div class='card'><b>%s</b>Warning</div>
<div class='card'><b>%s</b>Note</div>
</div>
<h2>Datasets</h2>
<table><thead><tr><th>Domain</th><th>Records</th><th>Variables</th></tr></thead><tbody>%s</tbody></table>
<h2>Issue summary</h2>
<table><thead><tr><th>Rule</th><th>Severity</th><th>Domains</th><th>Records</th><th>Message</th></tr></thead><tbody>%s</tbody></table>
<h2>Details (first 500)</h2>
<table><thead><tr><th>Rule</th><th>Severity</th><th>Domain</th><th>Variable</th><th>Record</th><th>Count</th><th>Message</th></tr></thead><tbody>%s</tbody></table>
</body></html>",
    osv_esc(report$standard), osv_esc(report$version_label),
    osv_esc(format(report$created, "%Y-%m-%d %H:%M:%S %Z")),
    counts[["Reject"]], counts[["Error"]], counts[["Warning"]], counts[["Note"]],
    domain_rows, issue_rows, detail_rows)
}
