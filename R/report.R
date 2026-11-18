#' Write validation report in HTML, CSV, or Excel format.
#'
#' @param report An `osv_report` from [validate_submission()].
#' @param file Output path. Extension determines format (`.html`, `.csv`, `.xlsx`).
#' @param format Optional override: `"html"`, `"csv"`, `"excel"`, or `"both"`.
#' @return The path(s) written, invisibly.
#' @export
write_validation_report <- function(report, file, format = NULL) {
  stopifnot(inherits(report, "osv_report"))
  ext <- tolower(tools::file_ext(file))
  
  if (is.null(format)) {
    format <- switch(ext,
                     html = "html",
                     csv = "csv",
                     xlsx = "excel",
                     "html")
  }
  
  written <- character()
  
  if (format %in% c("html", "both")) {
    html_file <- if (ext == "html") file else sub("\\.[^.]+$", ".html", file)
    html <- osv_html_enhanced(report)
    writeLines(html, html_file, useBytes = TRUE)
    written <- c(written, html_file)
  }
  
  if (format %in% c("csv", "both")) {
    csv_file <- if (ext == "csv") file else sub("\\.[^.]+$", ".csv", file)
    utils::write.csv(report$issues, csv_file, row.names = FALSE)
    written <- c(written, csv_file)
  }
  
  if (format %in% c("excel", "both")) {
    if (!requireNamespace("writexl", quietly = TRUE)) {
      stop("Package 'writexl' is required for Excel export. Install with install.packages('writexl')", call. = FALSE)
    }
    xlsx_file <- if (ext == "xlsx") file else sub("\\.[^.]+$", ".xlsx", file)
    sheets <- list(
      Summary = summary(report),
      Details = report$issues,
      Domains = report$domains
    )
    writexl::write_xlsx(sheets, xlsx_file)
    written <- c(written, xlsx_file)
  }
  
  invisible(written)
}

osv_esc <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&", x, fixed = TRUE)
  x <- gsub("<", "<", x, fixed = TRUE)
  x <- gsub(">", ">", x, fixed = TRUE)
  x
}

osv_html_enhanced <- function(report) {
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
  detail <- utils::head(issues, 1000)
  if (nrow(detail)) {
    detail_rows <- paste(apply(detail, 1, function(r) {
      sprintf("<tr class='sev-%s'><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
              osv_esc(r[["severity"]]),
              osv_esc(r[["rule_id"]]), osv_esc(r[["severity"]]), osv_esc(r[["domain"]]),
              osv_esc(r[["variable"]]), osv_esc(r[["record"]]), osv_esc(r[["count"]]),
              osv_esc(r[["message"]]))
    }), collapse = "")
  } else {
    detail_rows <- "<tr><td colspan='7'>No issues.</td></tr>"
  }
  
  ver_note <- if (!is.null(report$standard_version)) paste0(" &middot; Version: ", report$standard_version) else ""
  
  sprintf("<!DOCTYPE html>
<html lang='en'><head><meta charset='utf-8'>
<title>openstudyval report</title>
<style>
body { font-family: system-ui, -apple-system, sans-serif; margin: 2rem; color: #1c1917; background: #faf7f2; }
h1 { font-weight: 600; letter-spacing: -0.02em; }
.banner { background: #1c1917; color: #faf7f2; padding: 1.2rem 1.4rem; border-radius: 8px; }
.banner p { margin: 0.2rem 0; }
.cards { display: flex; gap: 0.8rem; margin: 1rem 0; flex-wrap: wrap; }
.card { background: white; border: 1px solid #e7e5e4; padding: 0.8rem 1rem; min-width: 6rem; border-radius: 6px; }
.card b { display: block; font-size: 1.4rem; }
table { border-collapse: collapse; width: 100%%; background: white; margin-bottom: 1.5rem; }
th, td { border-bottom: 1px solid #e7e5e4; text-align: left; padding: 0.45rem 0.5rem; font-size: 0.92rem; vertical-align: top; }
th { background: #f5f5f4; position: sticky; top: 0; }
.sev-Reject td:nth-child(2), .sev-Reject td:nth-child(1) { color: #9f1239; font-weight: 700; }
.sev-Error td:nth-child(2), .sev-Error td:nth-child(1) { color: #b45309; font-weight: 700; }
.note { font-size: 0.85rem; color: #57534e; }
</style></head><body>
<div class='banner'>
<h1>Study data validation report</h1>
<p>Standard: %s%s &middot; Engine: %s</p>
<p>Generated: %s &middot; Rules executed: %s</p>
</div>
<p class='note'>Independent open-source checks based on public FDA technical-rejection criteria and structural CDISC expectations. Not a Pinnacle 21 report and not an FDA or PMDA certification.</p>
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
<h2>Details (first 1000)</h2>
<table><thead><tr><th>Rule</th><th>Severity</th><th>Domain</th><th>Variable</th><th>Record</th><th>Count</th><th>Message</th></tr></thead><tbody>%s</tbody></table>
</body></html>",
    osv_esc(report$standard), ver_note,
    osv_esc(report$version_label),
    osv_esc(format(report$created, "%Y-%m-%d %H:%M:%S %Z")),
    report$n_rules,
    counts[["Reject"]], counts[["Error"]], counts[["Warning"]], counts[["Note"]],
    domain_rows, issue_rows, detail_rows)
}
