# Internal helpers. No exported side effects.

osv_severity_rank <- function(x) {
  ranks <- c(Reject = 1L, Error = 2L, Warning = 3L, Note = 4L)
  out <- ranks[match(x, names(ranks))]
  out[is.na(out)] <- 9L
  out
}

osv_empty_issues <- function() {
  data.frame(
    rule_id = character(),
    publisher = character(),
    severity = character(),
    domain = character(),
    variable = character(),
    record = integer(),
    value = character(),
    message = character(),
    count = integer(),
    stringsAsFactors = FALSE
  )
}

osv_issue <- function(rule_id, severity, domain, message,
                      variable = "", record = NA_integer_, value = "",
                      publisher = "openstudyval", count = 1L) {
  data.frame(
    rule_id = as.character(rule_id),
    publisher = as.character(publisher),
    severity = as.character(severity),
    domain = as.character(domain),
    variable = as.character(variable),
    record = as.integer(record),
    value = as.character(value),
    message = as.character(message),
    count = as.integer(count),
    stringsAsFactors = FALSE
  )
}

osv_bind <- function(parts) {
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) return(osv_empty_issues())
  parts <- lapply(parts, function(x) {
    if (is.null(x) || !nrow(x)) return(NULL)
    x
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) return(osv_empty_issues())
  do.call(rbind, parts)
}

osv_upper <- function(x) {
  toupper(trimws(as.character(x)))
}

osv_has <- function(df, name) {
  name %in% names(df)
}

osv_col <- function(df, name) {
  if (!osv_has(df, name)) return(NULL)
  df[[name]]
}

#' @keywords internal
osv_iso8601 <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  ok_empty <- x == ""
  # Complete or partial ISO 8601 date/datetime used in CDISC --DTC variables.
  pat <- paste0(
    "^[0-9]{4}$|",
    "^[0-9]{4}-[0-9]{2}$|",
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}$|",
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}$|",
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}$|",
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}$|",
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]+$"
  )
  ok_empty | grepl(pat, x)
}

osv_limit <- function(idx, n = 25L) {
  if (length(idx) <= n) idx else idx[seq_len(n)]
}
