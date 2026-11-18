#' Validate a study-data folder for an FDA submission package.
#'
#' Enhanced version with version support, rule filtering, and batch options.
#'
#' @param path Directory of datasets, or a vector of dataset files.
#' @param standard `"SDTM"`, `"SEND"`, or `"ADaM"`.
#' @param standard_version Optional version string (e.g. `"3.3"`, `"1.2"`).
#' @param define Optional path to define.xml.
#' @param rules Optional data frame of extra rules.
#' @param ct Optional controlled terminology.
#' @param rule_ids Optional character vector of rule IDs to run (subset).
#' @param severity Optional character vector of severities to include.
#' @param version_label Character label stored on the report.
#' @return An `osv_report` object.
#' @export
validate_submission <- function(path,
                                standard = c("SDTM", "SEND", "ADaM"),
                                standard_version = NULL,
                                define = NULL,
                                rules = NULL,
                                ct = NULL,
                                rule_ids = NULL,
                                severity = NULL,
                                version_label = "openstudyval 0.2.0") {
  standard <- match.arg(standard)
  if (length(path) == 1L && dir.exists(path) && is.null(define)) {
    candidate <- file.path(path, "define.xml")
    if (file.exists(candidate)) define <- candidate
  }
  datasets <- read_study_data(path)
  validate_datasets(datasets, standard = standard,
                    standard_version = standard_version,
                    define = define, rules = rules, ct = ct,
                    rule_ids = rule_ids, severity = severity,
                    version_label = version_label,
                    path = if (length(path) == 1L) path else NULL)
}

#' Validate datasets already in memory.
#'
#' @export
validate_datasets <- function(datasets,
                              standard = c("SDTM", "SEND", "ADaM"),
                              standard_version = NULL,
                              define = NULL,
                              rules = NULL,
                              ct = NULL,
                              rule_ids = NULL,
                              severity = NULL,
                              version_label = "openstudyval 0.2.0",
                              path = NULL) {
  standard <- match.arg(standard)
  stopifnot(is.list(datasets), length(datasets) >= 1L)
  
  catalog <- osv_load_rules(standard, rules, standard_version)
  
  if (!is.null(rule_ids)) {
    catalog <- catalog[catalog$rule_id %in% rule_ids, , drop = FALSE]
  }
  if (!is.null(severity)) {
    catalog <- catalog[catalog$severity %in% severity, , drop = FALSE]
  }
  
  parts <- list(
    osv_xpt_constraints(datasets),
    osv_structural(datasets, standard),
    osv_trc(path, datasets, standard, define)
  )
  for (i in seq_len(nrow(catalog))) {
    rule <- catalog[i, , drop = FALSE]
    parts[[length(parts) + 1L]] <- tryCatch(
      osv_dispatch(datasets, rule, ct),
      error = function(e) {
        osv_issue(rule$rule_id, "Note", NA_character_,
                  paste("Rule failed to execute:", conditionMessage(e)),
                  publisher = "openstudyval")
      }
    )
  }
  parts[[length(parts) + 1L]] <- osv_define(define, datasets)
  issues <- osv_bind(parts)
  if (nrow(issues)) {
    issues <- issues[order(osv_severity_rank(issues$severity), issues$rule_id, issues$domain), , drop = FALSE]
    rownames(issues) <- NULL
  }
  domain_summary <- data.frame(
    domain = names(datasets),
    records = vapply(datasets, nrow, integer(1)),
    variables = vapply(datasets, ncol, integer(1)),
    stringsAsFactors = FALSE
  )
  structure(
    list(
      issues = issues,
      domains = domain_summary,
      standard = standard,
      standard_version = standard_version,
      version_label = version_label,
      define = define,
      n_rules = nrow(catalog),
      created = Sys.time()
    ),
    class = "osv_report"
  )
}

#' Batch validate multiple study folders.
#'
#' @param paths Character vector of study folder paths.
#' @param ... Arguments passed to `validate_submission()`.
#' @return Named list of `osv_report` objects.
#' @export
validate_studies <- function(paths, ...) {
  names(paths) <- basename(paths)
  lapply(paths, function(p) {
    tryCatch(validate_submission(p, ...), error = function(e) {
      structure(list(error = conditionMessage(e), path = p), class = "osv_error")
    })
  })
}

#' @export
print.osv_report <- function(x, ...) {
  cat("openstudyval report\n")
  cat("  Standard: ", x$standard, if (!is.null(x$standard_version)) paste0(" (", x$standard_version, ")") else "", "\n", sep = "")
  cat("  Engine:   ", x$version_label, "\n", sep = "")
  cat("  Datasets: ", nrow(x$domains), "\n", sep = "")
  cat("  Issues:   ", nrow(x$issues), "\n", sep = "")
  if (nrow(x$issues)) {
    tab <- table(factor(x$issues$severity, levels = severity_levels()))
    cat(paste0("  ", names(tab), ": ", as.integer(tab), collapse = "\n"), "\n")
  }
  invisible(x)
}

#' @export
summary.osv_report <- function(object, ...) {
  if (!nrow(object$issues)) {
    out <- data.frame(rule_id = character(), severity = character(),
                      domains = character(), records = integer(),
                      message = character(), stringsAsFactors = FALSE)
  } else {
    sp <- split(object$issues, object$issues$rule_id)
    out <- do.call(rbind, lapply(sp, function(d) {
      data.frame(
        rule_id = d$rule_id[[1]],
        severity = d$severity[[1]],
        domains = paste(unique(d$domain), collapse = ", "),
        records = sum(d$count),
        message = d$message[[1]],
        stringsAsFactors = FALSE
      )
    }))
    out <- out[order(osv_severity_rank(out$severity), out$rule_id), , drop = FALSE]
    rownames(out) <- NULL
  }
  attr(out, "standard") <- object$standard
  class(out) <- c("summary.osv_report", "data.frame")
  out
}

#' @export
print.summary.osv_report <- function(x, ...) {
  cat("Issue summary (", attr(x, "standard"), ")\n", sep = "")
  if (!nrow(x)) {
    cat("No issues.\n")
  } else {
    print.data.frame(x, row.names = FALSE)
  }
  invisible(x)
}

#' @export
as.data.frame.osv_report <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$issues
}
