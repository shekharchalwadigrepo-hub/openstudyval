#' Validate a study-data folder for an FDA submission package.
#'
#' This is the main entry point. It reads datasets, runs XPORT constraints,
#' standard-specific structural checks, the built-in rule catalog, technical
#' rejection criteria, and an optional define.xml cross-check.
#'
#' The result is not a regulatory certification. FDA and PMDA validation
#' engines, including Pinnacle 21 Enterprise as used by FDA, remain the
#' reference for a live submission. Use this report to find issues before
#' that step.
#'
#' @param path Directory of datasets, or a vector of dataset files.
#' @param standard `"SDTM"`, `"SEND"`, or `"ADaM"`.
#' @param define Optional path to define.xml. If `NULL`, a file named
#'   `define.xml` in `path` is used when present.
#' @param rules Optional data frame of extra rules from [add_custom_rules()].
#' @param ct Optional data frame of controlled terminology. Column names are
#'   variable names; values are permitted terms.
#' @param version_label Character label stored on the report (IG or engine version).
#' @return An `osv_report` object.
#' @export
#' @examples
#' report <- validate_submission(example_study("sdtm"), standard = "SDTM")
#' summary(report)
validate_submission <- function(path,
                                standard = c("SDTM", "SEND", "ADaM"),
                                define = NULL,
                                rules = NULL,
                                ct = NULL,
                                version_label = "openstudyval 0.1.0") {
  standard <- match.arg(standard)
  if (length(path) == 1L && dir.exists(path) && is.null(define)) {
    candidate <- file.path(path, "define.xml")
    if (file.exists(candidate)) define <- candidate
  }
  datasets <- read_study_data(path)
  validate_datasets(datasets, standard = standard, define = define,
                    rules = rules, ct = ct, version_label = version_label,
                    path = if (length(path) == 1L) path else NULL)
}

#' Validate datasets already in memory.
#'
#' @param datasets A named list of data frames, as returned by [read_study_data()].
#' @param standard `"SDTM"`, `"SEND"`, or `"ADaM"`.
#' @param define Optional define.xml path.
#' @param rules Optional extra rules.
#' @param ct Optional controlled terminology.
#' @param version_label Report label.
#' @param path Original folder, used only for technical-rejection file checks.
#' @return An `osv_report` object.
#' @export
validate_datasets <- function(datasets,
                              standard = c("SDTM", "SEND", "ADaM"),
                              define = NULL,
                              rules = NULL,
                              ct = NULL,
                              version_label = "openstudyval 0.1.0",
                              path = NULL) {
  standard <- match.arg(standard)
  stopifnot(is.list(datasets), length(datasets) >= 1L)
  catalog <- osv_load_rules(standard, rules)
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
      version_label = version_label,
      define = define,
      n_rules = nrow(catalog),
      created = Sys.time()
    ),
    class = "osv_report"
  )
}

#' @export
print.osv_report <- function(x, ...) {
  cat("openstudyval report\n")
  cat("  Standard: ", x$standard, "\n", sep = "")
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
