#' Ordered severity levels used by the validator.
#'
#' @return Character vector `Reject`, `Error`, `Warning`, `Note`.
#' @export
severity_levels <- function() {
  c("Reject", "Error", "Warning", "Note")
}

#' Built-in rule catalog.
#'
#' @param standard Optional filter.
#' @param standard_version Optional version filter (placeholder for future versioned catalogs).
#' @return A data frame of rule metadata.
#' @export
rules_catalog <- function(standard = NULL, standard_version = NULL) {
  path <- system.file("extdata", "rules", "core_rules.csv",
                      package = "openstudyval", mustWork = TRUE)
  rules <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "")
  if (!is.null(standard)) {
    standard <- toupper(standard)
    rules <- rules[toupper(rules$standard) %in% c(standard, "ALL", "XPT", "TRC"), , drop = FALSE]
  }
  # Future: filter by version if version column is added to CSV
  rules
}

#' Explain one catalog rule.
#'
#' @param rule_id Rule identifier.
#' @return A one-row data frame.
#' @export
explain_rule <- function(rule_id) {
  rules <- rules_catalog()
  hit <- rules[rules$rule_id == rule_id, , drop = FALSE]
  if (!nrow(hit)) stop("Unknown rule_id: ", rule_id, call. = FALSE)
  hit
}

#' Append user rules to the catalog.
#'
#' @param rules A data frame of additional rules.
#' @param catalog Optional catalog to extend.
#' @return Combined catalog.
#' @export
add_custom_rules <- function(rules, catalog = rules_catalog()) {
  need <- c("rule_id", "publisher", "standard", "domains", "severity",
            "check", "argument", "message", "guidance")
  missing <- setdiff(need, names(rules))
  if (length(missing)) {
    stop("Custom rules are missing columns: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  rbind(catalog, rules[, need])
}

osv_load_rules <- function(standard, extra = NULL, standard_version = NULL) {
  rules <- rules_catalog(standard, standard_version)
  if (!is.null(extra)) rules <- add_custom_rules(extra, rules)
  rules <- rules[toupper(rules$standard) %in% c(toupper(standard), "ALL", "XPT", "TRC"), , drop = FALSE]
  rules
}
