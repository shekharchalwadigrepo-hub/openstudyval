#' Ordered severity levels used by the validator.
#'
#' @return Character vector `Reject`, `Error`, `Warning`, `Note`.
#' @noRd
#' @export
severity_levels <- function() {
  c("Reject", "Error", "Warning", "Note")
}

#' Built-in rule catalog.
#'
#' Rules are original implementations of publicly described expectations from
#' the FDA Study Data Technical Conformance Guide, the eCTD technical-rejection
#' criteria for study data, and structural requirements of SDTM, SEND, and
#' ADaM. Messages are not copied from Pinnacle 21 or from CDISC conformance-rule
#' publications.
#'
#' @param standard Optional filter: `"SDTM"`, `"SEND"`, `"ADaM"`, `"XPT"`, or
#'   `"TRC"`.
#' @return A data frame of rule metadata.
#' @noRd
#' @export
rules_catalog <- function(standard = NULL) {
  path <- system.file("extdata", "rules", "core_rules.csv",
                      package = "openstudyval", mustWork = TRUE)
  rules <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "")
  if (!is.null(standard)) {
    standard <- toupper(standard)
    rules <- rules[toupper(rules$standard) %in% c(standard, "ALL", "XPT", "TRC"), , drop = FALSE]
  }
  rules
}

#' Explain one catalog rule.
#'
#' @param rule_id Rule identifier, such as `"TRC1734"` or `"SDTM010"`.
#' @return A one-row data frame, or an error if the id is unknown.
#' @noRd
#' @export
explain_rule <- function(rule_id) {
  rules <- rules_catalog()
  hit <- rules[rules$rule_id == rule_id, , drop = FALSE]
  if (!nrow(hit)) stop("Unknown rule_id: ", rule_id, call. = FALSE)
  hit
}

#' Append user rules to the catalog.
#'
#' User rules use the same columns as [rules_catalog()]. Supported `check`
#' values are `dataset_required`, `variable_required`, `codelist`,
#' `iso8601`, `key_unique`, `one_to_one`, `nonmissing`, and `regex`.
#'
#' @param rules A data frame of additional rules.
#' @param catalog Optional catalog to extend. Defaults to [rules_catalog()].
#' @return Combined catalog.
#' @noRd
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

osv_load_rules <- function(standard, extra = NULL) {
  rules <- rules_catalog(standard)
  if (!is.null(extra)) rules <- add_custom_rules(extra, rules)
  rules <- rules[toupper(rules$standard) %in% c(toupper(standard), "ALL", "XPT", "TRC"), , drop = FALSE]
  rules
}
