#' Load validation settings from a YAML configuration file.
#'
#' @param file Path to a YAML file.
#' @return A list of settings that can be passed to `validate_submission()`.
#' @export
load_config <- function(file) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required. Install with install.packages('yaml')", call. = FALSE)
  }
  cfg <- yaml::read_yaml(file)
  cfg
}

#' Validate a study using settings from a YAML config file.
#'
#' @param path Study folder path.
#' @param config_file Path to YAML configuration.
#' @param ... Additional arguments that override the config.
#' @return An `osv_report` object.
#' @export
validate_from_config <- function(path, config_file, ...) {
  cfg <- load_config(config_file)
  args <- modifyList(cfg, list(...))
  args$path <- path
  do.call(validate_submission, args)
}
