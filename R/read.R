#' Read a study-data folder into a named list of data frames.
#'
#' CSV and RDS files are read with base R. SAS transport files (`.xpt`) are
#' read with \pkg{haven} when that package is installed. Dataset names are the
#' upper-case file stem, matching FDA transport naming (`dm.xpt` becomes
#' `DM`).
#'
#' @param path Directory containing study datasets, or a character vector of
#'   file paths.
#' @return A named list of data frames with a `source_file` attribute on each
#'   element and a `path` attribute on the list.
#' @export
#' @examples
#' study <- read_study_data(example_study("sdtm"))
#' names(study)
read_study_data <- function(path) {
  stopifnot(is.character(path), length(path) >= 1L)
  files <- if (length(path) == 1L && dir.exists(path)) {
    list.files(path, full.names = TRUE, recursive = FALSE)
  } else {
    path
  }
  files <- files[file.exists(files)]
  files <- files[!dir.exists(files)]
  ext <- tolower(tools::file_ext(files))
  keep <- ext %in% c("csv", "rds", "xpt", "xport")
  files <- files[keep]
  if (!length(files)) {
    stop("No CSV, RDS, or XPT datasets found in ", paste(path, collapse = ", "))
  }
  out <- list()
  for (f in files) {
    name <- toupper(tools::file_path_sans_ext(basename(f)))
    ext_i <- tolower(tools::file_ext(f))
    df <- if (ext_i == "csv") {
      utils::read.csv(f, stringsAsFactors = FALSE, na.strings = c("", "NA"),
                      check.names = FALSE)
    } else if (ext_i == "rds") {
      readRDS(f)
    } else {
      osv_read_xpt(f)
    }
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    names(df) <- toupper(names(df))
    attr(df, "source_file") <- normalizePath(f, mustWork = FALSE)
    attr(df, "n_rows") <- nrow(df)
    out[[name]] <- df
  }
  attr(out, "path") <- if (length(path) == 1L) normalizePath(path, mustWork = FALSE) else NA_character_
  class(out) <- c("osv_study", "list")
  out
}

osv_read_xpt <- function(path) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop(
      "Reading ", basename(path), " requires the suggested package 'haven'. ",
      "Install it with install.packages(\"haven\"), or supply CSV/RDS datasets.",
      call. = FALSE
    )
  }
  haven::read_xpt(path)
}

#' Path to a bundled example study.
#'
#' The example studies intentionally contain conformance issues so a first
#' validation run produces a non-empty report.
#'
#' @param which `"sdtm"` or `"adam"`.
#' @return Character path to the example directory.
#' @export
example_study <- function(which = c("sdtm", "adam")) {
  which <- match.arg(which)
  system.file("extdata", paste0("example_", which), package = "openstudyval",
              mustWork = TRUE)
}
