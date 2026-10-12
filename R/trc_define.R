osv_trc <- function(path, datasets, standard, define = NULL) {
  parts <- list()
  standard <- toupper(standard)
  files <- if (!is.null(path) && length(path) == 1L && dir.exists(path)) {
    list.files(path, full.names = FALSE)
  } else {
    character()
  }
  lower <- tolower(files)
  has_ts <- "TS" %in% names(datasets) || any(lower == "ts.xpt")
  has_dm <- "DM" %in% names(datasets) || any(lower == "dm.xpt")
  has_adsl <- "ADSL" %in% names(datasets) || any(lower == "adsl.xpt")
  has_define <- !is.null(define) || any(tolower(files) == "define.xml")
  if (!has_ts && standard %in% c("SDTM", "SEND")) {
    parts[[length(parts) + 1L]] <- osv_issue(
      "TRC1734", "Reject", "TS",
      "A dataset named ts.xpt with study start date must be present for each study (eCTD technical-rejection criterion 1734).",
      publisher = "FDA-TRC"
    )
  }
  if (standard %in% c("SDTM", "SEND") && !has_dm) {
    parts[[length(parts) + 1L]] <- osv_issue(
      "TRC1736", "Reject", "DM",
      "DM and define.xml must be present (eCTD technical-rejection criterion 1736).",
      publisher = "FDA-TRC"
    )
  }
  if (standard == "ADAM" && !has_adsl) {
    parts[[length(parts) + 1L]] <- osv_issue(
      "TRC1736", "Reject", "ADSL",
      "ADSL and define.xml must be present for Module 5 analysis data (eCTD criterion 1736).",
      publisher = "FDA-TRC"
    )
  }
  if (!has_define) {
    parts[[length(parts) + 1L]] <- osv_issue(
      "TRC1736D", "Error", "define.xml",
      "define.xml was not found beside the datasets. FDA expects a define.xml for standardized study data.",
      publisher = "FDA-TRC"
    )
  }
  stf <- files[grepl("\\.xml$", files, ignore.case = TRUE) & grepl("stf", files, ignore.case = TRUE)]
  if (length(stf)) {
    parts[[length(parts) + 1L]] <- osv_issue(
      "TRC1735", "Note", "STF",
      "An STF file is present. Confirm file-tags are data-tabulation-dataset-sdtm, data-tabulation-dataset-send, or analysis-dataset-adam (criterion 1735). This check does not parse the eCTD backbone.",
      publisher = "FDA-TRC"
    )
  }
  if ("TS" %in% names(datasets) && "STUDYID" %in% names(datasets$TS)) {
    ids <- unique(as.character(datasets$TS$STUDYID))
    ids <- ids[nzchar(ids)]
    if (length(ids) > 1L) {
      parts[[length(parts) + 1L]] <- osv_issue(
        "TRC1738", "Error", "TS",
        "More than one STUDYID in TS. Criterion 1738 expects STUDYID to match the study tagging file study id.",
        variable = "STUDYID", value = paste(ids, collapse = "|"),
        publisher = "FDA-TRC"
      )
    }
  }
  osv_bind(parts)
}

osv_define <- function(define, datasets) {
  if (is.null(define) || !file.exists(define)) return(osv_empty_issues())
  if (!requireNamespace("xml2", quietly = TRUE)) {
    return(osv_issue(
      "DEF000", "Note", "define.xml",
      "define.xml was supplied but the suggested package xml2 is not installed, so content checks were skipped.",
      publisher = "openstudyval"
    ))
  }
  doc <- tryCatch(xml2::read_xml(define), error = function(e) e)
  if (inherits(doc, "error")) {
    return(osv_issue(
      "DEF001", "Error", "define.xml",
      paste("define.xml is not well-formed XML:", conditionMessage(doc)),
      publisher = "Define-XML"
    ))
  }
  xml2::xml_ns_strip(doc)
  items <- xml2::xml_find_all(doc, ".//ItemGroupDef")
  names_def <- xml2::xml_attr(items, "Name")
  names_def <- toupper(names_def[!is.na(names_def)])
  parts <- list()
  missing_data <- setdiff(names_def, names(datasets))
  missing_def <- setdiff(names(datasets), names_def)
  if (length(missing_data)) {
    parts[[length(parts) + 1L]] <- osv_issue(
      "DEF002", "Warning", "define.xml",
      "ItemGroupDef names have no matching dataset in the study folder.",
      value = paste(missing_data, collapse = "|"),
      publisher = "Define-XML", count = length(missing_data)
    )
  }
  if (length(missing_def)) {
    parts[[length(parts) + 1L]] <- osv_issue(
      "DEF003", "Error", "define.xml",
      "Datasets are not described in define.xml.",
      value = paste(missing_def, collapse = "|"),
      publisher = "Define-XML", count = length(missing_def)
    )
  }
  if (!length(xml2::xml_find_all(doc, ".//MetaDataVersion"))) {
    parts[[length(parts) + 1L]] <- osv_issue(
      "DEF004", "Error", "define.xml",
      "define.xml has no MetaDataVersion element.",
      publisher = "Define-XML"
    )
  }
  osv_bind(parts)
}
