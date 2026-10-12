osv_xpt_constraints <- function(datasets) {
  parts <- list()
  for (dom in names(datasets)) {
    if (nchar(dom) > 8L || !grepl("^[A-Z][A-Z0-9]*$", dom)) {
      parts[[length(parts) + 1L]] <- osv_issue(
        "XPT001", "Error", dom,
        "Dataset name must be 1-8 characters, start with a letter, and use only letters and digits (SAS v5 transport / FDA TCG).",
        variable = dom, publisher = "FDA-TCG"
      )
    }
    df <- datasets[[dom]]
    if (!nrow(df)) {
      parts[[length(parts) + 1L]] <- osv_issue(
        "XPT006", "Warning", dom,
        "Dataset has zero observations.",
        publisher = "openstudyval"
      )
    }
    for (v in names(df)) {
      if (nchar(v) > 8L || !grepl("^[A-Z][A-Z0-9]*$", v)) {
        parts[[length(parts) + 1L]] <- osv_issue(
          "XPT002", "Error", dom,
          "Variable name must be 1-8 characters, start with a letter, and use only letters and digits.",
          variable = v, publisher = "FDA-TCG"
        )
      }
      label <- attr(df[[v]], "label")
      if (!is.null(label) && nchar(label) > 40L) {
        parts[[length(parts) + 1L]] <- osv_issue(
          "XPT003", "Error", dom,
          "Variable label longer than 40 characters.",
          variable = v, value = label, publisher = "FDA-TCG"
        )
      }
      if (is.character(df[[v]]) || is.factor(df[[v]])) {
        lens <- nchar(as.character(df[[v]]))
        lens[is.na(lens)] <- 0L
        if (any(lens > 200L)) {
          parts[[length(parts) + 1L]] <- osv_issue(
            "XPT004", "Error", dom,
            "Character value longer than 200 bytes. Split across SUPP-- or a second variable.",
            variable = v, publisher = "FDA-TCG",
            count = sum(lens > 200L, na.rm = TRUE)
          )
        }
      }
    }
    src <- attr(df, "source_file")
    if (!is.null(src) && grepl("\\.xpt$", src, ignore.case = TRUE)) {
      info <- file.info(src)
      if (!is.na(info$size) && info$size > 5 * 1024^3) {
        parts[[length(parts) + 1L]] <- osv_issue(
          "XPT005", "Warning", dom,
          "Transport file exceeds 5 GB. FDA TCG recommends splitting large datasets.",
          publisher = "FDA-TCG", value = as.character(info$size)
        )
      }
    }
  }
  osv_bind(parts)
}

osv_structural <- function(datasets, standard) {
  standard <- toupper(standard)
  parts <- list()
  if (standard %in% c("SDTM", "SEND")) {
    parts[[length(parts) + 1L]] <- osv_sdtm_structure(datasets, standard)
  }
  if (standard == "ADAM") {
    parts[[length(parts) + 1L]] <- osv_adam_structure(datasets)
  }
  osv_bind(parts)
}

osv_sdtm_structure <- function(datasets, standard) {
  parts <- list()
  if (!"DM" %in% names(datasets)) {
    parts[[length(parts) + 1L]] <- osv_issue(
      "SDTM001", "Error", "DM",
      "Demographics dataset DM is required for an SDTM or SEND tabulation package.",
      publisher = "FDA-TCG"
    )
  }
  trial <- c("TA", "TE", "TI", "TS", "TV")
  miss_trial <- setdiff(trial, names(datasets))
  if (length(miss_trial)) {
    parts[[length(parts) + 1L]] <- osv_issue(
      "SDTM002", "Warning", paste(miss_trial, collapse = ","),
      "Trial Design dataset missing. FDA TCG expects TA, TE, TI, TS, and TV when applicable.",
      publisher = "FDA-TCG", count = length(miss_trial)
    )
  }
  if ("TS" %in% names(datasets)) {
    ts <- datasets$TS
    if (all(c("TSPARMCD", "TSVAL") %in% names(ts))) {
      codes <- osv_upper(ts$TSPARMCD)
      if (!"SSTDTC" %in% codes) {
        parts[[length(parts) + 1L]] <- osv_issue(
          "SDTM003", "Reject", "TS",
          "TS does not contain TSPARMCD = SSTDTC. FDA technical-rejection criterion 1734 requires a study start date.",
          publisher = "FDA-TRC", variable = "TSPARMCD"
        )
      } else {
        val <- ts$TSVAL[match("SSTDTC", codes)]
        nf <- if ("TSVALNF" %in% names(ts)) ts$TSVALNF[match("SSTDTC", codes)] else NA
        if ((is.na(val) || !nzchar(val)) && (is.na(nf) || !nzchar(nf))) {
          parts[[length(parts) + 1L]] <- osv_issue(
            "SDTM004", "Reject", "TS",
            "SSTDTC is present but both TSVAL and TSVALNF are empty.",
            publisher = "FDA-TRC", variable = "TSVAL"
          )
        }
      }
    }
  } else {
    parts[[length(parts) + 1L]] <- osv_issue(
      "TRC1734", "Reject", "TS",
      "ts.xpt is missing. A Trial Summary dataset with study start date is required (eCTD rule 1734).",
      publisher = "FDA-TRC"
    )
  }
  dm_ids <- NULL
  if ("DM" %in% names(datasets) && "USUBJID" %in% names(datasets$DM)) {
    dm_ids <- unique(as.character(datasets$DM$USUBJID))
  }
  for (dom in names(datasets)) {
    df <- datasets[[dom]]
    if (osv_has(df, "DOMAIN")) {
      bad <- which(osv_upper(df$DOMAIN) != dom & !startsWith(dom, "SUPP"))
      if (length(bad) && !startsWith(dom, "SUPP")) {
        parts[[length(parts) + 1L]] <- osv_issue(
          "SDTM010", "Error", dom,
          "DOMAIN value does not match the dataset name.",
          variable = "DOMAIN", record = bad[[1]],
          value = paste(unique(df$DOMAIN[bad]), collapse = "|"),
          publisher = "CDISC-SDTM", count = length(bad)
        )
      }
    }
    if (osv_has(df, "STUDYID") && osv_has(df, "USUBJID")) {
      sid <- as.character(df$STUDYID)
      uid <- as.character(df$USUBJID)
      bad <- which(nzchar(uid) & nzchar(sid) & !startsWith(uid, sid))
      if (length(bad)) {
        parts[[length(parts) + 1L]] <- osv_issue(
          "SDTM011", "Error", dom,
          "USUBJID should begin with STUDYID.",
          variable = "USUBJID", record = bad[[1]],
          value = paste(unique(uid[osv_limit(bad)]), collapse = "|"),
          publisher = "CDISC-SDTM", count = length(bad)
        )
      }
    }
    if (!is.null(dm_ids) && dom != "DM" && osv_has(df, "USUBJID") && !startsWith(dom, "SUPP") &&
        !dom %in% c("TA", "TE", "TI", "TS", "TV", "TD")) {
      extra <- setdiff(unique(as.character(df$USUBJID)), dm_ids)
      extra <- extra[nzchar(extra)]
      if (length(extra)) {
        parts[[length(parts) + 1L]] <- osv_issue(
          "SDTM012", "Error", dom,
          "USUBJID is not present in DM.",
          variable = "USUBJID", value = paste(utils::head(extra, 10), collapse = "|"),
          publisher = "CDISC-SDTM", count = length(extra)
        )
      }
    }
    seq_var <- paste0(substr(dom, 1, 2), "SEQ")
    if (osv_has(df, seq_var) && osv_has(df, "USUBJID")) {
      key <- interaction(df$USUBJID, df[[seq_var]], drop = TRUE)
      if (any(duplicated(key))) {
        parts[[length(parts) + 1L]] <- osv_issue(
          "SDTM013", "Error", dom,
          paste(seq_var, "is not unique within USUBJID."),
          variable = seq_var, publisher = "CDISC-SDTM",
          count = sum(duplicated(key))
        )
      }
    }
  }
  if ("EX" %in% names(datasets) && "DM" %in% names(datasets)) {
    parts[[length(parts) + 1L]] <- osv_exposure_window(datasets$EX, datasets$DM)
  }
  osv_bind(parts)
}

osv_exposure_window <- function(ex, dm) {
  if (!all(c("USUBJID", "RFSTDTC", "RFENDTC") %in% names(dm))) return(NULL)
  dtc <- intersect(c("EXSTDTC", "EXENDTC"), names(ex))
  if (!length(dtc) || !"USUBJID" %in% names(ex)) return(NULL)
  dm_key <- unique(dm[c("USUBJID", "RFSTDTC", "RFENDTC")])
  merged <- merge(ex, dm_key, by = "USUBJID", all.x = TRUE)
  bad <- 0L
  for (v in dtc) {
    start <- substr(as.character(merged[[v]]), 1, 10)
    rfstd <- substr(as.character(merged$RFSTDTC), 1, 10)
    rfend <- substr(as.character(merged$RFENDTC), 1, 10)
    bad <- bad + sum(nzchar(start) & nzchar(rfstd) & nzchar(rfend) &
                       (start < rfstd | start > rfend), na.rm = TRUE)
  }
  if (!bad) return(NULL)
  osv_issue(
    "FDAB008", "Warning", "EX",
    "Exposure date/time falls outside the DM reference start/end window (public FDA business-rule intent FDAB008).",
    publisher = "FDA-BUSINESS", count = bad
  )
}

osv_adam_structure <- function(datasets) {
  parts <- list()
  if (!"ADSL" %in% names(datasets)) {
    parts[[length(parts) + 1L]] <- osv_issue(
      "ADAM001", "Reject", "ADSL",
      "ADSL is required for an ADaM analysis package (eCTD rule 1736 for Module 5).",
      publisher = "FDA-TRC"
    )
    return(osv_bind(parts))
  }
  adsl <- datasets$ADSL
  if ("USUBJID" %in% names(adsl) && any(duplicated(adsl$USUBJID))) {
    parts[[length(parts) + 1L]] <- osv_issue(
      "ADAM002", "Error", "ADSL",
      "ADSL must contain one record per USUBJID.",
      variable = "USUBJID", publisher = "CDISC-ADAM",
      count = sum(duplicated(adsl$USUBJID))
    )
  }
  ids <- if ("USUBJID" %in% names(adsl)) unique(as.character(adsl$USUBJID)) else character()
  for (dom in setdiff(names(datasets), "ADSL")) {
    df <- datasets[[dom]]
    if ("USUBJID" %in% names(df)) {
      extra <- setdiff(unique(as.character(df$USUBJID)), ids)
      extra <- extra[nzchar(extra)]
      if (length(extra)) {
        parts[[length(parts) + 1L]] <- osv_issue(
          "ADAM003", "Error", dom,
          "USUBJID is not present in ADSL.",
          variable = "USUBJID",
          value = paste(utils::head(extra, 10), collapse = "|"),
          publisher = "CDISC-ADAM", count = length(extra)
        )
      }
    }
  }
  if (all(c("TRTSDT", "TRTEDT") %in% names(adsl))) {
    s <- as.character(adsl$TRTSDT)
    e <- as.character(adsl$TRTEDT)
    bad <- which(nzchar(s) & nzchar(e) & s > e)
    if (length(bad)) {
      parts[[length(parts) + 1L]] <- osv_issue(
        "ADAM004", "Error", "ADSL",
        "TRTSDT is after TRTEDT.",
        variable = "TRTSDT", record = bad[[1]],
        publisher = "CDISC-ADAM", count = length(bad)
      )
    }
  }
  bds <- datasets[grepl("^AD", names(datasets)) & names(datasets) != "ADSL"]
  for (dom in names(bds)) {
    df <- bds[[dom]]
    if (all(c("PARAM", "PARAMCD") %in% names(df))) {
      sub <- unique(df[c("PARAMCD", "PARAM")])
      n <- tapply(sub$PARAM, sub$PARAMCD, function(z) length(unique(z)))
      if (any(n > 1)) {
        parts[[length(parts) + 1L]] <- osv_issue(
          "ADAM005", "Error", dom,
          "PARAMCD maps to more than one PARAM.",
          variable = "PARAMCD",
          value = paste(names(n)[n > 1], collapse = "|"),
          publisher = "CDISC-ADAM", count = sum(n > 1)
        )
      }
    }
  }
  osv_bind(parts)
}
