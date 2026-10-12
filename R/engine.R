osv_domain_names <- function(datasets) {
  names(datasets)
}

osv_match_domains <- function(spec, available) {
  if (is.na(spec) || spec == "" || spec == "*") return(available)
  parts <- strsplit(spec, "[|;,]")[[1]]
  parts <- trimws(parts)
  out <- character()
  for (p in parts) {
    if (grepl("\\*$", p)) {
      prefix <- sub("\\*$", "", p)
      out <- c(out, available[startsWith(available, prefix)])
    } else {
      out <- c(out, intersect(p, available))
    }
  }
  unique(out)
}

osv_check_dataset_required <- function(datasets, rule) {
  needed <- strsplit(rule$argument, "[|;,]")[[1]]
  needed <- trimws(needed)
  missing <- needed[!needed %in% names(datasets)]
  if (!length(missing)) return(NULL)
  osv_issue(
    rule$rule_id, rule$severity, paste(missing, collapse = ","),
    rule$message, variable = paste(missing, collapse = ","),
    publisher = rule$publisher, count = length(missing)
  )
}

osv_check_variable_required <- function(datasets, rule) {
  domains <- osv_match_domains(rule$domains, names(datasets))
  vars <- trimws(strsplit(rule$argument, "[|;,]")[[1]])
  parts <- list()
  for (dom in domains) {
    df <- datasets[[dom]]
    miss <- vars[!vars %in% names(df)]
    if (length(miss)) {
      parts[[length(parts) + 1L]] <- osv_issue(
        rule$rule_id, rule$severity, dom, rule$message,
        variable = paste(miss, collapse = ","),
        publisher = rule$publisher, count = length(miss)
      )
    }
  }
  osv_bind(parts)
}

osv_check_codelist <- function(datasets, rule, ct = NULL) {
  domains <- osv_match_domains(rule$domains, names(datasets))
  bits <- strsplit(rule$argument, "=", fixed = TRUE)[[1]]
  var <- trimws(bits[[1]])
  allowed <- if (!is.null(ct) && var %in% names(ct)) {
    unique(osv_upper(ct[[var]]))
  } else if (length(bits) >= 2L) {
    osv_upper(trimws(strsplit(bits[[2]], "[|;,]")[[1]]))
  } else {
    character()
  }
  allowed <- allowed[nzchar(allowed)]
  parts <- list()
  for (dom in domains) {
    df <- datasets[[dom]]
    if (!osv_has(df, var)) next
    vals <- osv_upper(df[[var]])
    bad <- which(nzchar(vals) & !vals %in% allowed)
    bad <- osv_limit(bad)
    if (length(bad)) {
      parts[[length(parts) + 1L]] <- osv_issue(
        rule$rule_id, rule$severity, dom,
        sprintf("%s Allowed: %s.", rule$message, paste(allowed, collapse = ", ")),
        variable = var, record = bad[[1]],
        value = paste(unique(vals[bad]), collapse = "|"),
        publisher = rule$publisher, count = length(bad)
      )
    }
  }
  osv_bind(parts)
}

osv_check_iso <- function(datasets, rule) {
  domains <- osv_match_domains(rule$domains, names(datasets))
  suffix <- if (nzchar(rule$argument)) rule$argument else "DTC"
  parts <- list()
  for (dom in domains) {
    df <- datasets[[dom]]
    vars <- names(df)[grepl(paste0(suffix, "$"), names(df))]
    for (v in vars) {
      vals <- as.character(df[[v]])
      vals[is.na(vals)] <- ""
      bad <- which(!osv_iso8601(vals))
      bad <- osv_limit(bad)
      if (length(bad)) {
        parts[[length(parts) + 1L]] <- osv_issue(
          rule$rule_id, rule$severity, dom, rule$message,
          variable = v, record = bad[[1]],
          value = paste(unique(vals[bad]), collapse = "|"),
          publisher = rule$publisher, count = length(bad)
        )
      }
    }
  }
  osv_bind(parts)
}

osv_check_unique <- function(datasets, rule) {
  domains <- osv_match_domains(rule$domains, names(datasets))
  keys <- trimws(strsplit(rule$argument, "[|;,+]")[[1]])
  parts <- list()
  for (dom in domains) {
    df <- datasets[[dom]]
    if (!all(keys %in% names(df))) next
    key <- interaction(df[keys], drop = TRUE, lex.order = TRUE)
    dup <- duplicated(key) | duplicated(key, fromLast = TRUE)
    if (any(dup)) {
      parts[[length(parts) + 1L]] <- osv_issue(
        rule$rule_id, rule$severity, dom, rule$message,
        variable = paste(keys, collapse = "+"),
        record = which(dup)[[1]],
        publisher = rule$publisher, count = sum(dup)
      )
    }
  }
  osv_bind(parts)
}

osv_check_one_to_one <- function(datasets, rule) {
  domains <- osv_match_domains(rule$domains, names(datasets))
  pair <- trimws(strsplit(rule$argument, "[|;,]")[[1]])
  if (length(pair) < 2L) return(NULL)
  a <- pair[[1]]
  b <- pair[[2]]
  parts <- list()
  for (dom in domains) {
    df <- datasets[[dom]]
    if (!all(c(a, b) %in% names(df))) next
    sub <- unique(df[nzchar(as.character(df[[a]])) & nzchar(as.character(df[[b]])), c(a, b)])
    n_b <- tapply(sub[[b]], sub[[a]], function(z) length(unique(z)))
    n_a <- tapply(sub[[a]], sub[[b]], function(z) length(unique(z)))
    bad <- names(n_b)[n_b > 1] 
    bad2 <- names(n_a)[n_a > 1]
    if (length(bad) || length(bad2)) {
      parts[[length(parts) + 1L]] <- osv_issue(
        rule$rule_id, rule$severity, dom, rule$message,
        variable = paste(a, b, sep = "/"),
        value = paste(c(bad, bad2), collapse = "|"),
        publisher = rule$publisher,
        count = length(unique(c(bad, bad2)))
      )
    }
  }
  osv_bind(parts)
}

osv_check_nonmissing <- function(datasets, rule) {
  domains <- osv_match_domains(rule$domains, names(datasets))
  vars <- trimws(strsplit(rule$argument, "[|;,]")[[1]])
  parts <- list()
  for (dom in domains) {
    df <- datasets[[dom]]
    for (v in vars) {
      if (!osv_has(df, v)) next
      vals <- df[[v]]
      bad <- which(is.na(vals) | !nzchar(trimws(as.character(vals))))
      if (length(bad)) {
        parts[[length(parts) + 1L]] <- osv_issue(
          rule$rule_id, rule$severity, dom, rule$message,
          variable = v, record = bad[[1]],
          publisher = rule$publisher, count = length(bad)
        )
      }
    }
  }
  osv_bind(parts)
}

osv_check_regex <- function(datasets, rule) {
  domains <- osv_match_domains(rule$domains, names(datasets))
  bits <- strsplit(rule$argument, "=", fixed = TRUE)[[1]]
  var <- trimws(bits[[1]])
  pat <- bits[[2]]
  parts <- list()
  for (dom in domains) {
    df <- datasets[[dom]]
    if (!osv_has(df, var)) next
    vals <- as.character(df[[var]])
    vals[is.na(vals)] <- ""
    bad <- which(nzchar(vals) & !grepl(pat, vals))
    bad <- osv_limit(bad)
    if (length(bad)) {
      parts[[length(parts) + 1L]] <- osv_issue(
        rule$rule_id, rule$severity, dom, rule$message,
        variable = var, record = bad[[1]],
        value = paste(unique(vals[bad]), collapse = "|"),
        publisher = rule$publisher, count = length(bad)
      )
    }
  }
  osv_bind(parts)
}

osv_dispatch <- function(datasets, rule, ct = NULL) {
  switch(
    rule$check,
    dataset_required = osv_check_dataset_required(datasets, rule),
    variable_required = osv_check_variable_required(datasets, rule),
    codelist = osv_check_codelist(datasets, rule, ct),
    iso8601 = osv_check_iso(datasets, rule),
    key_unique = osv_check_unique(datasets, rule),
    one_to_one = osv_check_one_to_one(datasets, rule),
    nonmissing = osv_check_nonmissing(datasets, rule),
    regex = osv_check_regex(datasets, rule),
    NULL
  )
}
