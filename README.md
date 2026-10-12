# openstudyval

Open-source R validator for FDA study-data submissions (SDTM, SEND, and ADaM).

This package follows the same *workflow* as a commercial validator such as Pinnacle 21 Community: point it at a study folder, choose a standard, optionally pass define.xml and controlled terminology, and get a severity-ranked HTML/CSV report.

It is **not** Pinnacle 21. Rule text, engine, and report layout are original. It implements publicly described expectations from the FDA Study Data Technical Conformance Guide and the eCTD technical-rejection criteria for study data (1734, 1735, 1736, 1738). It does not copy Pinnacle 21 rule IDs, messages, or configuration files, and a clean report is not an FDA or PMDA certification.

## Install

From this repository, once you have published it:

```r
# install.packages("remotes")
remotes::install_github("shekharchalwadigrepo-hub/openstudyval")
```

After CRAN acceptance:

```r
install.packages("openstudyval")
```

SAS transport files need the suggested package `haven`. define.xml content checks need `xml2`.

```r
install.packages(c("haven", "xml2"))
```

## Validate a study

```r
library(openstudyval)

# Bundled example (issues are intentional)
report <- validate_submission(example_study("sdtm"), standard = "SDTM")
summary(report)

write_validation_report(report, "validation-report.html")

# A real folder of dm.xpt, ae.xpt, ts.xpt, define.xml, ...
report <- validate_submission(
  "m5/datasets/abc123/tabulations/sdtm",
  standard = "SDTM"
)
```

`standard` is `"SDTM"`, `"SEND"`, or `"ADaM"`. CSV and RDS are read with base R. `.xpt` is read with `haven`.

## What is checked

| Layer | Examples |
| --- | --- |
| Technical rejection | Missing `ts.xpt` / SSTDTC (1734), missing DM or ADSL and define.xml (1736), STUDYID consistency (1738), STF presence note (1735) |
| XPORT / TCG | Dataset and variable names of 1–8 characters, labels ≤ 40, character values ≤ 200, files over 5 GB |
| SDTM / SEND | DM required, trial-design presence, DOMAIN matches dataset, USUBJID starts with STUDYID, USUBJID in DM, `--SEQ` uniqueness, ISO 8601 `--DTC`, core codelists, paired test code/name |
| ADaM | ADSL required, one row per USUBJID, subjects exist on ADSL, PARAM/PARAMCD one-to-one, population flags, TRTSDT ≤ TRTEDT |
| define.xml | Well-formed XML, ItemGroupDef names vs datasets (requires `xml2`) |

MedDRA and WHODrug are licensed and are not shipped. Pass a study codelist:

```r
ct <- data.frame(AESEV = c("MILD", "MODERATE", "SEVERE"))
validate_submission(path, standard = "SDTM", ct = ct)
```

## Add rules without editing the package

Catalog columns: `rule_id`, `publisher`, `standard`, `domains`, `severity`, `check`, `argument`, `message`, `guidance`.

`check` is one of `dataset_required`, `variable_required`, `codelist`, `iso8601`, `key_unique`, `one_to_one`, `nonmissing`, `regex`.

```r
extra <- data.frame(
  rule_id = "CUST_ARM", publisher = "sponsor", standard = "SDTM",
  domains = "DM", severity = "Error", check = "codelist",
  argument = "ARMCD=A|B|SCRNFAIL",
  message = "ARMCD is not in the study arm list.",
  guidance = "Protocol section 6",
  stringsAsFactors = FALSE
)
validate_submission(path, standard = "SDTM", rules = extra)
```

Domains accept `AE|LB`, `AD*`, or `*`.

## Before you submit this package to CRAN

1. Replace `Package Maintainer <you@example.com>` in `DESCRIPTION` with your name and a working email.
2. Replace `YOU/openstudyval` in `DESCRIPTION`, `README.md`, and this repo URL.
3. Run `R CMD check --as-cran` and keep a clean check on at least one platform besides your own. GitHub Actions in `.github/workflows/R-CMD-check.yaml` does this.
4. Confirm the license (MIT) is what you want to ship.
5. Do not claim FDA endorsement or compatibility with a named Pinnacle 21 engine version.

## Repository layout

```
R/                  validator, report writer, rule engine
inst/extdata/rules  core rule catalog
inst/extdata/example_sdtm  intentional-failure demo
inst/extdata/example_adam
tests/testthat
vignettes/validating.Rmd
```

## License

MIT. See [LICENSE](LICENSE).
