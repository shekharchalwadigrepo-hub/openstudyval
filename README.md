# openstudyval (Enhanced 0.2.0)

Open-source R validator for FDA study-data submissions (SDTM, SEND, and ADaM).

**This is an independent open-source implementation. It is not affiliated with, endorsed by, or a replacement for Pinnacle 21 Enterprise.**

## New in 0.2.0

- **Version support**: Pass `standard_version` (e.g. `"3.3"`, `"1.2"`)
- **Rule filtering**: Run only specific `rule_ids` or `severity` levels
- **Batch processing**: `validate_studies()` for multiple folders
- **Excel export**: `write_validation_report(..., format = "excel")` (requires `writexl`)
- **Improved HTML reports** with better styling and version info
- Clearer positioning as a lightweight early-check tool

## Install

```r
# From local folder (after downloading this package)
install.packages("path/to/openstudyval-enhanced", repos = NULL, type = "source")

# Or after you push to GitHub
remotes::install_github("shekharchalwadigrepo-hub/openstudyval")
```

Suggested packages for full functionality:

```r
install.packages(c("haven", "xml2", "writexl"))
```

## Usage

```r
library(openstudyval)

# Basic
report <- validate_submission(example_study("sdtm"), standard = "SDTM")
summary(report)

# With version and filtered rules
report <- validate_submission(
  "path/to/sdtm",
  standard = "SDTM",
  standard_version = "3.3",
  severity = c("Reject", "Error")
)

# Batch
reports <- validate_studies(c("study1", "study2"), standard = "ADaM")

# Reports
write_validation_report(report, "report.html")
write_validation_report(report, "report.xlsx", format = "excel")
```

## Important Disclaimer

A clean report from openstudyval is **not** an FDA or PMDA certification.  
Pinnacle 21 Enterprise (and similar commercial tools) remain the reference for formal submissions. Use this package for early, R-native structural checks inside your development workflow.

## License

MIT
