# openstudyval 0.2.1 — Enhanced

Open-source R validator for FDA study-data submissions (SDTM, SEND, and ADaM).

**Independent open-source implementation. Not affiliated with or a replacement for Pinnacle 21 Enterprise.**

## Features in 0.2.1

- Expanded rule catalog (additional common SDTM, ADaM, and SEND checks)
- YAML configuration support (`validate_from_config()`)
- Version filtering, rule subsetting, and batch processing
- Excel + improved HTML reporting

## Quick Start

```r
# Install dependencies for full features
install.packages(c("haven", "xml2", "writexl", "yaml"))

library(openstudyval)

# Simple validation
report <- validate_submission(example_study("sdtm"), standard = "SDTM")
summary(report)
write_validation_report(report, "report.html")

# Using YAML config
# Create config.yaml:
# standard: SDTM
# standard_version: "3.3"
# severity: ["Reject", "Error"]

report <- validate_from_config("path/to/study", "config.yaml")
```

## YAML Configuration Example

```yaml
standard: ADaM
standard_version: "1.2"
severity: ["Reject", "Error", "Warning"]
# rule_ids: ["ADAM030", "ADAM031"]
```

## Disclaimer

A clean openstudyval report is **not** an FDA or PMDA certification.  
Use this package for early structural checks inside an R workflow. Formal submissions should still use established commercial validators (e.g., Pinnacle 21 Enterprise).

## License

MIT
