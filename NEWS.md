# openstudyval 0.2.0

## New features
- Added `standard_version` argument to `validate_submission()` and `validate_datasets()`.
- Added rule filtering via `rule_ids` and `severity` arguments.
- Added `validate_studies()` for batch validation of multiple study folders.
- Added Excel export support in `write_validation_report()` (requires the suggested package `writexl`).
- Added YAML configuration support via `load_config()` and `validate_from_config()`.
- Expanded the rule catalog with additional common SDTM, SEND, and ADaM checks.
- Improved HTML report styling and content (version information, rule count, better severity highlighting).

## Other changes
- Updated package version label to "openstudyval 0.2.0".
- Clarified in documentation that the package is an independent open-source tool and is not a replacement for Pinnacle 21 Enterprise.

# openstudyval 0.1.1

- Initial CRAN release.
