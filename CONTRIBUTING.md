# Contributing

Issues and pull requests are welcome.

- Do not add rule text copied from Pinnacle 21, CDISC conformance-rule PDFs, or other copyrighted validator catalogs.
- New executable checks should cite the public source (FDA TCG section, eCTD criterion, or the standard's structural requirement) in the `guidance` column.
- Add a test that fails on a small synthetic data frame and passes on a corrected one.
- Keep Imports limited to base packages. Optional readers stay in Suggests.
