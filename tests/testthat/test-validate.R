test_that("example SDTM study raises rejection and identifier issues", {
  report <- validate_submission(example_study("sdtm"), standard = "SDTM")
  expect_s3_class(report, "osv_report")
  ids <- report$issues$rule_id
  expect_true("SDTM003" %in% ids || "TRC1734" %in% ids)
  expect_true("SDTM011" %in% ids)
  expect_true("SDTM010" %in% ids)
  expect_true(any(report$issues$severity == "Reject"))
  expect_s3_class(summary(report), "summary.osv_report")
})

test_that("example ADaM study flags ADSL key and PARAM pair", {
  report <- validate_submission(example_study("adam"), standard = "ADaM")
  ids <- report$issues$rule_id
  expect_true("ADAM002" %in% ids)
  expect_true("ADAM005" %in% ids || "ADAM025" %in% ids)
  expect_true("TRC1736D" %in% ids)
})

test_that("custom rule is executed", {
  study <- read_study_data(example_study("sdtm"))
  extra <- data.frame(
    rule_id = "CUST1", publisher = "user", standard = "SDTM",
    domains = "DM", severity = "Warning", check = "codelist",
    argument = "SEX=M|F", message = "Custom sex check.",
    guidance = "test", stringsAsFactors = FALSE
  )
  report <- validate_datasets(study, standard = "SDTM", rules = extra)
  expect_true("CUST1" %in% report$issues$rule_id)
})

test_that("ISO 8601 helper accepts partial dates", {
  expect_true(all(openstudyval:::osv_iso8601(c("2020", "2020-01", "2020-01-02", ""))))
  expect_false(openstudyval:::osv_iso8601("20JAN2020"))
})

test_that("rule catalog can explain a known rule", {
  hit <- explain_rule("SDTM024")
  expect_equal(hit$check, "codelist")
  expect_error(explain_rule("NO-SUCH"))
})
