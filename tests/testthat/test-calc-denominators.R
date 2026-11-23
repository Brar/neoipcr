# Tests for R/calc-denominators.R — the department of the stay. A stay's
# patient-days and its patients count for the department of its enrolment,
# not the department its patient's record or its forms were recorded under;
# the calculation entry points hand these functions the data set as
# `.departments_of_the_stay()` reads it, and these tests hand it to them the
# same way. Uses make_stay_department_calc_test_ds() from helper-fixtures.R.

test_that("risk time counts a stay's patient-days for the department of its stay", {
  ds <- make_stay_department_calc_test_ds()
  r <- neoipcr:::get_risk_time(
    neoipcr:::.departments_of_the_stay(ds),
    group_cols = "department_key", use_cache = FALSE)

  expect_equal(r$department_key, 2L)
  expect_equal(r$patient_days, 30L)
})

test_that("risk population counts a stay's patients and enrolments for the department of its stay", {
  ds <- make_stay_department_calc_test_ds()
  r <- neoipcr:::get_risk_population(
    neoipcr:::.departments_of_the_stay(ds),
    group_cols = "department_key", use_cache = FALSE)

  expect_equal(r$department_key, 2L)
  expect_equal(r$n_enrollments, 2L)
  expect_equal(r$n_patients, 2L)
})

test_that("surgery risk counts a stay's procedure for the department of its stay", {
  ds <- make_stay_department_calc_test_ds()
  r <- neoipcr:::get_surgery_risk(
    neoipcr:::.departments_of_the_stay(ds),
    group_cols = "department_key", use_cache = FALSE)

  expect_equal(r$department_key, 2L)
  expect_equal(r$n_procedures, 1L)
})

test_that("procedures count for the department of the stay the procedure belongs to", {
  ds <- make_stay_department_calc_test_ds()
  r <- neoipcr:::get_procedures(
    neoipcr:::.departments_of_the_stay(ds),
    group_cols = "department_key", use_cache = FALSE)

  expect_equal(r$department_key, 2L)
  expect_equal(r$n, 1L)
})
