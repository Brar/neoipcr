# Tests for R/calc-denominators.R — per-file unit tests.
# Uses make_exposure_patients_test_ds() from helper-fixtures.R.

exposure_ds <- make_exposure_patients_test_ds()

test_that("get_patient_exposure_days returns one row per patient with the eight factors' days", {
  result <- neoipcr:::get_patient_exposure_days(exposure_ds, use_cache = FALSE)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 16L)
  expect_setequal(
    names(result),
    c("patient_key", "department_key",
      "cvc_days", "pvc_days", "inv_days", "niv_days", "ab_days",
      "human_milk_days", "probiotic_days", "kangaroo_care_days"))
  p3 <- result[result$patient_key == 3, ]
  expect_equal(p3$cvc_days, 10)
  expect_equal(p3$human_milk_days, 20)
  expect_equal(p3$department_key, 1L)
})

test_that("the stays of one patient sum to that patient, and a stay without an antibiotic entry counts", {
  result <- neoipcr:::get_patient_exposure_days(exposure_ds, use_cache = FALSE)
  p1 <- result[result$patient_key == 1, ]
  # Patient 1's readmission carries two of its five CVC days and stands
  # without an antibiotic entry; a helper that left such stays out would
  # give three days, and one that kept the stays apart would give two rows.
  expect_equal(nrow(p1), 1L)
  expect_equal(p1$cvc_days, 5)
  expect_equal(p1$niv_days, 1)
})

test_that("patients without an exposure day keep a row at zero", {
  result <- neoipcr:::get_patient_exposure_days(exposure_ds, use_cache = FALSE)
  zeros <- result[result$patient_key %in% c(11, 14, 16), ]
  expect_equal(nrow(zeros), 3L)
  days <- zeros |> dplyr::select(tidyselect::ends_with("_days"))
  expect_equal(sum(unlist(days)), 0)
})

test_that("the patients of a department gather under its key", {
  result <- neoipcr:::get_patient_exposure_days(exposure_ds, use_cache = FALSE)
  expect_equal(
    result |> dplyr::count(.data$department_key, name = "n") |>
      dplyr::pull(.data$n),
    c(6L, 6L, 4L))
})

test_that("a stay with a blank day count adds none of its days, the known ones still counting", {
  blank_ds <- make_exposure_patients_test_ds()
  # Patient 1's readmission, the surveillance-end form of event 4, holds no
  # CVC day: unknown is not no. Its stay adds none, and the three days of
  # patient 1's first stay are all that stand counted.
  blank_ds$surveillanceEndData$cvc_days[
    blank_ds$surveillanceEndData$event_key == 4L] <- NA_integer_
  result <- neoipcr:::get_patient_exposure_days(blank_ds, use_cache = FALSE)
  expect_equal(result$cvc_days[result$patient_key == 1], 3)
  table_result <- get_patients_exposed_table(blank_ds, use_cache = FALSE)
  cvc <- table_result[table_result$factor == "cvc" &
                        table_result$measure == "patients_exposed", ]
  # Patient 1 is still one of the seven exposed.
  expect_equal(cvc$n, 7L)
  cvc_days <- table_result[table_result$factor == "cvc" &
                             table_result$measure == "mean_days", ]
  expect_equal(cvc_days$n, 29L)
})
