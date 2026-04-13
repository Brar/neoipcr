# Tests for R/calc-api.R — calculate_department_data() integration test.

# Build a realistic neoipcr_ds with enough data for the full pipeline.
make_calc_test_ds <- function() {
  md <- read_test_metadata(
    dataset_options = dhis2_dataset_options(
      include_department = "yes",
      include_country    = "yes"))
  md$departments     <- make_test_metadata_departments()
  md$hospitals       <- make_test_metadata_hospitals()
  md$countries       <- make_test_metadata_countries()
  md$worldBankClasses <- make_test_metadata_wb_classes()
  md$eventTypes      <- make_test_metadata_event_types()
  md$dataset_options <- dhis2_dataset_options(
    include_department = "yes",
    include_country    = "yes")

  patients <- make_test_patients(3,
    department_key = c(1L, 1L, 2L),
    birth_weight   = c(800L, 1200L, 2500L),
    gest_age       = c("25+0", "30+0", "36+0"),
    total_gestation_days = c(175L, 210L, 252L))

  enrollments <- make_test_enrollments(3,
    patient_keys   = 1:3,
    department_key = c(1L, 1L, 2L),
    enrolledAt     = as.Date(c("2024-01-01", "2024-01-05", "2024-01-10")))

  # Events: adm + end + one BSI + one surgery per enrollment
  events <- make_test_events(
    n               = 9,
    enrollment_keys = c(1,1,1, 2,2,2, 3,3,3),
    patient_keys    = c(1,1,1, 2,2,2, 3,3,3),
    event_type_keys = c("adm","end","bsi", "adm","end","pro", "adm","end","bsi"),
    occurredAt      = as.Date(c(
      "2024-01-01","2024-01-15","2024-01-08",
      "2024-01-05","2024-01-20","2024-01-12",
      "2024-01-10","2024-01-25","2024-01-18")),
    department_key  = c(1,1,1, 1,1,1, 2,2,2))

  adm_data <- make_test_admission_data(c(1L, 4L, 7L))
  end_data <- make_test_surveillance_end_data(c(2L, 5L, 8L),
    patient_days = c(15L, 16L, 16L))
  bsi_data <- make_test_sepsis_data(c(3L, 9L),
    dol = c(8L, 9L), los = c(7L, 8L),
    dev_ass = factor(c("1", "0")))
  pro_data <- make_test_surgery_data(6L)
  iaf_data <- make_test_iaf(c(3L, 9L))
  sbd_data <- make_test_substance_days(c(2L, 5L, 8L))

  make_test_ds(
    metadata            = md,
    patients            = patients,
    enrollments         = enrollments,
    events              = events,
    admissionData       = adm_data,
    surveillanceEndData = end_data,
    sepsisData          = bsi_data,
    surgeryData         = pro_data,
    infectiousAgentFindings = iaf_data,
    substanceDays       = sbd_data)
}

test_that("calculate_department_data produces neoipcr_rep_ds", {
  ds <- make_calc_test_ds()
  result <- calculate_department_data(ds, use_cache = FALSE)

  expect_s3_class(result, "neoipcr_rep_ds")

  # All expected slots present
  expected_slots <- c(
    "metadata", "birth_weight_figure", "gestational_age_figure",
    "n_departments", "n_patients", "n_enrollments", "n_patient_days",
    "n_infections", "n_surgical_departments",
    "n_surgical_procedures", "n_surgical_patients",
    "usage_density_rate_table", "antibiotic_utilisation_table",
    "surgery_rate_table", "incidence_density_rate_table",
    "dev_ass_incidence_density_rate_table",
    "infectious_agent_detection_rate_per_agent_table",
    "abr_infection_rate_table", "organism_resistance_rate_table",
    "secondary_bsi_rate_table",
    "infectious_agent_detection_rate_per_inf_type_table",
    "resistance_test_rate_table")
  expect_true(all(expected_slots %in% names(result)))
})

test_that("calculate_department_data computes correct summary counts", {
  ds <- make_calc_test_ds()
  result <- calculate_department_data(ds, use_cache = FALSE)

  expect_equal(result$n_patients$total, 3L)
  expect_equal(result$n_enrollments$total, 3L)
  expect_equal(result$n_departments, 2L)
  # Patient days = sum of surveillance end patient_days: 15 + 16 + 16 = 47
  expect_equal(result$n_patient_days$total, 47L)
})

test_that("calculate_department_data usage_density_rate_table has expected structure", {
  ds <- make_calc_test_ds()
  result <- calculate_department_data(ds, use_cache = FALSE)

  udr <- result$usage_density_rate_table
  expect_s3_class(udr, "tbl_df")
  expect_true("factor" %in% names(udr))
  expect_true("days" %in% names(udr))
  expect_true("rate" %in% names(udr))
  # Should have rows for: cvc, pvc, vs, inv, niv, human_milk,
  # probiotic, kangaroo_care, ab, a, w, r
  expect_equal(nrow(udr), 12L)
})

test_that("calculate_department_data incidence_density_rate_table has structure", {
  ds <- make_calc_test_ds()
  result <- calculate_department_data(ds, use_cache = FALSE)

  idr <- result$incidence_density_rate_table
  expect_s3_class(idr, "tbl_df")
  expect_true(nrow(idr) > 0L)
  # Table has rate and count columns
  expect_true("rate" %in% names(idr) || "n" %in% names(idr))
})
