# Tests for R/data-removal.R — apply_data_removal()
# The authoritative data-protection guardian.

# Build a fully-populated dataset once for reuse across tests.
base_ds <- make_populated_test_ds()

# Helper: run apply_data_removal with specific options.
# Defaults keep everything; overrides via ... replace specific flags.
remove_with <- function(ds = base_ds, ...) {
  defaults <- list(
    include_department       = "full",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full",
    patient_columns          = "id",
    include_dhis2_ids        = c("patients", "enrollments", "departments",
                                 "events", "notes", "event_types", "users"))
  args <- utils::modifyList(defaults, list(...))
  opts <- do.call(dhis2_dataset_options, args)
  neoipcr:::apply_data_removal(ds, opts)
}

# --- patient_columns — controls patient_id exposure ---

test_that("apply_data_removal keeps patient_id when 'id' is in patient_columns", {
  result <- remove_with(patient_columns = "id")
  expect_true("patient_id" %in% names(result$patients))
})

test_that("apply_data_removal removes patient_id when 'id' not in patient_columns", {
  result <- remove_with(patient_columns = character())
  expect_false("patient_id" %in% names(result$patients))
})

# --- include_dhis2_ids ---

test_that("apply_data_removal removes trackedEntity when patients not in include_dhis2_ids", {
  result <- remove_with(include_dhis2_ids = c("enrollments", "departments",
    "events", "notes", "event_types", "users"))
  expect_false("trackedEntity" %in% names(result$patients))
})

test_that("apply_data_removal removes enrollment ID when enrollments not in include_dhis2_ids", {
  result <- remove_with(include_dhis2_ids = c("patients", "departments",
    "events", "notes", "event_types", "users"))
  expect_false("enrollment" %in% names(result$enrollments))
})

test_that("apply_data_removal removes orgUnit from departments when departments not in include_dhis2_ids", {
  result <- remove_with(include_dhis2_ids = c("patients", "enrollments",
    "events", "notes", "event_types", "users"))
  expect_false("orgUnit" %in% names(result$metadata$departments))
})

test_that("apply_data_removal removes event ID when events not in include_dhis2_ids", {
  result <- remove_with(include_dhis2_ids = c("patients", "enrollments",
    "departments", "notes", "event_types", "users"))
  expect_false("event" %in% names(result$events))
  expect_false("event" %in% names(result$eventDetails))
})

test_that("apply_data_removal removes note ID when notes not in include_dhis2_ids", {
  result <- remove_with(include_dhis2_ids = c("patients", "enrollments",
    "departments", "events", "event_types", "users"))
  expect_false("note" %in% names(result$eventNotes))
})

test_that("apply_data_removal removes programStage when event_types not in include_dhis2_ids", {
  result <- remove_with(include_dhis2_ids = c("patients", "enrollments",
    "departments", "events", "notes", "users"))
  expect_false("programStage" %in% names(result$metadata$eventTypes))
})

test_that("apply_data_removal removes user ID when users not in include_dhis2_ids", {
  result <- remove_with(include_dhis2_ids = c("patients", "enrollments",
    "departments", "events", "notes", "event_types"))
  expect_false("user" %in% names(result$metadata$users))
})

test_that("apply_data_removal keeps all IDs when all types in include_dhis2_ids", {
  result <- remove_with()
  expect_true("trackedEntity" %in% names(result$patients))
  expect_true("enrollment" %in% names(result$enrollments))
  expect_true("event" %in% names(result$events))
  expect_true("programStage" %in% names(result$metadata$eventTypes))
})

# --- include_department ---

test_that("include_department = fullkeeps departments and department_key", {
  result <- remove_with(include_department = "full")
  expect_false(is.null(result$metadata$departments))
  expect_true("department_key" %in% names(result$patients))
  expect_true("department_key" %in% names(result$enrollments))
  expect_true("department_key" %in% names(result$events))
})

test_that("include_department = no removes departments table and department_key columns", {
  result <- remove_with(include_department = "no")
  expect_null(result$metadata$departments)
  expect_false("department_key" %in% names(result$patients))
  expect_false("department_key" %in% names(result$enrollments))
  expect_false("department_key" %in% names(result$events))
})

test_that("include_department = pseudo with dhis2 IDs keeps only department_key and orgUnit", {
  result <- remove_with(include_department = "pseudo")
  expect_equal(
    sort(names(result$metadata$departments)),
    sort(c("department_key", "orgUnit")))
  # Foreign keys preserved in data tables
  expect_true("department_key" %in% names(result$patients))
})

test_that("include_department = pseudo without dhis2 IDs removes departments table", {
  result <- remove_with(
    include_department = "pseudo",
    include_dhis2_ids = c("patients", "enrollments",
      "events", "notes", "event_types", "users"))
  expect_null(result$metadata$departments)
})

# --- include_hospital ---

test_that("include_hospital = no removes hospitals table and hospital_key columns", {
  result <- remove_with(include_hospital = "no")
  expect_null(result$metadata$hospitals)
  expect_false("hospital_key" %in% names(result$patients))
  expect_false("hospital_key" %in% names(result$enrollments))
  expect_false("hospital_key" %in% names(result$events))
  # Also removed from departments metadata
  expect_false("hospital_key" %in% names(result$metadata$departments))
})

test_that("include_hospital = pseudo removes hospitals table but keeps hospital_key", {
  result <- remove_with(include_hospital = "pseudo")
  expect_null(result$metadata$hospitals)
  expect_true("hospital_key" %in% names(result$patients))
})

# --- include_country ---
#
# Like WB classes, the `metadata$countries` tibble shape is reader-owned
# under the three-mode schema contract (`countries_cols` in
# R/schema-orgunits.R, verified in test-dhis2-metadata.R). Tests here
# cover `apply_data_removal()`'s remaining responsibility: scrubbing the
# `country_key` foreign key from fact and adjacent-metadata tables when
# the user opted out of countries entirely.

test_that("include_country = no removes country_key from every fact and metadata table", {
  result <- remove_with(include_country = "no")
  expect_false("country_key" %in% names(result$patients))
  expect_false("country_key" %in% names(result$enrollments))
  expect_false("country_key" %in% names(result$events))
  # Cascades to hospitals and departments metadata
  expect_false("country_key" %in% names(result$metadata$hospitals))
  expect_false("country_key" %in% names(result$metadata$departments))
})

test_that("include_country = pseudo keeps country_key in fact tables", {
  # The fact-table FK is retained so pseudo keys can still group joins
  # across fact tables. The table-shape contract is enforced by the
  # reader and tested in `test-dhis2-metadata.R`.
  result <- remove_with(include_country = "pseudo")
  expect_true("country_key" %in% names(result$patients))
  expect_true("country_key" %in% names(result$enrollments))
  expect_true("country_key" %in% names(result$events))
})

test_that("include_country = full keeps country_key in fact tables", {
  result <- remove_with(include_country = "full")
  expect_true("country_key" %in% names(result$patients))
  expect_true("country_key" %in% names(result$enrollments))
  expect_true("country_key" %in% names(result$events))
})

# --- include_world_bank_class ---
#
# The shape of `metadata$worldBankClasses` itself is produced by the
# reader (`read_metadata_wb_classes` in R/dhis2-metadata-reference.R)
# under the three-mode `worldBankClasses_cols` contract and verified in
# `test-dhis2-metadata.R`. The tests here cover `apply_data_removal()`'s
# remaining responsibility: scrubbing the `world_bank_class_key` foreign
# key from fact and other-metadata tables when the user opted out of WB
# classes entirely.

test_that("include_world_bank_class = no removes world_bank_class_key from every fact and metadata table", {
  result <- remove_with(include_world_bank_class = "no")
  expect_false("world_bank_class_key" %in% names(result$patients))
  expect_false("world_bank_class_key" %in% names(result$enrollments))
  expect_false("world_bank_class_key" %in% names(result$events))
  # Cascades through adjacent metadata
  expect_false("world_bank_class_key" %in% names(result$metadata$countries))
  expect_false("world_bank_class_key" %in% names(result$metadata$hospitals))
  expect_false("world_bank_class_key" %in% names(result$metadata$departments))
})

test_that("include_world_bank_class = pseudo keeps world_bank_class_key in fact tables", {
  # The fact-table FK is retained so pseudo keys can still group joins
  # across fact tables. The table-shape contract is enforced by the
  # reader and tested in `test-dhis2-metadata.R`.
  result <- remove_with(include_world_bank_class = "pseudo")
  expect_true("world_bank_class_key" %in% names(result$patients))
  expect_true("world_bank_class_key" %in% names(result$enrollments))
  expect_true("world_bank_class_key" %in% names(result$events))
})

test_that("include_world_bank_class = full keeps world_bank_class_key in fact tables", {
  result <- remove_with(include_world_bank_class = "full")
  expect_true("world_bank_class_key" %in% names(result$patients))
  expect_true("world_bank_class_key" %in% names(result$enrollments))
  expect_true("world_bank_class_key" %in% names(result$events))
})

# --- Cascading removal ---

test_that("removing country with hospital = no does not error on missing hospitals", {
  result <- remove_with(
    include_hospital = "no",
    include_country  = "no")
  expect_null(result$metadata$hospitals)
  # `countries` shape is reader-owned — verified in test-dhis2-metadata.R.
  # Here we just confirm the FK-scrub cascade fires on patients.
  expect_false("country_key" %in% names(result$patients))
})

test_that("removing world_bank_class with country = no does not error on missing countries", {
  result <- remove_with(
    include_country          = "no",
    include_world_bank_class = "no")
  # `countries` + `worldBankClasses` shapes are reader-owned — verified
  # in test-dhis2-metadata.R. Here we just confirm the cascade doesn't
  # crash when both options are "no" simultaneously and that the FK
  # columns are scrubbed from patients as a consequence.
  expect_false("country_key" %in% names(result$patients))
  expect_false("world_bank_class_key" %in% names(result$patients))
})

# --- Full removal (most restrictive) ---

test_that("all include flags at most restrictive removes all optional data", {
  result <- remove_with(
    patient_columns          = character(),
    include_dhis2_ids        = character(),
    include_department       = "no",
    include_hospital         = "no",
    include_country          = "no",
    include_world_bank_class = "no")
  # Core keys survive
  expect_true("patient_key" %in% names(result$patients))
  expect_true("enrollment_key" %in% names(result$enrollments))
  expect_true("event_key" %in% names(result$events))
  # Optional data removed
  expect_false("patient_id" %in% names(result$patients))
  expect_false("trackedEntity" %in% names(result$patients))
  expect_null(result$metadata$departments)
  expect_null(result$metadata$hospitals)
  # `countries` + `worldBankClasses` shapes are reader-owned under the
  # three-mode schema contract and asserted in test-dhis2-metadata.R.
  # Here we just confirm the FK-column scrub cascaded.
  expect_false("country_key" %in% names(result$patients))
  expect_false("country_key" %in% names(result$enrollments))
  expect_false("country_key" %in% names(result$events))
  expect_false("world_bank_class_key" %in% names(result$patients))
  expect_false("world_bank_class_key" %in% names(result$enrollments))
  expect_false("world_bank_class_key" %in% names(result$events))
})

# --- Null-safe: eventNotes can be NULL ---

test_that("apply_data_removal handles NULL eventNotes gracefully", {
  ds <- base_ds
  ds$eventNotes <- NULL
  result <- remove_with(ds, include_dhis2_ids = character())
  expect_null(result$eventNotes)
})
