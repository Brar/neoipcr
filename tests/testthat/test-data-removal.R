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

# `patient_id` / `trackedEntity` on patients are now reader-owned via
# `R/schema-patients.R::patients_cols`: `patient_id` is gated by `"id"
# %in% patient_columns`, `trackedEntity` by `"patients" %in%
# include_dhis2_ids`. The legacy scrubs in `apply_data_removal()` are
# redundant and have been removed. See `test-schema-orgunits.R` and
# (future) schema-level patient tests for the invariants.

# --- include_dhis2_ids ---

test_that("apply_data_removal removes enrollment ID when enrollments not in include_dhis2_ids", {
  result <- remove_with(include_dhis2_ids = c("patients", "departments",
    "events", "notes", "event_types", "users"))
  expect_false("enrollment" %in% names(result$enrollments))
})

# Note: `orgUnit` stripping under `"departments" %not in% include_dhis2_ids`
# is now handled by the reader's `finalize_to_schema()` via
# `departments_cols` in R/schema-orgunits.R, not by `apply_data_removal()`.
# Coverage moved to `test-schema-orgunits.R`.

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

# `event_types` — tibble shape is now reader-owned via
# `R/schema-orgunits.R::eventTypes_cols`. The `programStage` column is
# gated on `"event_types" %in% include_dhis2_ids` at the schema level;
# the legacy scrub in `apply_data_removal()` is redundant and has been
# removed. See `test-schema-orgunits.R` for schema-level coverage.

# `users` — tibble shape is now reader-owned via
# `R/schema-orgunits.R::users_cols`. The `user` column is gated on
# `"users" %in% include_dhis2_ids` at the schema level; the legacy
# scrub in `apply_data_removal()` is redundant and has been removed.
# See `test-schema-orgunits.R` for the schema-level coverage of the
# `users` tibble's three-mode shape.

test_that("apply_data_removal keeps all IDs when all types in include_dhis2_ids", {
  result <- remove_with()
  expect_true("trackedEntity" %in% names(result$patients))
  expect_true("enrollment" %in% names(result$enrollments))
  expect_true("event" %in% names(result$events))
  expect_true("programStage" %in% names(result$metadata$eventTypes))
})

# --- include_department ---
#
# Like the other hierarchy entities, the `metadata$departments` tibble
# shape is reader-owned under the three-mode schema contract
# (`departments_cols` in R/schema-orgunits.R, verified in
# test-dhis2-metadata.R). Tests here cover `apply_data_removal()`'s
# remaining responsibility: scrubbing the `department_key` foreign key
# from fact tables when the user opted out of departments entirely.

test_that("include_department = full keeps department_key in fact tables", {
  result <- remove_with(include_department = "full")
  expect_true("department_key" %in% names(result$patients))
  expect_true("department_key" %in% names(result$enrollments))
  expect_true("department_key" %in% names(result$events))
})

test_that("include_department = no removes department_key from fact tables", {
  result <- remove_with(include_department = "no")
  expect_false("department_key" %in% names(result$patients))
  expect_false("department_key" %in% names(result$enrollments))
  expect_false("department_key" %in% names(result$events))
})

test_that("include_department = pseudo keeps department_key in fact tables", {
  result <- remove_with(include_department = "pseudo")
  # Foreign keys preserved in data tables — the tibble shape itself is
  # reader-owned and verified via the schema tests.
  expect_true("department_key" %in% names(result$patients))
  expect_true("department_key" %in% names(result$enrollments))
  expect_true("department_key" %in% names(result$events))
})

# --- include_hospital ---
#
# Like WB classes and countries, the `metadata$hospitals` tibble shape
# is reader-owned under the three-mode schema contract (`hospitals_cols`
# in R/schema-orgunits.R, verified in test-dhis2-metadata.R). Tests here
# cover `apply_data_removal()`'s remaining responsibility: scrubbing the
# `hospital_key` foreign key from fact and adjacent-metadata tables when
# the user opted out of hospitals entirely.

test_that("include_hospital = no removes hospital_key from every fact and metadata table", {
  result <- remove_with(include_hospital = "no")
  expect_false("hospital_key" %in% names(result$patients))
  expect_false("hospital_key" %in% names(result$enrollments))
  expect_false("hospital_key" %in% names(result$events))
  # Also removed from departments metadata
  expect_false("hospital_key" %in% names(result$metadata$departments))
})

# `hospital_key` on fact tables is reader-owned via `patients_cols` /
# `enrollments_cols` / `events_cols`'s inheritance rule: a fact tibble
# carries `hospital_key` directly only when its immediate parent's
# schema doesn't already carry it. Under the common full-department
# case (departments has hospital_key pre-joined) patients reaches it
# via one-hop `department_key → departments`, so `hospital_key` is
# absent from patients directly. Hospital-key presence on enrollments
# / events lands with the fact-table phase B sub-tasks.
test_that("include_hospital = pseudo keeps hospital_key reachable (via departments)", {
  result <- remove_with(include_hospital = "pseudo")
  # Under include_department = "full" + inheritance, hospital_key lives
  # on departments and is reachable from patients via department_key.
  expect_true("hospital_key" %in% names(result$metadata$departments))
  expect_true("department_key" %in% names(result$patients))
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
  # `hospitals` + `countries` shapes are reader-owned — verified in
  # test-dhis2-metadata.R. Here we just confirm the FK-scrub cascade
  # fires on patients for both keys.
  expect_false("hospital_key" %in% names(result$patients))
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
  # `patient_id` / `trackedEntity` shape is reader-owned via
  # `patients_cols` and tested under `test-schema-patients.R`.
  # All hierarchy tibble shapes are reader-owned under the three-mode
  # schema contract (see test-dhis2-metadata.R). Here we just confirm
  # the FK-column scrubs cascaded to fact tables.
  expect_false("department_key" %in% names(result$patients))
  expect_false("department_key" %in% names(result$enrollments))
  expect_false("department_key" %in% names(result$events))
  # `hospitals` + `countries` + `worldBankClasses` shapes are
  # reader-owned under the three-mode schema contract and asserted in
  # test-dhis2-metadata.R. Here we just confirm the FK-column scrubs
  # cascaded.
  expect_false("hospital_key" %in% names(result$patients))
  expect_false("hospital_key" %in% names(result$enrollments))
  expect_false("hospital_key" %in% names(result$events))
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
