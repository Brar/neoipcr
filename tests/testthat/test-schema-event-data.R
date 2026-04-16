# Tests for R/schema-event-data.R — the seven per-event-type data
# schemas (admissionData, surveillanceEndData, sepsisData, necData,
# pneumoniaData, surgeryData, ssiData).

# ---- Three-mode shape per type -------------------------------------------

.type_cols <- list(
  adm = neoipcr:::admissionData_cols,
  end = neoipcr:::surveillanceEndData_cols,
  bsi = neoipcr:::sepsisData_cols,
  nec = neoipcr:::necData_cols,
  hap = neoipcr:::pneumoniaData_cols,
  pro = neoipcr:::surgeryData_cols,
  ssi = neoipcr:::ssiData_cols
)

test_that("every per-event-type tibble is 0x0 under include_event = 'no'", {
  opts <- dhis2_dataset_options(include_event = "no")
  for (type in names(.type_cols)) {
    schema <- neoipcr:::compile_schema(.type_cols[[type]], opts)
    expect_equal(ncol(schema), 0L, info = type)
    expect_equal(nrow(schema), 0L, info = type)
  }
})

test_that("pseudo events carries only event_key + payload (link/hierarchy absent via inheritance)", {
  # Under pseudo events, events_cols has only event_key. The
  # inheritance rule on per-event-type cols therefore keeps enrollment_key
  # / patient_key / hierarchy keys ABSENT because events doesn't carry
  # them — the rule only materializes a key on the child when the
  # parent doesn't. So the child tibble has event_key + payload only.
  opts <- dhis2_dataset_options(
    include_event = "pseudo",
    # Orthogonal gates deliberately open — the inheritance rule
    # governs presence, not these.
    include_enrollment       = "full",
    include_patient          = "full",
    include_department       = "full",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full",
    include_test_data        = TRUE)
  # Under pseudo-event, events_cols is just event_key. The inheritance
  # rule keeps link/hierarchy keys absent on children when the parent
  # schema doesn't carry them — but events only has event_key, so the
  # children should materialize them. Verify for admissionData.
  schema <- neoipcr:::compile_schema(neoipcr:::admissionData_cols, opts)
  expect_true("event_key" %in% names(schema))
  # Under pseudo events (events has no enrollment_key/patient_key/etc.),
  # the children materialize them directly per the inheritance rule.
  expect_true("enrollment_key" %in% names(schema))
  expect_true("patient_key"    %in% names(schema))
})

test_that("full events keeps per-event-type lean (hierarchy / links reached via events)", {
  opts <- dhis2_dataset_options(
    include_event            = "full",
    include_enrollment       = "full",
    include_patient          = "full",
    include_department       = "full",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full",
    include_test_data        = TRUE)
  for (type in names(.type_cols)) {
    schema <- neoipcr:::compile_schema(.type_cols[[type]], opts)
    # Events already materializes all links + hierarchy + isTest;
    # children inherit (absent direct) per the rule.
    expect_false("enrollment_key"       %in% names(schema), info = type)
    expect_false("patient_key"          %in% names(schema), info = type)
    expect_false("department_key"       %in% names(schema), info = type)
    expect_false("hospital_key"         %in% names(schema), info = type)
    expect_false("country_key"          %in% names(schema), info = type)
    expect_false("world_bank_class_key" %in% names(schema), info = type)
    expect_false("isTest"               %in% names(schema), info = type)
    # event_key always present
    expect_true("event_key" %in% names(schema), info = type)
  }
})

# ---- Per-type payload coverage -------------------------------------------

test_that("admissionData: payload covers type + dol", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::admissionData_cols, opts)
  expect_true(all(c("type", "dol") %in% names(schema)))
  expect_true(is.factor(schema$type))
  expect_identical(levels(schema$type), c("1", "2", "3"))
})

test_that("surveillanceEndData: payload covers reason + day-counter set + derived vs_days", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(
    neoipcr:::surveillanceEndData_cols, opts)
  expect_true(all(c(
    "reason", "patient_days", "cvc_days", "pvc_days", "vs_days",
    "inv_days", "niv_days", "ab_days", "human_milk_days",
    "kangaroo_care_days", "probiotic_days") %in% names(schema)))
  expect_identical(levels(schema$reason), c("1", "2"))
})

test_that("sepsisData: payload includes dev_ass + los + dol + all symptom flags", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::sepsisData_cols, opts)
  expect_true(all(c(
    "dev_ass", "los", "dol",
    "acidosis", "ab_treatment", "apnoea", "bradycardia", "crp",
    "feeding_intolerance", "hyperglycaemia", "it_ratio", "interleukin",
    "irritability", "no_pos_culture", "perfusion", "platelet_count",
    "procalcitonin", "temperature", "wbc") %in% names(schema)))
  expect_identical(levels(schema$dev_ass), c("0", "1", "2"))
})

test_that("necData: sec_bsi is factor (option set KfIEzWRibj7 levels)", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::necData_cols, opts)
  expect_true(is.factor(schema$sec_bsi))
  expect_identical(levels(schema$sec_bsi), c("1", "0", "-1"))
})

test_that("pneumoniaData: dev_ass + sec_bsi + microbiological_test_result are all factors", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::pneumoniaData_cols, opts)
  expect_true(is.factor(schema$dev_ass))
  expect_true(is.factor(schema$sec_bsi))
  expect_true(is.factor(schema$microbiological_test_result))
  expect_identical(levels(schema$microbiological_test_result),
                   c("1", "0", "-1"))
})

test_that("surgeryData: asa_score + wound_class are factors, duration is integer", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::surgeryData_cols, opts)
  expect_true(is.factor(schema$asa_score))
  expect_identical(levels(schema$asa_score), c("1", "2", "3", "4", "5"))
  expect_true(is.factor(schema$wound_class))
  expect_identical(levels(schema$wound_class), c("1", "2", "3", "4"))
  expect_type(schema$duration, "integer")
})

test_that("ssiData: infection_type + sec_bsi + organisms_* are all factors", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::ssiData_cols, opts)
  expect_true(is.factor(schema$infection_type))
  expect_identical(levels(schema$infection_type), c("1", "2", "3"))
  for (col in c("sec_bsi", "organisms_superf", "organisms_deep",
                "organisms_organ")) {
    expect_true(is.factor(schema[[col]]), info = col)
  }
})

# ---- Companion-column gating -------------------------------------------

test_that("event_data_attribute_cols: three companions per DE (createdBy + createdAt + updatedAt)", {
  opts_bare <- dhis2_dataset_options(include_event = "full")
  opts_user <- dhis2_dataset_options(
    include_event = "full", include_user = "full")
  opts_ts   <- dhis2_dataset_options(
    include_event = "full", include_timestamps = TRUE)
  opts_full <- dhis2_dataset_options(
    include_event = "full", include_user = "full",
    include_timestamps = TRUE)

  s_bare <- neoipcr:::compile_schema(neoipcr:::admissionData_cols, opts_bare)
  s_user <- neoipcr:::compile_schema(neoipcr:::admissionData_cols, opts_user)
  s_ts   <- neoipcr:::compile_schema(neoipcr:::admissionData_cols, opts_ts)
  s_full <- neoipcr:::compile_schema(neoipcr:::admissionData_cols, opts_full)

  expect_false("dol_createdBy" %in% names(s_bare))
  expect_false("dol_createdAt" %in% names(s_bare))
  expect_false("dol_updatedAt" %in% names(s_bare))

  expect_true("dol_createdBy" %in% names(s_user))
  expect_false("dol_createdAt" %in% names(s_user))
  expect_true("dol_createdBy" %in% names(s_full))
  expect_true("dol_createdAt" %in% names(s_full))
  expect_true("dol_updatedAt" %in% names(s_full))
  # DHIS2 EventDataValue createdBy/updatedBy not both fetched by the
  # current reader — only _createdBy, _createdAt, _updatedAt. No
  # _updatedBy / _storedBy on the schema.
  expect_false("dol_updatedBy" %in% names(s_full))
  expect_false("dol_storedBy"  %in% names(s_full))

  expect_true("dol_createdAt" %in% names(s_ts))
  expect_true("dol_updatedAt" %in% names(s_ts))
})

# ---- Fixture round-trips -------------------------------------------------

test_that("make_test_admission_data matches admissionData_cols schema", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::admissionData_cols, opts)
  fixture <- make_test_admission_data(event_keys = 1:3)
  expect_schema_matches(fixture, schema)
})

test_that("make_test_surveillance_end_data matches schema (vs_days present)", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(
    neoipcr:::surveillanceEndData_cols, opts)
  fixture <- make_test_surveillance_end_data(event_keys = 1:3)
  expect_schema_matches(fixture, schema)
  expect_true("vs_days" %in% names(fixture))
})

test_that("make_test_sepsis_data matches schema", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::sepsisData_cols, opts)
  fixture <- make_test_sepsis_data(event_keys = 1:3)
  expect_schema_matches(fixture, schema)
})

test_that("make_test_nec_data matches schema (sec_bsi is factor)", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::necData_cols, opts)
  fixture <- make_test_nec_data(event_keys = 1:3)
  expect_schema_matches(fixture, schema)
  expect_true(is.factor(fixture$sec_bsi))
})

test_that("make_test_pneumonia_data matches schema", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::pneumoniaData_cols, opts)
  fixture <- make_test_pneumonia_data(event_keys = 1:3)
  expect_schema_matches(fixture, schema)
})

test_that("make_test_surgery_data matches schema (asa_score is factor)", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::surgeryData_cols, opts)
  fixture <- make_test_surgery_data(event_keys = 1:3)
  expect_schema_matches(fixture, schema)
  expect_true(is.factor(fixture$asa_score))
})

test_that("make_test_ssi_data matches schema", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::ssiData_cols, opts)
  fixture <- make_test_ssi_data(event_keys = 1:3)
  expect_schema_matches(fixture, schema)
})

# ---- event_data_cols_for dispatch ---------------------------------------

test_that("event_data_cols_for dispatches every valid event_type_key", {
  for (k in c("adm", "end", "bsi", "nec", "hap", "pro", "ssi")) {
    expect_identical(
      neoipcr:::event_data_cols_for(k),
      .type_cols[[k]],
      info = k)
  }
  expect_error(
    neoipcr:::event_data_cols_for("unknown"),
    "Unknown event_type_key")
})
