# Tests for R/validation-rules-patient.R — rules 56 to 58 and 73.

# Two patients with the multiple-birth flags `flags` and the numbers of
# infants `infants`, the columns the rule reads selected.
multiple_birth_ds <- function(flags, infants)
  make_test_ds(
    patients = make_test_patients(2,
      patient_columns = c("id", "multiple_birth", "siblings"),
      multiple_birth  = flags,
      siblings        = infants),
    enrollments = make_test_enrollments(2, patient_keys = c(1L, 2L)))

test_that("rule 56 detects a multiple birth with fewer than two infants", {
  result <- neoipcr:::validation_rule_56(
    multiple_birth_ds(c(TRUE, TRUE), c(1L, 2L)), NULL)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_equal(result$rule_id, 56L)
  expect_equal(result$patient_key, 1L)
  # A patient-level finding names no enrolment or event.
  expect_true(is.na(result$enrollment_key))
  expect_true(is.na(result$event_key))
  expect_named(result$context[[1]], "siblings")
  expect_equal(result$context[[1]]$siblings, 1L)
  # Zero is fewer than two as well.
  expect_equal(nrow(neoipcr:::validation_rule_56(
    multiple_birth_ds(c(TRUE, TRUE), c(0L, 2L)), NULL)), 1L)
})

test_that("rule 56 leaves a number of one alone without the multiple-birth flag", {
  # Without the flag the form does not ask for the number: a one stored
  # there is the client's to clear, and a missing flag is no flag.
  expect_equal(nrow(neoipcr:::validation_rule_56(
    multiple_birth_ds(c(FALSE, NA), c(1L, 1L)), NULL)), 0L)
})

test_that("rule 56 returns no rows for two or more infants or for a missing number", {
  expect_equal(nrow(neoipcr:::validation_rule_56(
    multiple_birth_ds(c(TRUE, TRUE), c(2L, 3L)), NULL)), 0L)
  # A flag without a number is a completeness matter, not this rule's.
  expect_equal(nrow(neoipcr:::validation_rule_56(
    multiple_birth_ds(c(TRUE, TRUE), c(NA_integer_, 2L)), NULL)), 0L)
})

test_that("rule 56 honours exceptions", {
  result <- neoipcr:::validation_rule_56(
    multiple_birth_ds(c(TRUE, TRUE), c(1L, 2L)),
    make_test_exceptions(56L, patient_key = 1L))
  expect_equal(nrow(result), 0L)
})

test_that("rule 56 skips without a warning when the flag or the number is absent", {
  for (col in c("multiple_birth", "siblings")) {
    ds <- multiple_birth_ds(c(TRUE, TRUE), c(1L, 2L))
    ds$patients[[col]] <- NULL
    expect_no_warning(result <- neoipcr:::validation_rule_56(ds, NULL))
    expect_null(result)
  }
})

# Three patients with the birth weights `weights`, the total gestation days
# `days` and the gestational-age texts `texts`, the columns the rules read
# selected.
eligibility_ds <- function(weights, days, texts = rep(NA_character_, 3))
  make_test_ds(
    patients = make_test_patients(3,
      patient_columns      = c("id", "birth_weight", "gestational_age"),
      birth_weight         = weights,
      total_gestation_days = days,
      gest_age             = texts),
    enrollments = make_test_enrollments(3, patient_keys = 1:3))

test_that("rule 57 detects a patient with neither birth weight nor gestational age", {
  result <- neoipcr:::validation_rule_57(
    eligibility_ds(c(NA, 1200L, 2500L), c(NA, 210L, 280L)), NULL)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_equal(result$rule_id, 57L)
  expect_equal(result$patient_key, 1L)
  # A patient-level finding names no enrolment or event, and with both
  # values missing it has nothing to record.
  expect_true(is.na(result$enrollment_key))
  expect_true(is.na(result$event_key))
  expect_null(result$context[[1]])
})

test_that("rule 57 takes an empty gestational-age text for none", {
  result <- neoipcr:::validation_rule_57(
    eligibility_ds(c(NA, NA, 2500L), c(NA, NA, 280L), c("", NA, NA)), NULL)
  expect_equal(result$patient_key, c(1L, 2L))
})

test_that("rule 57 leaves a patient with a gestational-age text alone, in whatever format", {
  # A text is a recorded gestational age even without the total gestation
  # days; a text in the wrong format is rule 58's finding, not this one's.
  expect_equal(nrow(neoipcr:::validation_rule_57(
    eligibility_ds(c(NA, NA, NA), c(NA, NA, NA), c("25+4", "25 +4", " ")),
    NULL)), 0L)
})

test_that("rule 57 leaves a patient with either value alone, eligible or not", {
  # One recorded value is enough for the eligibility filter to judge by:
  # a birth weight or a gestational age alone, below the bound or above it.
  expect_equal(nrow(neoipcr:::validation_rule_57(
    eligibility_ds(c(1200L, NA, 1600L), c(NA, 210L, NA)), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_57(
    eligibility_ds(c(NA, 2500L, 900L), c(240L, 280L, 192L)), NULL)), 0L)
})

test_that("rule 57 honours exceptions", {
  result <- neoipcr:::validation_rule_57(
    eligibility_ds(c(NA, NA, 1200L), c(NA, NA, 210L)),
    make_test_exceptions(57L, patient_key = 1L))
  expect_equal(result$patient_key, 2L)
})

test_that("rule 57 skips without a warning when the birth weight or the gestational age is absent", {
  # Without the text the rule cannot tell a patient who records only the
  # text from one who records nothing, so it skips rather than flag both.
  for (col in c("birth_weight", "total_gestation_days", "gest_age")) {
    ds <- eligibility_ds(c(NA, 1200L, 2500L), c(NA, 210L, 280L))
    ds$patients[[col]] <- NULL
    expect_no_warning(result <- neoipcr:::validation_rule_57(ds, NULL))
    expect_null(result)
  }
})

# --- Rule 58: the gestational-age text in the wrong format ---

# One patient per text in `texts`, each with an eligible birth weight and
# total gestation days, the text column selected.
gestational_age_text_ds <- function(texts)
  make_test_ds(
    patients = make_test_patients(length(texts),
      patient_columns = c("id", "birth_weight", "gestational_age"),
      gest_age        = texts),
    enrollments = make_test_enrollments(
      length(texts), patient_keys = seq_along(texts)))

test_that("rule 58 detects a gestational-age text in the wrong format", {
  result <- neoipcr:::validation_rule_58(
    gestational_age_text_ds(c("25 +4", "25+4")), NULL)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_equal(result$rule_id, 58L)
  expect_equal(result$patient_key, 1L)
  expect_true(is.na(result$enrollment_key))
  expect_true(is.na(result$event_key))
  expect_named(result$context[[1]], "gest_age")
  expect_equal(result$context[[1]]$gest_age, "25 +4")
})

test_that("rule 58 holds the text to the whole format the registration form requires", {
  # Weeks 20 to 49 and days 0 to 6; anything before or after the value,
  # a trailing line break included, is outside the format.
  wrong <- c("19+6", "50+0", "25+7", "25+", "+4", "25-4", "25+4 ", " 25+4",
             "25+4\n", "5+4", "025+4", "25+14", "25w4d", " ")
  result <- neoipcr:::validation_rule_58(gestational_age_text_ds(wrong), NULL)
  expect_equal(result$patient_key, seq_along(wrong))
  expect_equal(
    vapply(result$context, \(context) context$gest_age, character(1)), wrong)
  right <- c("20+0", "49+6", "25+4", "31+0", "40+3")
  expect_equal(nrow(neoipcr:::validation_rule_58(
    gestational_age_text_ds(right), NULL)), 0L)
})

test_that("rule 58 leaves a missing or empty text alone", {
  # An empty text is no text: whether a gestational age is recorded at all
  # is rule 57's matter.
  expect_equal(nrow(neoipcr:::validation_rule_58(
    gestational_age_text_ds(c("", NA_character_)), NULL)), 0L)
})

test_that("rule 58 honours exceptions", {
  result <- neoipcr:::validation_rule_58(
    gestational_age_text_ds(c("25 +4", "19+6")),
    make_test_exceptions(58L, patient_key = 1L))
  expect_equal(result$patient_key, 2L)
})

test_that("rule 58 skips without a warning when the gestational-age text is absent", {
  ds <- gestational_age_text_ds(c("25 +4", "25+4"))
  ds$patients$gest_age <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_58(ds, NULL))
  expect_null(result)
})

# --- Rule 73: the missing birth weight ---

test_that("rule 73 detects a patient with no birth weight but a gestational age", {
  # Patient 1 has the gestational age as a total, patient 3 only as a text;
  # both keep it, so neither is rule 57's patient, and the query asks for the
  # birth weight the gestational age does not stand in for.
  result <- neoipcr:::validation_rule_73(
    eligibility_ds(c(NA, 1200L, NA), c(210L, 210L, NA), c(NA, NA, "28+3")),
    NULL)
  expect_equal(result$patient_key, c(1L, 3L))
  expect_declared_context(result)
  expect_equal(result$rule_id, c(73L, 73L))
  # A patient-level finding names no enrolment or event, and the rule
  # carries no value: the finding is the missing field itself.
  expect_true(all(is.na(result$enrollment_key)))
  expect_true(all(is.na(result$event_key)))
  expect_null(result$context[[1]])
})

test_that("rule 73 leaves a patient with a birth weight alone, in whatever range", {
  # The rule queries the missing value, not an implausible one: a birth
  # weight, small or large, is a recorded birth weight.
  expect_equal(nrow(neoipcr:::validation_rule_73(
    eligibility_ds(c(420L, 1200L, 3100L), c(210L, 210L, 280L)), NULL)), 0L)
})

test_that("rule 73 leaves a patient with neither value alone, which is rule 57's finding", {
  # A patient who records neither an eligible birth weight nor a gestational
  # age (in either form) is rule 57's error, which removes the patient; the
  # query for the missing birth weight leaves it alone, so the two rules do
  # not flag the same record.
  expect_equal(nrow(neoipcr:::validation_rule_73(
    eligibility_ds(c(NA, NA, NA), c(NA, NA, NA), c(NA, "", NA)),
    NULL)), 0L)
})

test_that("rule 73 honours exceptions", {
  result <- neoipcr:::validation_rule_73(
    eligibility_ds(c(NA, NA, 1200L), c(210L, 210L, 210L)),
    make_test_exceptions(73L, patient_key = 1L))
  expect_equal(result$patient_key, 2L)
})

test_that("rule 73 skips without a warning when a value it reads is absent", {
  # Without the gestational age the rule cannot tell its patient from rule
  # 57's, so it skips rather than flag the overlap.
  for (col in c("birth_weight", "total_gestation_days", "gest_age")) {
    ds <- eligibility_ds(c(NA, 1200L, 2500L), c(210L, 210L, 280L))
    ds$patients[[col]] <- NULL
    expect_no_warning(result <- neoipcr:::validation_rule_73(ds, NULL))
    expect_null(result)
  }
})

test_that("rule 73 is registered as a patient-level warning", {
  # The warning severity is what makes the import's pass report the finding
  # without removing the patient (import-dhis2.R keeps a patient no error
  # flags); this pins the registration that drives that.
  entry <- Filter(\(rule) rule$id == 73L, neoipcr:::validation_rules)[[1]]
  expect_equal(entry$level, "patient")
  expect_equal(entry$severity, "warning")
  expect_false("event_types" %in% names(entry))
  expect_equal(entry$context, character())
  severities <- neoipcr::validation_rule_severities()
  expect_equal(unname(severities[["73"]]), "warning")
})
