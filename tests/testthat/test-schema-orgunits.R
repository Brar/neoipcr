# Tests for R/schema-orgunits.R — entity column declarations for the
# org-unit hierarchy (World Bank classes today; countries / hospitals /
# departments in subsequent Phase B sub-tasks).
#
# These tests are schema-engine level: they exercise the column lists
# and `compile_schema()` output directly, without constructing DHIS2
# metadata input. Reader-level tests that exercise the three-mode
# contract on populated metadata live in `test-dhis2-metadata.R`.

# --- worldBankClasses_cols — per-column include_when predicates ---

test_that("col_wb_class_key appears iff include_world_bank_class != 'no'", {
  key_col <- neoipcr:::col_wb_class_key

  expect_false(key_col$include_when(dhis2_dataset_options(
    include_world_bank_class = "no")))
  expect_true(key_col$include_when(dhis2_dataset_options(
    include_world_bank_class = "pseudo")))
  expect_true(key_col$include_when(dhis2_dataset_options(
    include_world_bank_class = "full")))
})

test_that("`class` column appears only under include_world_bank_class = 'full'", {
  class_col <- purrr::detect(
    neoipcr:::worldBankClasses_cols, \(c) c$name == "class")
  expect_false(is.null(class_col))

  expect_false(class_col$include_when(dhis2_dataset_options(
    include_world_bank_class = "no")))
  expect_false(class_col$include_when(dhis2_dataset_options(
    include_world_bank_class = "pseudo")))
  expect_true(class_col$include_when(dhis2_dataset_options(
    include_world_bank_class = "full")))
})

test_that("`class` column declares fixed factor levels c('L','LM','UM','H')", {
  class_col <- purrr::detect(
    neoipcr:::worldBankClasses_cols, \(c) c$name == "class")
  expect_identical(class_col$factor_levels, c("L", "LM", "UM", "H"))
  expect_identical(class_col$levels_source, "fixed")
})

test_that("`fiscal_year` column appears only under 'full'", {
  fy_col <- purrr::detect(
    neoipcr:::worldBankClasses_cols, \(c) c$name == "fiscal_year")
  expect_false(is.null(fy_col))

  expect_false(fy_col$include_when(dhis2_dataset_options(
    include_world_bank_class = "no")))
  expect_false(fy_col$include_when(dhis2_dataset_options(
    include_world_bank_class = "pseudo")))
  expect_true(fy_col$include_when(dhis2_dataset_options(
    include_world_bank_class = "full")))
})

# --- get_worldBankClasses_schema — three-mode shape contract ---

test_that("get_worldBankClasses_schema is 0x0 under 'no'", {
  schema <- neoipcr:::get_worldBankClasses_schema(
    dhis2_dataset_options(include_world_bank_class = "no"))

  expect_schema_matches(schema, tibble::tibble())
  expect_equal(ncol(schema), 0L)
  expect_equal(nrow(schema), 0L)
})

test_that("get_worldBankClasses_schema is single-column under 'pseudo'", {
  schema <- neoipcr:::get_worldBankClasses_schema(
    dhis2_dataset_options(include_world_bank_class = "pseudo"))

  expect_equal(ncol(schema), 1L)
  expect_equal(nrow(schema), 0L)
  expect_identical(names(schema), "world_bank_class_key")
  expect_true(is.integer(schema$world_bank_class_key))
})

test_that("get_worldBankClasses_schema is full-schema under 'full'", {
  schema <- neoipcr:::get_worldBankClasses_schema(
    dhis2_dataset_options(include_world_bank_class = "full"))

  expect_equal(ncol(schema), 3L)
  expect_equal(nrow(schema), 0L)
  expect_identical(
    names(schema),
    c("world_bank_class_key", "class", "fiscal_year"))
  expect_true(is.integer(schema$world_bank_class_key))
  expect_true(is.factor(schema$class))
  expect_identical(levels(schema$class), c("L", "LM", "UM", "H"))
  expect_true(is.integer(schema$fiscal_year))
})

test_that("get_worldBankClasses_schema strict 0 -> 1 -> N column-count progression", {
  opts_no     <- dhis2_dataset_options(include_world_bank_class = "no")
  opts_pseudo <- dhis2_dataset_options(include_world_bank_class = "pseudo")
  opts_full   <- dhis2_dataset_options(include_world_bank_class = "full")

  expect_equal(ncol(neoipcr:::get_worldBankClasses_schema(opts_no)),     0L)
  expect_equal(ncol(neoipcr:::get_worldBankClasses_schema(opts_pseudo)), 1L)
  expect_true(ncol(neoipcr:::get_worldBankClasses_schema(opts_full))    > 1L)
})

test_that("get_worldBankClasses_schema does not depend on unrelated options", {
  # Iterate every other relevant option field; schema shape must follow
  # only `include_world_bank_class`.
  for (opts in iter_dataset_options(
    fields = c("include_country", "include_hospital"))) {
    for (wb_mode in c("no", "pseudo", "full")) {
      opts$include_world_bank_class <- wb_mode
      schema <- neoipcr:::get_worldBankClasses_schema(opts)

      expected_ncol <- switch(wb_mode, "no" = 0L, "pseudo" = 1L, "full" = 3L)
      expect_equal(ncol(schema), expected_ncol,
        info = sprintf(
          "wb_mode='%s', include_country='%s', include_hospital='%s'",
          wb_mode, opts$include_country, opts$include_hospital))
    }
  }
})

# --- assert_schema sanity: builder output matches declared schema ---

test_that("make_test_metadata_wb_classes('full') matches full schema", {
  schema <- neoipcr:::get_worldBankClasses_schema(
    dhis2_dataset_options(include_world_bank_class = "full"))
  fixture <- make_test_metadata_wb_classes(
    n = 2, include_world_bank_class = "full")

  expect_schema_matches(fixture, schema)
  expect_equal(nrow(fixture), 2L)
})

test_that("make_test_metadata_wb_classes('pseudo') matches pseudo schema", {
  schema <- neoipcr:::get_worldBankClasses_schema(
    dhis2_dataset_options(include_world_bank_class = "pseudo"))
  fixture <- make_test_metadata_wb_classes(
    n = 3, include_world_bank_class = "pseudo")

  expect_schema_matches(fixture, schema)
  expect_equal(nrow(fixture), 3L)
  expect_identical(names(fixture), "world_bank_class_key")
})

test_that("make_test_metadata_wb_classes('no') matches empty schema", {
  schema <- neoipcr:::get_worldBankClasses_schema(
    dhis2_dataset_options(include_world_bank_class = "no"))
  fixture <- make_test_metadata_wb_classes(include_world_bank_class = "no")

  expect_schema_matches(fixture, schema)
  expect_equal(ncol(fixture), 0L)
  expect_equal(nrow(fixture), 0L)
})
