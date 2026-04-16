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

# ---- countries_cols — per-column include_when predicates ---

test_that("col_country_key appears iff include_country != 'no'", {
  key_col <- neoipcr:::col_country_key

  expect_false(key_col$include_when(dhis2_dataset_options(
    include_country = "no")))
  expect_true(key_col$include_when(dhis2_dataset_options(
    include_country = "pseudo")))
  expect_true(key_col$include_when(dhis2_dataset_options(
    include_country = "full")))
})

test_that("country display columns appear only under include_country = 'full'", {
  # All four display columns are requested from DHIS2 under `include_country
  # == "full"` via `organisationUnitGroups:fields` in R/dhis2-metadata.R.
  for (name in c("code", "displayName", "displayShortName",
                 "displayDescription")) {
    col <- purrr::detect(
      neoipcr:::countries_cols, \(c) c$name == name)
    expect_false(is.null(col))
    expect_false(col$include_when(dhis2_dataset_options(
      include_country = "no")),
      info = name)
    expect_false(col$include_when(dhis2_dataset_options(
      include_country = "pseudo")),
      info = name)
    expect_true(col$include_when(dhis2_dataset_options(
      include_country = "full")),
      info = name)
    expect_identical(col$levels_source, "data", info = name)
  }
})

test_that("countries schema has world_bank_class_key iff BOTH country and WB are non-'no'", {
  # Tests the direct parent-link FK contract at the SCHEMA level, not
  # at the atom level. The `world_bank_class_key` column on countries
  # is the shared `col_wb_class_key` atom (predicate:
  # `include_world_bank_class != "no"`) gated by the countries
  # containing-entity gate (`include_country != "no"`). Neither alone
  # encodes the compound rule; the composition does — which is the
  # whole point of `with_entity_gate`.
  for (cmode in c("no", "pseudo", "full")) {
    for (wbmode in c("no", "pseudo", "full")) {
      schema <- neoipcr:::get_countries_schema(dhis2_dataset_options(
        include_country = cmode, include_world_bank_class = wbmode))
      expected <- cmode != "no" && wbmode != "no"
      expect_equal(
        "world_bank_class_key" %in% names(schema),
        expected,
        info = sprintf("c=%s, wb=%s", cmode, wbmode))
    }
  }
})

test_that("get_countries_schema honors 0 -> 1 -> N progression across full cross-product", {
  # Regression for the latent containing-entity-gate gap: under every
  # combination of `include_country × include_world_bank_class`, the
  # schema shape must obey the strict progression. In particular,
  # under `include_country = "no"` + any WB mode, the schema must be
  # 0×0 — no stray `world_bank_class_key` leaking through because the
  # shared atom's predicate would otherwise fire.
  for (cmode in c("no", "pseudo", "full")) {
    for (wbmode in c("no", "pseudo", "full")) {
      opts <- dhis2_dataset_options(
        include_country = cmode, include_world_bank_class = wbmode)
      schema <- neoipcr:::get_countries_schema(opts)

      expected_ncol <- if (cmode == "no") {
        0L
      } else if (cmode == "pseudo") {
        if (wbmode == "no") 1L else 2L  # country_key (+ wb FK if WB exists)
      } else {  # "full"
        if (wbmode == "no") 5L else 6L  # + 4 display cols (+ wb FK)
      }

      expect_equal(ncol(schema), expected_ncol,
        info = sprintf("c=%s, wb=%s", cmode, wbmode))
      expect_equal(nrow(schema), 0L,
        info = sprintf("c=%s, wb=%s", cmode, wbmode))
    }
  }
})

# --- get_countries_schema — three-mode shape contract ---

test_that("get_countries_schema is 0x0 under include_country = 'no'", {
  # Strict 0×0 regardless of include_world_bank_class (inheritance
  # through an absent entity is still absent).
  for (wb_mode in c("no", "pseudo", "full")) {
    opts <- dhis2_dataset_options(
      include_country = "no", include_world_bank_class = wb_mode)
    schema <- neoipcr:::get_countries_schema(opts)
    expect_equal(ncol(schema), 0L,
      info = sprintf("wb_mode='%s'", wb_mode))
    expect_equal(nrow(schema), 0L,
      info = sprintf("wb_mode='%s'", wb_mode))
  }
})

test_that("get_countries_schema is 1 column under include_country='pseudo', wb='no'", {
  schema <- neoipcr:::get_countries_schema(dhis2_dataset_options(
    include_country = "pseudo", include_world_bank_class = "no"))

  expect_equal(ncol(schema), 1L)
  expect_identical(names(schema), "country_key")
  expect_true(is.integer(schema$country_key))
})

test_that("get_countries_schema is 2 columns under pseudo + wb non-'no'", {
  # Under pseudo mode, the public schema keeps `country_key` + the
  # direct WB-class link FK — that's how pseudo countries still group
  # into WB classes.
  for (wb_mode in c("pseudo", "full")) {
    schema <- neoipcr:::get_countries_schema(dhis2_dataset_options(
      include_country = "pseudo", include_world_bank_class = wb_mode))
    expect_equal(ncol(schema), 2L,
      info = sprintf("wb_mode='%s'", wb_mode))
    expect_identical(
      names(schema),
      c("country_key", "world_bank_class_key"),
      info = sprintf("wb_mode='%s'", wb_mode))
  }
})

test_that("get_countries_schema is full schema under include_country='full'", {
  # Full schema with all display columns + direct WB-class link when WB
  # is non-"no".
  schema <- neoipcr:::get_countries_schema(dhis2_dataset_options(
    include_country = "full", include_world_bank_class = "full"))

  expect_equal(ncol(schema), 6L)
  expect_identical(
    names(schema),
    c("country_key", "code", "displayName", "displayShortName",
      "displayDescription", "world_bank_class_key"))
  expect_true(is.integer(schema$country_key))
  expect_s3_class(schema$code, "ordered")
  expect_s3_class(schema$displayName, "ordered")
  expect_s3_class(schema$displayShortName, "ordered")
  expect_s3_class(schema$displayDescription, "ordered")
  expect_true(is.integer(schema$world_bank_class_key))
})

test_that("get_countries_schema full - wb_no = 5 columns (no WB FK)", {
  schema <- neoipcr:::get_countries_schema(dhis2_dataset_options(
    include_country = "full", include_world_bank_class = "no"))

  expect_equal(ncol(schema), 5L)
  expect_identical(
    names(schema),
    c("country_key", "code", "displayName", "displayShortName",
      "displayDescription"))
})

test_that("get_countries_schema strict 0 -> 1 -> N column-count progression under wb='no'", {
  opts_no     <- dhis2_dataset_options(
    include_country = "no", include_world_bank_class = "no")
  opts_pseudo <- dhis2_dataset_options(
    include_country = "pseudo", include_world_bank_class = "no")
  opts_full   <- dhis2_dataset_options(
    include_country = "full", include_world_bank_class = "no")

  expect_equal(ncol(neoipcr:::get_countries_schema(opts_no)),     0L)
  expect_equal(ncol(neoipcr:::get_countries_schema(opts_pseudo)), 1L)
  expect_true(ncol(neoipcr:::get_countries_schema(opts_full))   > 1L)
})

test_that("make_test_metadata_countries matches schema across all (country, wb) modes", {
  for (c_mode in c("no", "pseudo", "full")) {
    for (wb_mode in c("no", "pseudo", "full")) {
      opts <- dhis2_dataset_options(
        include_country = c_mode, include_world_bank_class = wb_mode)
      schema  <- neoipcr:::get_countries_schema(opts)
      fixture <- make_test_metadata_countries(
        n = 2,
        include_country = c_mode,
        include_world_bank_class = wb_mode)

      expect_schema_matches(fixture, schema)
    }
  }
})
