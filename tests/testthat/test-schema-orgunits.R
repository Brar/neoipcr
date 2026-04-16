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

# ---- hospitals_cols — per-column include_when predicates ---

test_that("col_hospital_key appears iff include_hospital != 'no'", {
  key_col <- neoipcr:::col_hospital_key
  expect_false(key_col$include_when(dhis2_dataset_options(
    include_hospital = "no")))
  expect_true(key_col$include_when(dhis2_dataset_options(
    include_hospital = "pseudo")))
  expect_true(key_col$include_when(dhis2_dataset_options(
    include_hospital = "full")))
})

test_that("hospitals orgUnit appears iff 'hospitals' in include_dhis2_ids", {
  col <- purrr::detect(
    neoipcr:::hospitals_cols, \(c) c$name == "orgUnit")
  expect_false(is.null(col))
  expect_false(col$include_when(dhis2_dataset_options(
    include_dhis2_ids = character())))
  expect_true(col$include_when(dhis2_dataset_options(
    include_dhis2_ids = "hospitals")))
})

test_that("hospitals display columns appear only under include_hospital = 'full'", {
  for (name in c("code", "displayName", "displayShortName",
                 "displayDescription", "comment",
                 "longitude", "latitude")) {
    col <- purrr::detect(
      neoipcr:::hospitals_cols, \(c) c$name == name)
    expect_false(is.null(col), info = name)
    expect_false(col$include_when(dhis2_dataset_options(
      include_hospital = "pseudo")),
      info = name)
    expect_true(col$include_when(dhis2_dataset_options(
      include_hospital = "full")),
      info = name)
  }
})

test_that("hospitals country_key (direct link-FK) appears iff BOTH include_hospital and include_country are non-'no'", {
  # `col_country_key`'s single-option predicate is `include_country != "no"`;
  # the hospitals entity-gate supplies the `include_hospital != "no"` half.
  # Result: the column only appears at the schema level when both halves pass.
  for (hmode in c("no", "pseudo", "full")) {
    for (cmode in c("no", "pseudo", "full")) {
      schema <- neoipcr:::get_hospitals_schema(dhis2_dataset_options(
        include_hospital = hmode, include_country = cmode))
      expected <- hmode != "no" && cmode != "no"
      expect_equal(
        "country_key" %in% names(schema),
        expected,
        info = sprintf("h=%s, c=%s", hmode, cmode))
    }
  }
})

test_that("hospitals world_bank_class_key follows the inheritance rule", {
  # The key appears on hospitals only when:
  #   - include_hospital != "no" (entity exists), AND
  #   - include_world_bank_class != "no" (key is meaningful), AND
  #   - countries' compiled schema doesn't already carry it
  #     (countries has it when include_country != "no", so inheritance
  #     only fires under include_country == "no").
  for (hmode in c("no", "pseudo", "full")) {
    for (cmode in c("no", "pseudo", "full")) {
      for (wbmode in c("no", "pseudo", "full")) {
        schema <- neoipcr:::get_hospitals_schema(dhis2_dataset_options(
          include_hospital         = hmode,
          include_country          = cmode,
          include_world_bank_class = wbmode))
        expected <-
          hmode != "no" &&
          wbmode != "no" &&
          cmode == "no"
        expect_equal(
          "world_bank_class_key" %in% names(schema),
          expected,
          info = sprintf("h=%s, c=%s, wb=%s", hmode, cmode, wbmode))
      }
    }
  }
})

# --- get_hospitals_schema — three-mode shape contract ---

test_that("get_hospitals_schema is 0x0 under include_hospital = 'no' regardless of country/WB mode", {
  for (cmode in c("no", "pseudo", "full")) {
    for (wbmode in c("no", "pseudo", "full")) {
      opts <- dhis2_dataset_options(
        include_hospital         = "no",
        include_country          = cmode,
        include_world_bank_class = wbmode)
      schema <- neoipcr:::get_hospitals_schema(opts)
      expect_equal(ncol(schema), 0L,
        info = sprintf("c=%s, wb=%s", cmode, wbmode))
      expect_equal(nrow(schema), 0L,
        info = sprintf("c=%s, wb=%s", cmode, wbmode))
    }
  }
})

test_that("get_hospitals_schema is 1 column under pseudo + country='no' + wb='no'", {
  schema <- neoipcr:::get_hospitals_schema(dhis2_dataset_options(
    include_hospital         = "pseudo",
    include_country          = "no",
    include_world_bank_class = "no",
    include_dhis2_ids        = character()))
  expect_equal(ncol(schema), 1L)
  expect_identical(names(schema), "hospital_key")
})

test_that("get_hospitals_schema under pseudo + country='no' + wb!='no' has 2 cols (inherited WB)", {
  for (wbmode in c("pseudo", "full")) {
    schema <- neoipcr:::get_hospitals_schema(dhis2_dataset_options(
      include_hospital         = "pseudo",
      include_country          = "no",
      include_world_bank_class = wbmode,
      include_dhis2_ids        = character()))
    expect_equal(ncol(schema), 2L, info = sprintf("wb=%s", wbmode))
    expect_identical(
      names(schema),
      c("hospital_key", "world_bank_class_key"),
      info = sprintf("wb=%s", wbmode))
  }
})

test_that("get_hospitals_schema under pseudo + country!='no' has 2 cols (country_key, no inherited WB)", {
  # Inheritance rule: countries carries wb_class_key here, so hospitals
  # does NOT — reach it via one-hop join through country_key.
  for (cmode in c("pseudo", "full")) {
    for (wbmode in c("pseudo", "full")) {
      schema <- neoipcr:::get_hospitals_schema(dhis2_dataset_options(
        include_hospital         = "pseudo",
        include_country          = cmode,
        include_world_bank_class = wbmode,
        include_dhis2_ids        = character()))
      expect_equal(ncol(schema), 2L,
        info = sprintf("c=%s, wb=%s", cmode, wbmode))
      expect_identical(
        names(schema),
        c("hospital_key", "country_key"),
        info = sprintf("c=%s, wb=%s", cmode, wbmode))
    }
  }
})

test_that("get_hospitals_schema under full + full country + full WB has the expected 10 columns", {
  schema <- neoipcr:::get_hospitals_schema(dhis2_dataset_options(
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full",
    include_dhis2_ids        = "hospitals"))
  expect_identical(
    names(schema),
    c("hospital_key", "orgUnit", "code", "displayName", "displayShortName",
      "displayDescription", "comment", "longitude", "latitude",
      "country_key"))
  # No `world_bank_class_key` — inheritance says countries carries it.
  expect_false("world_bank_class_key" %in% names(schema))
})

test_that("get_hospitals_schema under full + country='no' + full WB materializes inherited WB key", {
  schema <- neoipcr:::get_hospitals_schema(dhis2_dataset_options(
    include_hospital         = "full",
    include_country          = "no",
    include_world_bank_class = "full",
    include_dhis2_ids        = "hospitals"))
  # `country_key` absent (countries gate = "no"); `world_bank_class_key`
  # inherited directly because countries can't relay.
  expect_false("country_key" %in% names(schema))
  expect_true("world_bank_class_key" %in% names(schema))
})

test_that("make_test_metadata_hospitals matches schema across key (h, c, wb) combinations", {
  # Full 27-combo cross-product of (hospital × country × WB modes) on
  # both include_dhis2_ids configurations (hospitals present or not).
  for (hmode in c("no", "pseudo", "full")) {
    for (cmode in c("no", "pseudo", "full")) {
      for (wbmode in c("no", "pseudo", "full")) {
        for (ids in list(character(), "hospitals")) {
          opts <- dhis2_dataset_options(
            include_hospital         = hmode,
            include_country          = cmode,
            include_world_bank_class = wbmode,
            include_dhis2_ids        = ids)
          schema  <- neoipcr:::get_hospitals_schema(opts)
          fixture <- make_test_metadata_hospitals(
            n = 2,
            include_hospital         = hmode,
            include_country          = cmode,
            include_world_bank_class = wbmode,
            include_dhis2_ids        = ids)

          expect_schema_matches(fixture, schema)
        }
      }
    }
  }
})
