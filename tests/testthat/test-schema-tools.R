# Tests for R/schema-tools.R — the schema engine.

# ---- schema_col ----------------------------------------------------------

test_that("schema_col builds a neoipcr_schema_col object with declared fields", {
  col <- neoipcr:::schema_col("foo", integer())
  expect_s3_class(col, "neoipcr_schema_col")
  expect_identical(col$name, "foo")
  expect_identical(col$type, integer())
  expect_true(is.function(col$include_when))
  expect_null(col$factor_levels)
  expect_identical(col$levels_source, "fixed")
})

test_that("schema_col accepts a factor column with explicit levels", {
  col <- neoipcr:::schema_col(
    "sex", factor(),
    factor_levels = c("f", "m", "u"))
  expect_identical(col$factor_levels, c("f", "m", "u"))
  expect_identical(col$levels_source, "fixed")
})

test_that("schema_col accepts levels_source = 'data' for data-derived factors", {
  col <- neoipcr:::schema_col(
    "displayName", factor(),
    factor_levels = character(),
    levels_source = "data")
  expect_identical(col$levels_source, "data")
})

test_that("schema_col rejects invalid inputs with actionable messages", {
  expect_error(
    neoipcr:::schema_col(c("a", "b"), integer()),
    "single character string")
  expect_error(
    neoipcr:::schema_col("foo", 1:3),
    "zero-length vector")
  expect_error(
    neoipcr:::schema_col("foo", integer(), include_when = "not a function"),
    "must be a function")
  expect_error(
    neoipcr:::schema_col("foo", integer(), factor_levels = 1:3),
    "NULL or a character vector")
  expect_error(
    neoipcr:::schema_col("foo", integer(), levels_source = "bogus"),
    "must be one of")
})

# ---- compile_schema -----------------------------------------------------

# A small set of opts objects we can reuse. `opts_all_full` turns every
# 3-valued gate to "full" so every include_when returning != "no"
# evaluates TRUE.
opts_all_full <- function(...) dhis2_dataset_options(
  include_world_bank_class = "full",
  include_country          = "full",
  include_hospital         = "full",
  include_department       = "full",
  include_user             = "full",
  include_patient          = "full",
  include_enrollment       = "full",
  include_event            = "full",
  ...
)

test_that("compile_schema returns an empty tibble when no column is included", {
  cols <- list(
    neoipcr:::schema_col("a", integer(), \(opts) FALSE),
    neoipcr:::schema_col("b", character(), \(opts) FALSE)
  )
  out <- neoipcr:::compile_schema(cols, dhis2_dataset_options())
  expect_s3_class(out, "tbl_df")
  expect_identical(nrow(out), 0L)
  expect_identical(ncol(out), 0L)
})

test_that("compile_schema produces a 0-row tibble with declared columns and types", {
  cols <- list(
    neoipcr:::schema_col("id",   integer()),
    neoipcr:::schema_col("name", character())
  )
  out <- neoipcr:::compile_schema(cols, dhis2_dataset_options())
  expect_identical(names(out), c("id", "name"))
  expect_identical(nrow(out), 0L)
  expect_type(out$id,   "integer")
  expect_type(out$name, "character")
})

test_that("compile_schema preserves declaration order", {
  cols <- list(
    neoipcr:::schema_col("z", integer()),
    neoipcr:::schema_col("a", character()),
    neoipcr:::schema_col("m", logical())
  )
  expect_identical(
    names(neoipcr:::compile_schema(cols, dhis2_dataset_options())),
    c("z", "a", "m"))
})

test_that("compile_schema filters by include_when(opts)", {
  cols <- list(
    neoipcr:::schema_col("always",    integer()),
    neoipcr:::schema_col("full_only", integer(),
                         \(opts) opts$include_country == "full")
  )
  out_no <- neoipcr:::compile_schema(
    cols, dhis2_dataset_options(include_country = "no"))
  out_full <- neoipcr:::compile_schema(
    cols, dhis2_dataset_options(include_country = "full"))
  expect_identical(names(out_no),   "always")
  expect_identical(names(out_full), c("always", "full_only"))
})

test_that("compile_schema builds factor columns with declared levels", {
  cols <- list(neoipcr:::schema_col(
    "sex", factor(), factor_levels = c("f", "m", "u")))
  out <- neoipcr:::compile_schema(cols, dhis2_dataset_options())
  expect_true(is.factor(out$sex))
  expect_identical(levels(out$sex), c("f", "m", "u"))
})

# ---- schema_codes --------------------------------------------------------

test_that("schema_codes returns compiled column names", {
  cols <- list(
    neoipcr:::schema_col("a", integer()),
    neoipcr:::schema_col("b", integer(), \(opts) FALSE),
    neoipcr:::schema_col("c", integer())
  )
  expect_identical(
    neoipcr:::schema_codes(cols, dhis2_dataset_options()),
    c("a", "c"))
})

# ---- assert_schema -------------------------------------------------------

make_cols <- function() list(
  neoipcr:::schema_col("id",   integer()),
  neoipcr:::schema_col("sex",  factor(), factor_levels = c("f", "m", "u")),
  neoipcr:::schema_col("name", character())
)

test_that("assert_schema passes when x exactly matches the compiled schema", {
  cols <- make_cols()
  expected <- neoipcr:::compile_schema(cols, dhis2_dataset_options())
  expect_invisible(
    neoipcr:::assert_schema(expected, cols, dhis2_dataset_options()))
})

test_that("assert_schema errors on column-name mismatch", {
  cols <- make_cols()
  wrong <- tibble::tibble(id = integer(), name = character())
  expect_error(
    neoipcr:::assert_schema(wrong, cols, dhis2_dataset_options()),
    "column names / order differ")
})

test_that("assert_schema errors on column-order mismatch", {
  cols <- make_cols()
  reordered <- tibble::tibble(
    sex  = factor(character(), levels = c("f", "m", "u")),
    id   = integer(),
    name = character())
  expect_error(
    neoipcr:::assert_schema(reordered, cols, dhis2_dataset_options()),
    "column names / order differ")
})

test_that("assert_schema errors on class mismatch for a non-factor column", {
  cols <- make_cols()
  wrong <- tibble::tibble(
    id   = character(),
    sex  = factor(character(), levels = c("f", "m", "u")),
    name = character())
  expect_error(
    neoipcr:::assert_schema(wrong, cols, dhis2_dataset_options()),
    "class differs")
})

test_that("assert_schema errors on level mismatch for a fixed-levels factor", {
  cols <- make_cols()
  wrong <- tibble::tibble(
    id   = integer(),
    sex  = factor(character(), levels = c("f", "m")),      # missing "u"
    name = character())
  expect_error(
    neoipcr:::assert_schema(wrong, cols, dhis2_dataset_options()),
    "factor column .* levels differ")
})

test_that("assert_schema does not check levels of data-derived factor columns", {
  cols <- list(neoipcr:::schema_col(
    "displayName", factor(),
    factor_levels = character(),
    levels_source = "data"))
  # different levels from what's declared — still passes because "data"-sourced.
  x <- tibble::tibble(
    displayName = factor(character(),
                         levels = c("Country A", "Country B")))
  expect_invisible(
    neoipcr:::assert_schema(x, cols, dhis2_dataset_options()))
})

# ---- finalize_to_schema --------------------------------------------------

test_that("finalize_to_schema selects declared columns in declaration order", {
  cols <- make_cols()
  # Input has extras and wrong order.
  x <- tibble::tibble(
    extra = 1:3,
    name  = letters[1:3],
    id    = 1:3,
    sex   = c("f", "m", "u"))
  out <- neoipcr:::finalize_to_schema(x, cols, dhis2_dataset_options())
  expect_identical(names(out), c("id", "sex", "name"))
})

test_that("finalize_to_schema drops columns not in the schema", {
  cols <- list(neoipcr:::schema_col("keep", integer()))
  x <- tibble::tibble(keep = 1:3, drop = letters[1:3])
  out <- neoipcr:::finalize_to_schema(x, cols, dhis2_dataset_options())
  expect_identical(names(out), "keep")
})

test_that("finalize_to_schema errors when a declared column is missing", {
  cols <- make_cols()
  x <- tibble::tibble(id = 1:3, name = letters[1:3])   # missing `sex`
  expect_error(
    neoipcr:::finalize_to_schema(x, cols, dhis2_dataset_options()),
    "sex")
})

test_that("finalize_to_schema applies declared factor levels", {
  cols <- make_cols()
  x <- tibble::tibble(
    id   = 1:3,
    sex  = c("f", "m", "u"),   # character, not yet factor
    name = letters[1:3])
  out <- neoipcr:::finalize_to_schema(x, cols, dhis2_dataset_options())
  expect_true(is.factor(out$sex))
  expect_identical(levels(out$sex), c("f", "m", "u"))
})

test_that("finalize_to_schema respects include_when() filtering", {
  cols <- list(
    neoipcr:::schema_col("always",    integer()),
    neoipcr:::schema_col("full_only", integer(),
                         \(opts) opts$include_country == "full")
  )
  x <- tibble::tibble(always = 1:3, full_only = 1:3, extra = letters[1:3])
  out <- neoipcr:::finalize_to_schema(
    x, cols, dhis2_dataset_options(include_country = "no"))
  expect_identical(names(out), "always")
})
