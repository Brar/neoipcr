# Tests for R/dhis2-metadata-orgunits.R — the /organisationUnits request
# builder and the org-unit readers, with the custom-attribute values. The
# offline mock in helper-dhis2-mock.R serves fixtures regardless of the
# requested `fields`, so request-shape assertions are made directly on the
# request builders. The hospital / department readers' three-mode shapes
# are covered in test-dhis2-metadata.R; this file adds the attribute path.

ou_request_fields <- function(opts)
  httr2::url_parse(
    neoipcr:::get_organisationUnit_request(
      httr2::request("https://dhis2.example.org/api"), NULL, opts)$url
  )$query$fields

attribute_fragment <- "attributeValues[attribute[id],value]"

count_fragment <- function(fields)
  lengths(regmatches(
    fields, gregexpr(attribute_fragment, fields, fixed = TRUE)))

opts_label <- function(opts)
  sprintf(
    "include_department='%s', include_hospital='%s', include_country='%s', include_custom_attributes=[%s]",
    opts$include_department, opts$include_hospital, opts$include_country,
    paste(opts$include_custom_attributes, collapse = ","))

# A raw DHIS2 attribute value as `resp_body_json()` yields it.
raw_values <- function(id, value)
  list(list(attribute = list(id = id), value = value))

# --- get_organisationUnit_request ---

test_that("the department block always requests attribute values (IsTestunit feeds test-unit detection)", {
  for (opts in iter_dataset_options(c(
    "include_department", "include_hospital", "include_country"))) {
    fields <- ou_request_fields(opts)
    expect_true(
      startsWith(fields, paste0("id,", attribute_fragment)),
      info = opts_label(opts))
  }
})

test_that("the hospital block requests attribute values only when opted in and hospitals are present", {
  for (opts in iter_dataset_options(c(
    "include_hospital", "include_country", "include_custom_attributes"))) {
    fields <- ou_request_fields(opts)
    hospital_opted <-
      "hospitals" %in% opts$include_custom_attributes &&
      opts$include_hospital != "no"

    expect_equal(
      count_fragment(fields), if (hospital_opted) 2L else 1L,
      info = opts_label(opts))
    if (hospital_opted)
      expect_match(
        fields, "parent\\[.*attributeValues\\[attribute\\[id\\],value\\]",
        info = opts_label(opts))

    chars <- strsplit(fields, "", fixed = TRUE)[[1]]
    expect_equal(sum(chars == "["), sum(chars == "]"), info = opts_label(opts))
  }
})

test_that("get_metadata_request always requests the org-unit attribute definitions", {
  for (opts in iter_dataset_options(c(
    "include_department", "include_custom_attributes"))) {
    query <- httr2::url_parse(
      neoipcr:::get_metadata_request(
        httr2::request("https://dhis2.example.org/api"),
        list(authorities = character()), opts)$url)$query
    expect_equal(
      query[["attributes:fields"]], "id,code,name,valueType",
      info = opts_label(opts))
    expect_equal(
      query[["attributes:filter"]], "organisationUnitAttribute:eq:true",
      info = opts_label(opts))
  }
})

# --- read_organisationUnit_attribute_values ---

test_that("read_organisationUnit_attribute_values unnests the list column into the raw long form", {
  processed <- tibble::tibble(
    department_key  = c(1L, 2L, 3L),
    attributeValues = list(
      c(raw_values("ATTR_A", "x"), raw_values("ATTR_B", "2024-01-31")),
      list(),
      raw_values("ATTR_A", "y")))
  result <- neoipcr:::read_organisationUnit_attribute_values(
    processed, "department_key")

  expect_named(result, c("department_key", "attribute", "value"))
  expect_equal(nrow(result), 3L)
  expect_true(is.character(result$value))
  expect_true(is.character(result$attribute))
  expect_setequal(
    result$attribute[result$department_key == 1L], c("ATTR_A", "ATTR_B"))
  expect_equal(result$value[result$department_key == 3L], "y")
  expect_false(2L %in% result$department_key)
})

test_that("read_organisationUnit_attribute_values yields the empty shape when the column is absent or every list is empty", {
  absent <- neoipcr:::read_organisationUnit_attribute_values(
    tibble::tibble(hospital_key = 1:2), "hospital_key")
  expect_named(absent, c("hospital_key", "attribute", "value"))
  expect_equal(nrow(absent), 0L)
  expect_true(is.integer(absent$hospital_key))

  empty <- neoipcr:::read_organisationUnit_attribute_values(
    tibble::tibble(hospital_key = 1:2, attributeValues = list(list(), list())),
    "hospital_key")
  expect_named(empty, c("hospital_key", "attribute", "value"))
  expect_equal(nrow(empty), 0L)
})

# --- readers: attribute values are split off, the list column stripped ---

test_that("read_organisationUnits_hospitals dedupes repeated parents that carry attribute values and strips the list column", {
  opts <- dhis2_dataset_options(
    include_hospital = "full", include_custom_attributes = "hospitals")
  parent_values <- raw_values("ATTR_A", "Hospital text")
  x <- tibble::tibble(
    id              = c("H1", "H1", "H2"),
    code            = c("HOSP_1", "HOSP_1", "HOSP_2"),
    attributeValues = list(parent_values, parent_values, list()))
  result <- neoipcr:::read_organisationUnits_hospitals(x, opts)

  expect_named(result, c("processed", "internal_map", "attribute_values"))
  expect_equal(nrow(result$processed), 2L)
  expect_false("attributeValues" %in% names(result$processed))
  expect_false("attributeValues" %in% names(result$internal_map))
  expect_equal(nrow(result$attribute_values), 1L)
  h1_key <- result$processed$hospital_key[result$processed$orgUnit == "H1"]
  expect_equal(result$attribute_values$hospital_key, h1_key)
  expect_equal(result$attribute_values$attribute, "ATTR_A")
  expect_equal(result$attribute_values$value, "Hospital text")
})

test_that("read_organisationUnits_departments returns the attribute values and a processed tibble that still finalizes without scratch", {
  opts <- dhis2_dataset_options(
    include_department = "full", include_custom_attributes = "departments")
  x <- tibble::tibble(
    id              = c("D1", "D2"),
    code            = c("DEPT_1", "DEPT_2"),
    attributeValues = list(raw_values("ATTR_A", "text"), list()))
  result <- neoipcr:::read_organisationUnits_departments(x, list(), opts)

  expect_named(result, c("processed", "internal_map", "attribute_values"))
  expect_false("attributeValues" %in% names(result$processed))
  expect_no_error(neoipcr:::finalize_to_schema(
    result$processed, neoipcr:::departments_cols, opts))
  d1_key <- result$processed$department_key[result$processed$orgUnit == "D1"]
  expect_equal(result$attribute_values$department_key, d1_key)
  expect_equal(result$attribute_values$value, "text")
})

# --- resolve_organisationUnit_attribute_values ---

definitions_map <- tibble::tibble(
  attribute = c("ATTR_A", "ATTR_D", "ATTR_F"),
  code      = c("TEST_TEXT", "TEST_DATE", "IsTestunit"),
  valueType = c("TEXT", "DATE", "TRUE_ONLY"))

typed_columns <- c(
  "value_text", "value_logical", "value_integer", "value_number",
  "value_date", "value_datetime")

test_that("resolve_organisationUnit_attribute_values maps UIDs to codes, types the values and drops what it cannot resolve", {
  values <- tibble::tibble(
    department_key = c(1L, 1L, 2L, 3L),
    attribute      = c("ATTR_A", "ATTR_D", "ATTR_UNKNOWN", "ATTR_F"),
    value          = c("some text", "2024-08-03T00:00:00.000", "dropped", "true"))
  # Department 3 did not survive the metadata narrowing.
  parents <- tibble::tibble(department_key = c(1L, 2L))

  result <- neoipcr:::resolve_organisationUnit_attribute_values(
    values, definitions_map, "department_key", parents,
    "departmentAttributeValues")

  expect_named(result, c("department_key", "attribute_code", typed_columns))
  expect_equal(nrow(result), 2L)
  expect_setequal(result$attribute_code, c("TEST_TEXT", "TEST_DATE"))
  expect_equal(
    result$value_text[result$attribute_code == "TEST_TEXT"], "some text")
  expect_equal(
    result$value_date[result$attribute_code == "TEST_DATE"],
    as.Date("2024-08-03"))
  expect_true(is.na(result$value_text[result$attribute_code == "TEST_DATE"]))
  expect_false("ATTR_UNKNOWN" %in% result$attribute_code)
})

test_that("resolve_organisationUnit_attribute_values drops rows outside `parents` before spreading, so a pruned org unit's value never warns", {
  values <- tibble::tibble(
    department_key = c(1L, 2L),
    attribute      = c("ATTR_D", "ATTR_D"),
    value          = c("2024-08-03", "not a date"))
  parents <- tibble::tibble(department_key = 1L)

  expect_no_warning(
    result <- neoipcr:::resolve_organisationUnit_attribute_values(
      values, definitions_map, "department_key", parents,
      "departmentAttributeValues"),
    class = "neoipcr_attribute_value_parse_failure")
  expect_equal(result$department_key, 1L)
  expect_equal(result$value_date, as.Date("2024-08-03"))
})

test_that("resolve_organisationUnit_attribute_values keeps every org unit when no parents are given and tolerates NULL inputs", {
  values <- tibble::tibble(
    hospital_key = c(1L, 2L),
    attribute    = c("ATTR_F", "ATTR_F"),
    value        = c("true", "true"))
  unpruned <- neoipcr:::resolve_organisationUnit_attribute_values(
    values, definitions_map, "hospital_key", NULL, "hospitalAttributeValues")
  expect_equal(nrow(unpruned), 2L)
  expect_true(all(unpruned$value_logical))

  empty <- neoipcr:::resolve_organisationUnit_attribute_values(
    NULL, NULL, "hospital_key", NULL, "hospitalAttributeValues")
  expect_equal(nrow(empty), 0L)
  expect_named(empty, c("hospital_key", "attribute_code", typed_columns))
})

# --- read_metadata_orgUnitAttributes ---

test_that("read_metadata_orgUnitAttributes reads the definitions when an entity is opted in", {
  md <- read_test_metadata(
    dataset_options = dhis2_dataset_options(
      include_department = "full", include_custom_attributes = "departments"))

  expect_named(md$orgUnitAttributes, c("code", "name", "valueType"))
  expect_equal(nrow(md$orgUnitAttributes), 6L)
  expect_true("IsTestunit" %in% md$orgUnitAttributes$code)
  # The definition without a code stays in the public list ...
  expect_true(any(is.na(md$orgUnitAttributes$code)))
  # ... but not in the code map, which has nothing to address it by.
  expect_named(
    md$.orgUnitAttributes_internal_map, c("attribute", "code", "valueType"))
  expect_equal(nrow(md$.orgUnitAttributes_internal_map), 5L)
  expect_false(any(is.na(md$.orgUnitAttributes_internal_map$code)))
})

test_that("read_metadata_orgUnitAttributes keeps the code map while the public tibble is gated off", {
  md <- read_test_metadata()
  expect_equal(ncol(md$orgUnitAttributes), 0L)
  expect_equal(nrow(md$.orgUnitAttributes_internal_map), 5L)
})

test_that("read_metadata_orgUnitAttributes yields empty shapes when the payload carries no definitions", {
  md <- read_test_metadata(
    exclude = "org_unit_attributes",
    dataset_options = dhis2_dataset_options(
      include_department = "full", include_custom_attributes = "departments"))
  expect_named(md$orgUnitAttributes, c("code", "name", "valueType"))
  expect_equal(nrow(md$orgUnitAttributes), 0L)
  expect_equal(nrow(md$.orgUnitAttributes_internal_map), 0L)
})
