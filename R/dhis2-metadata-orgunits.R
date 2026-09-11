# Creates the organisationUnits query, which we use, so that we can apply the
# withinUserHierarchy filter
get_organisationUnit_request <- function(req_base, user_info, dataset_options)
{
  # Attribute values travel only for an opted-in entity that is present. The
  # `IsTestunit` flag does not need them: it arrives as org-unit ids from its
  # own narrowed request (see `get_test_unit_attribute_request()`).
  attribute_fields <- ",attributeValues[attribute[id],value]"
  fields <- "id"
  if ("departments" %in% dataset_options$include_custom_attributes &&
      dataset_options$include_department != "no")
    fields <- paste0(fields, attribute_fields)

  if(dataset_options$include_department == "full")
    fields <- paste0(fields, ",code,displayName,displayShortName,displayDescription,openingDate,comment,geometry")
  # We need the department code for filtering or to transform the supplied exceptions
  else if(length(dataset_options$include_invalid_patients) > 1 || length(dataset_options$department_filter) > 0)
    fields <- paste0(fields, ",code")

  # Hospital attribute values are fetched only on request. The hospital block
  # exists whenever hospitals are pseudonymized or the country / World Bank
  # hierarchy is wanted, so the opt-in is checked on its own.
  hospital_attribute_fields <-
    if ("hospitals" %in% dataset_options$include_custom_attributes &&
        dataset_options$include_hospital != "no")
      attribute_fields
    else
      ""

  if(length(dataset_options$country_filter) > 0 ||
     dataset_options$include_country != "no" ||
     dataset_options$include_world_bank_class != "no")
    country_fields <- ",parent[id]]"
  else
    country_fields <- "]"

  if(dataset_options$include_hospital == "full")
    fields <- paste0(fields, paste0(",parent[id,code,displayName,displayShortName,displayDescription,comment,geometry", hospital_attribute_fields, country_fields))
  else if (dataset_options$include_hospital == "pseudo" ||
           length(dataset_options$country_filter) > 0 ||
           dataset_options$include_country != "no" ||
           dataset_options$include_world_bank_class != "no")
    fields <- paste0(fields, paste0(",parent[id", hospital_attribute_fields, country_fields))

  req_base |>
    httr2::req_url_path_append("organisationUnits") |>
    httr2::req_url_query(
      withinUserHierarchy = "true",
      fields = fields,
      filter = "organisationUnitGroups.code:eq:NEO_DEPARTMENT")
}

# The departments flagged by the `IsTestunit` custom attribute, ids only: the
# same scope as `get_organisationUnit_request()` plus DHIS2's filter on one
# attribute's value, `<attribute uid>:eq:true` (see
# `get_test_unit_attribute_ids()`).
get_test_unit_attribute_request <- function(req_base, attribute_uid)
{
  req_base |>
    httr2::req_url_path_append("organisationUnits") |>
    httr2::req_url_query(
      withinUserHierarchy = "true",
      fields = "id",
      filter = c(
        "organisationUnitGroups.code:eq:NEO_DEPARTMENT",
        paste0(attribute_uid, ":eq:true")),
      .multi = "explode")
}

# The org-unit ids of a `get_test_unit_attribute_request()` response body.
read_test_unit_attribute_ids <- function(body)
{
  units <- body$organisationUnits
  if (length(units) == 0L)
    return(character())
  vapply(units, \(unit) as.character(unit$id), character(1))
}

read_organisationUnits <- function(organisationUnits, dataset_options)
{
  department_base <- tibble::tibble(units = organisationUnits$organisationUnits) |>
    tidyr::unnest_wider(1)

  ret <- list()

  # The parent of the department is the hospital
  if("parent" %in% names(department_base)) {
    hospital_base <- tibble::tibble(hospital = department_base$parent) |>
      tidyr::unnest_wider(1)

    hospitals_result <- read_organisationUnits_hospitals(
      hospital_base, dataset_options)
    ret$hospitals               <- hospitals_result$processed
    ret$.hospitals_internal_map <- hospitals_result$internal_map
    ret$hospitalAttributeValues <- hospitals_result$attribute_values
  }

  departments_result <- read_organisationUnits_departments(
    department_base,
    ret,
    dataset_options)
  ret$departments               <- departments_result$processed
  ret$.departments_internal_map <- departments_result$internal_map
  ret$departmentAttributeValues <- departments_result$attribute_values

  ret
}

# Read hospital rows from the parent-of-department block of the
# /organisationUnits response.
#
# Returns a named list with two components:
#   * `processed`    — transformed tibble carrying every column the
#                      orchestrator needs to finish building the public
#                      hospitals tibble: `hospital_key`, `orgUnit`, any
#                      display / geometry fields under "full", and the
#                      raw `country` DHIS2 id (used by the orchestrator's
#                      country_key join). `metadata$hospitals` starts as
#                      this tibble and is narrowed to
#                      `compile_schema(hospitals_cols, opts)` in
#                      `assemble_metadata()` once the country_key
#                      join has added its column.
#   * `internal_map` — lookup subset with `hospital_key`, `orgUnit`, and
#                      `country` (when available). Used by
#                      `read_organisationUnits_departments()` for the
#                      dept→hospital join, and by
#                      `assemble_metadata()` for the country_key
#                      lookup and the WB-class inheritance path under
#                      `include_country = "no"`. Threaded through
#                      `metadata$.hospitals_internal_map` and stripped at
#                      `import_dhis2()` exit.
#   * `attribute_values` — the raw custom-attribute values keyed by
#                      `hospital_key` (see
#                      `read_organisationUnit_attribute_values()`), resolved
#                      and typed by the orchestrator.
read_organisationUnits_hospitals <- function(x, dataset_options)
{
  opts <- dataset_options
  empty_result <- list(
    processed        = tibble::tibble(),
    internal_map     = NULL,
    attribute_values = empty_attribute_values("hospital_key")
  )

  if (is.null(x) || nrow(x) < 1L)
    return(empty_result)

  # Hoist geometry when present; otherwise pad with NA under "full" so
  # the schema's longitude/latitude columns are populated either way.
  if ("geometry" %in% names(x)) {
    x <- x |>
      tidyr::hoist(
        "geometry",
        longitude = list("coordinates", 1),
        latitude  = list("coordinates", 2)) |>
      dplyr::select(!"geometry")
  } else if (opts$include_hospital == "full") {
    x <- x |> dplyr::mutate(
      longitude = NA_real_,
      latitude  = NA_real_)
  }

  # Hoist the parent reference — for hospitals, the parent is the
  # country. Present in the raw response only when country / WB-class
  # info is requested (see `get_organisationUnit_request`).
  if ("parent" %in% names(x))
    x <- x |> tidyr::hoist("parent", country = "id")

  # The parent block repeats once per department; `distinct()` collapses the
  # repeats, the `attributeValues` list column included — every copy of a
  # hospital serializes identically, so its value lists are structurally
  # equal and dedupe with the rest of the row.
  processed <- x |>
    dplyr::distinct() |>
    dplyr::relocate("orgUnit" = "id") |>
    add_key_column("hospital_key")

  attribute_values <- read_organisationUnit_attribute_values(
    processed, "hospital_key")
  processed <- processed |>
    dplyr::select(!tidyselect::any_of("attributeValues"))

  internal_map <- processed |>
    dplyr::select(tidyselect::any_of(c("hospital_key", "orgUnit", "country")))

  list(
    processed        = processed,
    internal_map     = internal_map,
    attribute_values = attribute_values)
}

read_organisationUnits_departments <- function(x, y, dataset_options) {

  # Dept → hospital join uses the orchestrator-internal hospitals map
  # (not `y$hospitals` directly), because `metadata$hospitals` is later
  # narrowed to the public schema which may strip `orgUnit` when
  # `"hospitals" %not in% include_dhis2_ids`. The map always carries
  # `hospital_key` + `orgUnit` for this join.
  if(!is.null(y$.hospitals_internal_map) &&
     "orgUnit" %in% names(y$.hospitals_internal_map)){
    x <- x |>
      tidyr::hoist("parent", orgUnit = "id") |>
      dplyr::left_join(
        y$.hospitals_internal_map |>
          dplyr::select("orgUnit", "hospital_key"),
        dplyr::join_by("orgUnit")) |>
      dplyr::select(!c("orgUnit","parent"))
  }

  cols <- names(x)
  if("openingDate" %in% cols)
    x <- x |>
      dplyr::mutate(
        openingDate =  readr::parse_date(
          stringr::str_sub(.data$openingDate, end = 10)))

  # Hoist geometry when present; otherwise pad NA under "full" so the
  # schema's longitude/latitude columns are populated either way.
  if("geometry" %in% cols) {
    x <- x |>
      tidyr::hoist(
        "geometry",
        longitude = list("coordinates", 1),
        latitude  = list("coordinates", 2)) |>
      dplyr::select(!"geometry")
  } else if (dataset_options$include_department == "full") {
    x <- x |> dplyr::mutate(
      longitude = NA_real_,
      latitude  = NA_real_)
  }

  processed <- x |>
    dplyr::relocate("orgUnit" = "id") |>
    add_key_column("department_key")

  attribute_values <- read_organisationUnit_attribute_values(
    processed, "department_key")
  processed <- processed |>
    dplyr::select(!tidyselect::any_of("attributeValues"))

  internal_map <- processed |>
    dplyr::select("department_key", "orgUnit")

  list(
    processed        = processed,
    internal_map     = internal_map,
    attribute_values = attribute_values)
}

# The empty shape of a raw attribute-values tibble: one row per (org unit,
# attribute) with the attribute's DHIS2 UID and the value as DHIS2 serializes
# it, a string.
empty_attribute_values <- function(key_col)
  tibble::tibble(
    !!key_col := integer(),
    attribute = character(),
    value = character())

# Split the `attributeValues` list column of a keyed org-unit tibble into the
# raw long form: `<key_col>`, `attribute` (the attribute UID) and `value`.
# An org unit without values serializes an empty array, which
# `unnest_longer()` drops; a response that omits the column altogether (a
# fixture, or a request that did not ask for it) yields the empty shape.
read_organisationUnit_attribute_values <- function(processed, key_col)
{
  if (!("attributeValues" %in% names(processed)))
    return(empty_attribute_values(key_col))

  values <- processed |>
    dplyr::select(tidyselect::all_of(c(key_col, "attributeValues"))) |>
    tidyr::unnest_longer("attributeValues")

  if (nrow(values) == 0L)
    return(empty_attribute_values(key_col))

  values <- values |>
    tidyr::unnest_wider("attributeValues") |>
    tidyr::unnest_wider("attribute", names_sep = "_")

  if (!("value" %in% names(values)))
    values$value <- NA_character_

  values |>
    dplyr::mutate(
      attribute = as.character(.data$attribute_id),
      value = as.character(.data$value)) |>
    dplyr::select(tidyselect::all_of(c(key_col, "attribute", "value")))
}

# Resolve raw attribute values to their public, typed shape: the attribute
# UID becomes `attribute_code` through the definitions map, rows whose
# attribute is not in that map are dropped, rows whose org unit is not among
# `parents` are dropped, and the string value is spread into the typed
# `value_*` columns by the attribute's value type. Rows not in `parents`
# are dropped before the spread, so only surviving org units can raise a
# parse-failure warning.
#
# A value without a definition is not an error: DHIS2 serializes a value even
# when the caller cannot read the attribute's definition (the contact-person
# attributes are shared privately), and such a value has no code to be
# addressed by. Only counts are logged — the value may be a person's name.
resolve_organisationUnit_attribute_values <- function(
    values, definitions_map, key_col, parents, entity_name)
{
  if (is.null(values))
    values <- empty_attribute_values(key_col)
  if (is.null(definitions_map))
    definitions_map <- tibble::tibble(
      attribute = character(), code = character(), valueType = character())

  unmatched <- values |>
    dplyr::anti_join(definitions_map, dplyr::join_by("attribute"))
  if (nrow(unmatched) > 0L)
    logger::log_debug(
      "{entity_name}: dropped {nrow(unmatched)} attribute value(s) on {dplyr::n_distinct(unmatched$attribute)} attribute(s) without a readable definition",
      namespace = "neoipcr")

  resolved <- values |>
    dplyr::inner_join(definitions_map, dplyr::join_by("attribute")) |>
    dplyr::rename(attribute_code = "code")

  if (!is.null(parents) && key_col %in% names(parents))
    resolved <- resolved |>
      dplyr::semi_join(parents, by = key_col)

  resolved |>
    spread_typed_values(
      value_col = "value", type_col = "valueType", code_col = "attribute_code") |>
    dplyr::select(
      tidyselect::all_of(c(key_col, "attribute_code")),
      tidyselect::starts_with("value_"))
}
