get_trackedEntities_request <- function(
    req_base, dataset_options, programId, trackedEntityTypeId)
{
  fields <- "trackedEntity,inactive,potentialDuplicate"
  attributeFields <- "attribute,value"

  if(dataset_options$include_unenrolled_patients)
    req_base <- req_base |>
    httr2::req_url_query(trackedEntityType = trackedEntityTypeId)
  else
  {
    req_base <- req_base |>
    httr2::req_url_query(program = programId)

    if(!("enrollments" %in% dataset_options$include_incomplete))
      req_base <- req_base |>
      httr2::req_url_query(programStatus = "COMPLETED")
  }

  if(dataset_options$include_timestamps)
  {
    fields <- paste0(fields,",createdAt,createdAtClient,updatedAt,updatedAtClient")
    attributeFields <- paste0(attributeFields,",createdAt,updatedAt")
  }

  if(!dataset_options$include_test_data ||
     length(dataset_options$country_filter) > 0 ||
     length(dataset_options$trial_keys) > 0 ||
     dataset_options$include_department != "no" ||
     dataset_options$include_hospital != "no" ||
     dataset_options$include_country != "no" ||
     dataset_options$include_world_bank_class != "no" ||
     length(dataset_options$include_invalid_patients) > 1)
    fields <- paste0(fields,",orgUnit")

  if(dataset_options$include_deleted)
    fields <- paste0(fields,",deleted")

  if(dataset_options$include_user != "no")
  {
    fields <- paste0(fields,",createdBy[username],updatedBy[username]")
    attributeFields <- paste0(attributeFields,",storedBy")
  }

  fields <- paste0(fields,",attributes[", attributeFields, "]")

  req_base |>
    httr2::req_url_path_append("trackedEntities") |>
    httr2::req_url_query(fields = fields)
}

read_patients <- function(trackedEntities, metadata, dataset_options)
{
  # ToDo: Filter patients by orgUnit but think about unenrolled patients
  patients <- trackedEntities |>
    dplyr::rename(a = .data$attributes) |>
    tidyr::unnest_longer("a") |>
    tidyr::unnest_wider("a", names_sep = "_") |>
    dplyr::inner_join(
      metadata$trackedEntityAttributes |>
        dplyr::mutate(
          a_attribute = .data$attribute,
          code = tolower(.data$code),
          .keep = "none"),
      dplyr::join_by("a_attribute")) |>
    dplyr::select(!"a_attribute") |>
    dplyr::filter(!(.data$code %in% c("neoipc_tea_gest_age","neoipc_tea_multiple_birth")))

  if(!dataset_options$include_patient_id && length(dataset_options$include_invalid_patients) <= 1)
    patients <- patients |>
      dplyr::filter(.data$code != "neoipc_patient_id")

  if(dataset_options$include_timestamps) {
    patients <- patients |>
      dplyr::mutate(dplyr::across(tidyselect::contains("At", ignore.case = FALSE), readr::parse_datetime))
  } else {
    patients <- patients |>
      dplyr::select(!tidyselect::contains("At", ignore.case = FALSE))
  }

  if(dataset_options$include_user != "no")
    patients <- patients |>
      tidyr::hoist("createdBy", createdBy = 1, .remove = FALSE) |>
      dplyr::left_join(
        metadata$users |>
          dplyr::select("user_key", "username"),
        dplyr::join_by("createdBy" == "username")) |>
      dplyr::mutate(createdBy = .data$user_key, .keep = "unused") |>
      tidyr::hoist("updatedBy", updatedBy = 1, .remove = FALSE) |>
      dplyr::left_join(
        metadata$users |>
          dplyr::select("user_key", "username"),
        dplyr::join_by("updatedBy" == "username")) |>
      dplyr::mutate(updatedBy = .data$user_key, .keep = "unused") |>
      dplyr::left_join(
        metadata$users |>
          dplyr::select("user_key", "username"),
        dplyr::join_by("a_storedBy" == "username")) |>
      dplyr::mutate(a_storedBy = .data$user_key, .keep = "unused")

  if(!dataset_options$include_test_data ||
     length(dataset_options$country_filter) > 0 ||
     length(dataset_options$trial_keys) > 0)
    patients <- patients |>
      dplyr::semi_join(metadata$departments, dplyr::join_by("orgUnit"))

  patients <- patients |>
    tidyr::pivot_wider(
      names_from = "code",
      values_from = tidyselect::starts_with("a_"),
      names_glue = "{code}_{.value}",
      names_vary = "slowest") |>
    dplyr::rename_with(
      ~stringr::str_replace(
        .x,
        "^neoipc(?:_tea)?_(.+)_a(.+)*?(?:_value)?$",
        "\\1\\2"),
      tidyselect::contains("_a_")) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::any_of(c(
          "siblings",
          "total_gestation_days",
          "birth_weight")), as.integer),
      dplyr::across(
        tidyselect::any_of("sex"), ~factor(.x, levels = c("f","m","u"))),
      dplyr::across(
        tidyselect::any_of("delivery_mode"), ~factor(.x, levels = c("1","2","3")))
    )

  if(dataset_options$include_test_data ||
     dataset_options$include_department != "no" ||
     dataset_options$include_hospital != "no" ||
     dataset_options$include_country != "no" ||
     dataset_options$include_world_bank_class != "no" ||
     length(dataset_options$include_invalid_patients) > 1)
  {
    patients <- patients |>
      dplyr::left_join(
        metadata$departments |>
          dplyr::select(
            tidyselect::any_of(c(
              "orgUnit","is_test","department_key","hospital_key","country_key",
              "world_bank_class_key"))),
        dplyr::join_by("orgUnit")) |>
      dplyr::select(!"orgUnit")
  }

  patients <- patients |>
    add_key_column("patient_key") |>
    ensure_patients_schema(dataset_options)

  # Default eligibility criteria filter
  if (!dataset_options$include_ineligible_patients)
    patients <- patients |>
      dplyr::filter(.data$birth_weight < 1500L | .data$total_gestation_days < 224)

  # Birth weight filters
  if (is.integer(dataset_options$birth_weight_from))
    patients <- patients |>
    dplyr::filter(.data$birth_weight >= dataset_options$birth_weight_from)

  if (is.integer(dataset_options$birth_weight_to))
    patients <- patients |>
    dplyr::filter(.data$birth_weight <= dataset_options$birth_weight_to)

  # Gestational age filters
  if (is.integer(dataset_options$gestational_age_from)) {
    gestation_days_from <- 7 * dataset_options$gestational_age_from
    patients <- patients |>
    dplyr::filter(.data$total_gestation_days >= gestation_days_from)
  }

  if (is.integer(dataset_options$gestational_age_to)) {
    gestation_days_to <- 7 * dataset_options$gestational_age_to + 6
    patients <- patients |>
    dplyr::filter(.data$total_gestation_days <= gestation_days_to)
  }

  return(patients)
}
