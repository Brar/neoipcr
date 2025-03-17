get_trackedEntities_request <- function(req_base, metadata_options, trackedEntityTypeId)
{
  fields <- "trackedEntity,inactive,potentialDuplicate"
  attributeFields <- "attribute,value"

  if(metadata_options$include_timestamps)
  {
    fields <- paste0(fields,",createdAt,createdAtClient,updatedAt,updatedAtClient")
    attributeFields <- paste0(attributeFields,",createdAt,updatedAt")
  }

  if(metadata_options$include_department != "no" ||
     metadata_options$include_hospital != "no" ||
     metadata_options$include_country != "no" ||
     metadata_options$include_world_bank_class != "no")
    fields <- paste0(fields,",orgUnit")

  if(metadata_options$include_deleted)
    fields <- paste0(fields,",deleted")

  if(metadata_options$include_user != "no")
  {
    fields <- paste0(fields,",createdBy[username],updatedBy[username]")
    attributeFields <- paste0(attributeFields,",storedBy")
  }

  fields <- paste0(fields,",attributes[", attributeFields, "]")

  req_base |>
    httr2::req_url_path_append("trackedEntities") |>
    httr2::req_url_query(
      trackedEntityType = trackedEntityTypeId,
      fields = fields)
}

read_patients <- function(trackedEntities, metadata, metadata_options)
{
  browser()

  patients <- trackedEntities |>
    tidyr::unnest_longer("attributes") |>
    tidyr::unnest_wider("attributes", names_sep = "_") |>
    dplyr::inner_join(
      metadata$trackedEntityAttributes |>
        dplyr::select("attribute","code","valueType","optionSet"),
      dplyr::join_by("attributes_attribute" == "attribute")) |>
    dplyr::filter(.data$code != "NEOIPC_TEA_GEST_AGE") |>
    dplyr::left_join(
      metadata$options |>
        dplyr::arrange("optionSet_code", "sortOrder") |>
        dplyr::group_by(.data$optionSet_code) |>
        dplyr::summarise(levels = list(.data$code)),
      dplyr::join_by("optionSet" == "optionSet_code"))

  if(!metadata_options$include_patient_id)
    patients <- patients |>
    dplyr::filter(.data$code != "NEOIPC_PATIENT_ID")

  patients <- patients |>
    dplyr::mutate(
      value = convert_value(.data$attributes_value, .data$valueType, .data$levels),
      code = stringr::str_extract(tolower(.data$code), "^neoipc_tea_(.+)$", group = 1),
      .keep = "unused"
    )




  # |>
  #   tidyr::pivot_wider(
  #     names_from = "attributes_code",
  #     values_from = "attributes_value") |>
  #   hoist_createdByAndupdatedBy() |>
  #   dplyr::mutate(dplyr::across(tidyselect::any_of(c(
  #     "createdAt",
  #     "createdAtClient",
  #     "updatedAt",
  #     "updatedAtClient")), readr::parse_datetime)) |>
  #   dplyr::mutate(dplyr::across(tidyselect::any_of(c(
  #     "inactive",
  #     "potentialDuplicate",
  #     "NEOIPC_TEA_MULTIPLE_BIRTH")), as.logical)) |>
  #   dplyr::mutate(
  #     NEOIPC_TEA_DELIVERY_MODE = factor(
  #       .data$NEOIPC_TEA_DELIVERY_MODE)) |>
  #   dplyr::mutate(dplyr::across(tidyselect::any_of(c(
  #     "NeoIPC_TEA_TOTAL_GESTATION_DAYS",
  #     "NEOIPC_TEA_SIBLINGS",
  #     "NEOIPC_TEA_BIRTH_WEIGHT")), as.integer))|>
  #   dplyr::left_join(
  #     metadata$users |>
  #       dplyr::select("username", "user_key"),
  #     dplyr::join_by("createdBy" == "username")) |>
  #   dplyr::mutate(createdBy = .data$user_key, .keep = "unused") |>
  #   dplyr::left_join(
  #     metadata$users |>
  #       dplyr::select("username", "user_key"),
  #     dplyr::join_by("updatedBy" == "username")) |>
  #   dplyr::mutate(updatedBy = .data$user_key, .keep = "unused") |>
  #   dplyr::left_join(
  #     metadata$departments |>
  #       dplyr::select("orgUnit", "department_key"),
  #     dplyr::join_by("orgUnit")) |>
  #   dplyr::select(!"orgUnit") |>
  #   add_key_column("patient_key")
  #
  # if("NEOIPC_TEA_SIBLINGS" %in% names(patients))
  #   patients <- patients |>
  #     dplyr::mutate(
  #       NEOIPC_TEA_SIBLINGS = tidyr::replace_na(.data$NEOIPC_TEA_SIBLINGS, 1))
  #
  # if("NEOIPC_TEA_MULTIPLE_BIRTH" %in% names(patients))
  #   patients <- patients |>
  #     dplyr::mutate(
  #       NEOIPC_TEA_MULTIPLE_BIRTH = tidyr::replace_na(
  #         .data$NEOIPC_TEA_MULTIPLE_BIRTH, FALSE))

  patients
}
