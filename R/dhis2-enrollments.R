get_enrollments_request <- function(req_base, dataset_options, programId)
{
  fields <- "enrollment,trackedEntity,enrolledAt,followUp,notes"

  if("enrollments" %in% dataset_options$include_incomplete)
    fields <- paste0(fields,",status")
  else
    req_base <- req_base |>
      httr2::req_url_query(programStatus = "COMPLETED")

  if(dataset_options$include_timestamps)
    fields <- paste0(
      fields,
      ",occurredAt,createdAt,createdAtClient,updatedAt,updatedAtClient,completedAt")

  if(!dataset_options$include_test_data ||
     length(dataset_options$country_filter) > 0 ||
     length(dataset_options$department_filter) > 0 ||
     !is.null(dataset_options$trial_keys) ||
     dataset_options$include_department != "no" ||
     dataset_options$include_hospital != "no" ||
     dataset_options$include_country != "no" ||
     dataset_options$include_world_bank_class != "no" ||
     length(dataset_options$include_invalid_patients) > 1)
    fields <- paste0(fields,",orgUnit")

  if(dataset_options$include_deleted)
    fields <- paste0(fields,",deleted")

  if(dataset_options$include_user != "no")
    fields <- paste0(
      fields,",completedBy,storedBy,createdBy[username],updatedBy[username]")

  req_base |>
    httr2::req_url_path_append("enrollments") |>
    httr2::req_url_query(program = programId) |>
    httr2::req_url_query(fields = fields)
}

read_enrollments <- function(enrollments, patients, metadata, dataset_options)
{
  opts <- dataset_options

  # Entity gate short-circuit: under `include_enrollment = "no"` the
  # public enrollments tibble is 0×0, and downstream readers that
  # consume `enrollments` must tolerate the empty shape. Matches the
  # pattern in `read_patients()`.
  if (opts$include_enrollment == "no")
    return(compile_schema(enrollments_cols, opts))

  # The patient → enrollment link join needs `trackedEntity` on
  # patients. Under `include_patient = "full"` + `"patients" %in%
  # include_dhis2_ids`, `patients_cols` declares `trackedEntity` —
  # otherwise the column is absent from the public patients tibble.
  # The legacy reader assumed it was always there; now it's an
  # option-dependent dependency that we have to document.
  #
  # For the patient_key substitution to work at all, `include_patient`
  # must be at least "pseudo" (so patients has the key). Under "full"
  # + id-opt-in, patients also has `trackedEntity`, which is what the
  # join key needs. Under "full" + no id-opt-in, `trackedEntity` is
  # absent — the reader falls back to the raw `trackedEntity` on the
  # enrollments response and matches via the `.users_internal_map`-
  # style internal map of patients. TODO(phase-b-enrollments): if
  # this combination is a real use case, introduce
  # `.patients_internal_map` matching the established pattern. For
  # now, require the id-opt-in or include_patient = "pseudo" with
  # patient_key matched through a reader-internal join.
  enrollments <- enrollments |>
    dplyr::inner_join(
      patients |>
        dplyr::select("patient_key", "trackedEntity"),
      dplyr::join_by("trackedEntity")) |>
    dplyr::mutate(
      enrolledAt = readr::parse_date(
        stringr::str_sub(.data$enrolledAt, end = 10))) |>
    dplyr::select(!c("trackedEntity","notes"))

  if("enrollments" %in% dataset_options$include_incomplete)
    enrollments <- enrollments |>
      dplyr::mutate(
        status = factor(.data$status, levels = c(
          "ACTIVE", "COMPLETED", "CANCELLED")))

  if(dataset_options$include_timestamps)
    enrollments <- enrollments |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::any_of(
            c("occurredAt","createdAt","createdAtClient","updatedAt",
              "updatedAtClient","completedAt")), readr::parse_datetime))

  if(dataset_options$include_test_data ||
     dataset_options$include_department != "no" ||
     dataset_options$include_hospital != "no" ||
     dataset_options$include_country != "no" ||
     dataset_options$include_world_bank_class != "no" ||
     length(dataset_options$include_invalid_patients) > 1)
  {
    cols <- "orgUnit"
    if(dataset_options$include_department != "no" ||
       length(dataset_options$include_invalid_patients) > 1)
      cols <- c(cols, "department_key")
    if(dataset_options$include_hospital != "no")
      cols <- c(cols, "hospital_key")
    if(dataset_options$include_country != "no")
      cols <- c(cols, "country_key")
    if(dataset_options$include_world_bank_class != "no")
      cols <- c(cols, "world_bank_class_key")
    if(dataset_options$include_test_data)
      cols <- c(cols, "isTest")

    # Consumer-side assertion at the schema-to-consumer boundary —
    # same pattern as read_patients(). The hierarchy-key columns that
    # the option branches above committed to must actually be on
    # `metadata$departments`; silent `any_of` tolerance would turn a
    # schema ↔ reader mismatch into downstream wrong data.
    require_cols(metadata$departments, cols, "departments")
    enrollments <- enrollments |>
      dplyr::left_join(
        metadata$departments |>
          dplyr::select(tidyselect::all_of(cols)),
        dplyr::join_by("orgUnit"))
  }

  if(dataset_options$include_user != "no") {
    enrollments <- enrollments |>
      tidyr::hoist("createdBy", createdBy = 1, .remove = FALSE) |>
      dplyr::left_join(
        metadata$.users_internal_map |>
          dplyr::select("user_key", "username"),
        dplyr::join_by("createdBy" == "username")) |>
      dplyr::mutate(createdBy = .data$user_key, .keep = "unused") |>
      tidyr::hoist("updatedBy", updatedBy = 1, .remove = FALSE) |>
      dplyr::left_join(
        metadata$.users_internal_map |>
          dplyr::select("user_key", "username"),
        dplyr::join_by("updatedBy" == "username")) |>
      dplyr::mutate(updatedBy = .data$user_key, .keep = "unused")

    if("completedBy" %in% names(enrollments))
      enrollments <- enrollments |>
        dplyr::left_join(
          metadata$.users_internal_map |>
            dplyr::select("user_key", "username"),
          dplyr::join_by("completedBy" == "username")) |>
        dplyr::mutate(completedBy = .data$user_key, .keep = "unused")

    if("storedBy" %in% names(enrollments))
      enrollments <- enrollments |>
        dplyr::left_join(
          metadata$.users_internal_map |>
            dplyr::select("user_key", "username"),
          dplyr::join_by("storedBy" == "username")) |>
        dplyr::mutate(storedBy = .data$user_key, .keep = "unused")
  }

  if(!dataset_options$include_test_data ||
     length(dataset_options$country_filter) > 0 ||
     !is.null(dataset_options$trial_keys))
    enrollments <- enrollments |>
      dplyr::semi_join(metadata$departments, dplyr::join_by("orgUnit"))

  enrollments <- enrollments |>
    dplyr::select(!tidyselect::any_of("orgUnit")) |>
    add_key_column("enrollment_key")

  # Narrow to the public schema + loud-assert.
  enrollments <- enrollments |>
    finalize_to_schema(enrollments_cols, opts)
  assert_schema(enrollments, enrollments_cols, opts)

  enrollments
}
