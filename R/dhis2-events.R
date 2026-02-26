get_events_request <- function(req_base, dataset_options, programId) {
  fields <- "event,programStage,enrollment,trackedEntity,occurredAt,followup"
  dataValueFields <- "dataElement,value"

  if("events" %in% dataset_options$include_incomplete)
    fields <- paste0(fields,",status")
  else
    req_base <- req_base |>
    httr2::req_url_query(status = "COMPLETED")

  if(!("enrollments" %in% dataset_options$include_incomplete))
    req_base <- req_base |>
    httr2::req_url_query(programStatus = "COMPLETED")

  if(dataset_options$include_timestamps)
  {
    fields <- paste0(
      fields,
      ",scheduledAt,completedAt,createdAt,createdAtClient,updatedAt,updatedAtClient")

    dataValueFields <- paste0(dataValueFields,",createdAt,updatedAt")
  }

  if(!dataset_options$include_test_data ||
     length(dataset_options$country_filter) > 0 ||
     length(dataset_options$trial_keys) > 0 ||
     dataset_options$include_department != "no" ||
     dataset_options$include_hospital != "no" ||
     dataset_options$include_country != "no" ||
     dataset_options$include_world_bank_class != "no")
    fields <- paste0(fields,",orgUnit")

  if(dataset_options$include_deleted)
    fields <- paste0(fields,",deleted")

  if("events" %in% dataset_options$include_notes)
    fields <- paste0(fields,",notes")

  if(dataset_options$include_user != "no")
  {
    fields <- paste0(fields,",storedBy,completedBy,createdBy[username],updatedBy[username]")
    dataValueFields <- paste0(dataValueFields,",storedBy,createdBy[username],updatedBy[username]")
  }

  fields <- paste0(fields,",dataValues[", dataValueFields, "]")

  req_base |>
    httr2::req_url_path_append("events") |>
    httr2::req_url_query(program = programId) |>
    httr2::req_url_query(fields = fields)
}

read_events <- function(events, enrollments, patients, metadata, dataset_options) {
  events <- events |>
    dplyr::inner_join(
      metadata$eventTypes |>
        dplyr::select("event_type_key", "programStage"),
      dplyr::join_by("programStage")) |>
    dplyr::inner_join(
      enrollments |>
        dplyr::select("enrollment_key", "enrollment"),
      dplyr::join_by("enrollment")) |>
    dplyr::inner_join(
      patients |>
        dplyr::select("patient_key", "trackedEntity"),
      dplyr::join_by("trackedEntity")) |>
    dplyr::mutate(
      occurredAt = readr::parse_date(
        stringr::str_sub(.data$occurredAt, end = 10)))

  if(dataset_options$include_world_bank_class != "no")
    events <- events |>
      dplyr::inner_join(
        metadata$departments |>
          dplyr::select("orgUnit", "department_key", "hospital_key"),
        dplyr::join_by("orgUnit")) |>
      # Everything above department may not exist for test units
      # so we need left joins here
      dplyr::left_join(
        metadata$hospitals |>
          dplyr::select("hospital_key","country_key"),
        dplyr::join_by("hospital_key")) |>
      dplyr::left_join(
        metadata$countries |>
          dplyr::select("country_key", "world_bank_class_key"),
        dplyr::join_by("country_key"))
  else if(dataset_options$include_country != "no")
    events <- events |>
      dplyr::inner_join(
        metadata$departments |>
          dplyr::select("orgUnit", "department_key", "hospital_key"),
        dplyr::join_by("orgUnit")) |>
      # Everything above department may not exist for test units
      # so we need left joins here
      dplyr::left_join(
        metadata$hospitals |>
          dplyr::select("hospital_key","country_key"),
        dplyr::join_by("hospital_key"))
  else if(dataset_options$include_test_data ||
          dataset_options$include_hospital != "no" ||
          dataset_options$include_department != "no")
  {
    fields <- "orgUnit"

    if(dataset_options$include_hospital != "no")
      fields <- c(fields, "department_key", "hospital_key")
    else if(dataset_options$include_department != "no")
      fields <- c(fields, "department_key")

    if(dataset_options$include_test_data)
      fields <- c(fields, "isTest")

    events <- events |>
      dplyr::inner_join(
        metadata$departments |>
          dplyr::select(tidyselect::all_of(fields)),
        dplyr::join_by("orgUnit"))
  }

  if("events" %in% dataset_options$include_incomplete)
    events <- events |>
      dplyr::mutate(
        status = factor(
          .data$status,
          levels = c(
            "ACTIVE", "COMPLETED", "VISITED", "SCHEDULE", "OVERDUE", "SKIPPED"))
      )

  if(!dataset_options$include_test_data ||
     length(dataset_options$country_filter) > 0 ||
     length(dataset_options$trial_keys) > 0)
    events <- events |>
      dplyr::semi_join(metadata$departments, dplyr::join_by("orgUnit"))

  events |>
    dplyr::select(
      tidyselect::any_of(
        c(
          "event",
          "occurredAt",
          "status",
          "event_type_key",
          "enrollment_key",
          "patient_key",
          "department_key",
          "hospital_key",
          "country_key",
          "world_bank_class_key"))) |>
    add_key_column("event_key")
}

read_events2 <- function(events, enrollments, patients, metadata, dataset_options) {
  events <- events |>
    dplyr::inner_join(
      metadata$eventTypes |>
        dplyr::select("event_type_key", "programStage"),
      dplyr::join_by("programStage")) |>
    dplyr::inner_join(
      enrollments |>
        dplyr::select("enrollment_key", "enrollment"),
      dplyr::join_by("enrollment")) |>
    dplyr::inner_join(
      patients |>
        dplyr::select("patient_key", "trackedEntity"),
      dplyr::join_by("trackedEntity")) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::any_of(c("scheduledAt","occurredAt")),
        ~readr::parse_date(stringr::str_sub(.x, end = 10))
      )
    )|>
    dplyr::select(!c("programStage","enrollment","trackedEntity")) |>
    dplyr::rename("followUp" = "followup")

  if(!("events" %in% dataset_options$include_dhis2_ids))
    events <- events |>
      dplyr::select(!"event")

  if("events" %in% dataset_options$include_incomplete)
    events <- events |>
      dplyr::mutate(
        status = factor(
          .data$status,
          levels = c(
            "ACTIVE", "COMPLETED", "VISITED", "SCHEDULE", "OVERDUE", "SKIPPED"))
      )


  # For some reason we don't have createdAtClient and updatedAtClient in our
  # dataset although the DHIS2 docs claim that they should be available and we
  # request them via the API.
  # Maybe some bug?
  # Keep the code to process them hoping that they to magically appear at some
  # point.
  if(dataset_options$include_timestamps)
    events <- events |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::any_of(
            c("createdAt","createdAtClient","updatedAt",
              "updatedAtClient","completedAt")), readr::parse_datetime))

  if(dataset_options$include_user != "no") {
    events <- events |>
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
        dplyr::join_by("storedBy" == "username")) |>
      dplyr::mutate(storedBy = .data$user_key, .keep = "unused")

    # For some reason we don't have completedBy in our dataset although
    # the DHIS2 docs claim that it should be available and we request it
    # via the API.
    # Maybe some bug?
    # Keep the code to process it hoping it to magically appear at some point.
    if ("completedBy" %in% rlang::names2(events))
      events <- events |>
        dplyr::left_join(
          metadata$users |>
            dplyr::select("user_key", "username"),
          dplyr::join_by("completedBy" == "username")) |>
        dplyr::mutate(completedBy = .data$user_key, .keep = "unused")
  }

  if(!dataset_options$include_test_data ||
     length(dataset_options$country_filter) > 0 ||
     length(dataset_options$trial_keys) > 0)
    events <- events |>
      dplyr::semi_join(metadata$departments, dplyr::join_by("orgUnit"))

  if(dataset_options$include_world_bank_class != "no" ||
     dataset_options$include_country != "no" ||
     dataset_options$include_test_data ||
     dataset_options$include_hospital != "no" ||
     dataset_options$include_department != "no")
  {
    events <- events |>
      dplyr::inner_join(
        metadata$departments |>
          dplyr::select(
            c(
              "orgUnit",
              tidyselect::any_of(
                c("is_test","department_key","hospital_key","country_key",
                  "world_bank_class_key")))),
        dplyr::join_by("orgUnit"))
  }

  events <- events |>
    dplyr::select(!"orgUnit") |>
    add_key_column("event_key")

  notes <- events |>
    dplyr::select(c("event_key","notes")) |>
    read_event_notes(metadata$users, dataset_options)

  admissionData <-  events |>
    expand_event_data(
      "adm",
      metadata$users,
      metadata$dataElements,
      dataset_options,
      "NEOIPC_ADMISSION_",
      "NEOIPC_ADMISSION_LOS") |>
    read_admission_event_data(dataset_options)

  surveillanceEndData <-  events |>
    expand_event_data(
      "end",
      metadata$users,
      metadata$dataElements,
      dataset_options,
      "NEOIPC_SURVEILLANCE_END_",
      c(
        paste0("NEOIPC_SURVEILLANCE_END_AB_SUBST_0", seq(1,9)),
        paste0("NEOIPC_SURVEILLANCE_END_AB_SUBST_0", seq(1,9), "_DAYS"))) |>
    read_surveillance_end_event_data(dataset_options)

  sepsisData <-  events |>
    expand_event_data(
      "bsi",
      metadata$users,
      metadata$dataElements,
      dataset_options,
      "NEOIPC_BSI_",
      c(
        paste0("NEOIPC_BSI_PATHOGEN_", seq(1,3)),
        paste0("NEOIPC_BSI_PATHOGEN_", seq(1,3), "_3GCR"),
        paste0("NEOIPC_BSI_PATHOGEN_", seq(1,3), "_MRSA"),
        paste0("NEOIPC_BSI_PATHOGEN_", seq(1,3), "_VRE"),
        paste0("NEOIPC_BSI_PATHOGEN_", seq(1,3), "_CAR"),
        paste0("NEOIPC_BSI_PATHOGEN_", seq(1,3), "_COR"),
        paste0("NEOIPC_BSI_PATHOGEN_", seq(1,3), "_NAME"),
        paste0("NEOIPC_BSI_PATHOGEN_", seq(1,3), "_MULTIPLE"),
        paste0("NEOIPC_BSI_PATHOGEN_", seq(1,3), "_SOURCE"))) |>
    read_sepsis_event_data(dataset_options)

  necData <-  events |>
    expand_event_data(
      "nec",
      metadata$users,
      metadata$dataElements,
      dataset_options,
      "NEOIPC_NEC_",
      c(
        paste0("NEOIPC_NEC_SEC_BSI_PATHOGEN_", seq(1,3)),
        paste0("NEOIPC_NEC_SEC_BSI_PATHOGEN_", seq(1,3), "_3GCR"),
        paste0("NEOIPC_NEC_SEC_BSI_PATHOGEN_", seq(1,3), "_MRSA"),
        paste0("NEOIPC_NEC_SEC_BSI_PATHOGEN_", seq(1,3), "_VRE"),
        paste0("NEOIPC_NEC_SEC_BSI_PATHOGEN_", seq(1,3), "_CAR"),
        paste0("NEOIPC_NEC_SEC_BSI_PATHOGEN_", seq(1,3), "_COR"),
        paste0("NEOIPC_NEC_SEC_BSI_PATHOGEN_", seq(1,3), "_NAME"))) |>
    read_nec_event_data(dataset_options)

  pneumoniaData <-  events |>
    expand_event_data(
      "hap",
      metadata$users,
      metadata$dataElements,
      dataset_options,
      "NEOIPC_HAP_",
      c(
        paste0("NEOIPC_HAP_PATHOGEN_", seq(1,3)),
        paste0("NEOIPC_HAP_PATHOGEN_", seq(1,3), "_3GCR"),
        paste0("NEOIPC_HAP_PATHOGEN_", seq(1,3), "_MRSA"),
        paste0("NEOIPC_HAP_PATHOGEN_", seq(1,3), "_VRE"),
        paste0("NEOIPC_HAP_PATHOGEN_", seq(1,3), "_CAR"),
        paste0("NEOIPC_HAP_PATHOGEN_", seq(1,3), "_COR"),
        paste0("NEOIPC_HAP_PATHOGEN_", seq(1,3), "_NAME"),
        paste0("NEOIPC_HAP_PATHOGEN_", seq(1,3), "_SOURCE"),
        paste0("NEOIPC_HAP_SEC_BSI_PATHOGEN_", seq(1,3)),
        paste0("NEOIPC_HAP_SEC_BSI_PATHOGEN_", seq(1,3), "_3GCR"),
        paste0("NEOIPC_HAP_SEC_BSI_PATHOGEN_", seq(1,3), "_MRSA"),
        paste0("NEOIPC_HAP_SEC_BSI_PATHOGEN_", seq(1,3), "_VRE"),
        paste0("NEOIPC_HAP_SEC_BSI_PATHOGEN_", seq(1,3), "_CAR"),
        paste0("NEOIPC_HAP_SEC_BSI_PATHOGEN_", seq(1,3), "_COR"),
        paste0("NEOIPC_HAP_SEC_BSI_PATHOGEN_", seq(1,3), "_NAME"))) |>
    read_pneumonia_event_data(dataset_options)

  surgeryData <-  events |>
    expand_event_data(
      "pro",
      metadata$users,
      metadata$dataElements,
      dataset_options,
      "NEOIPC_SURGERY_") |>
    read_surgery_event_data(dataset_options)

  ssiData <-  events |>
    expand_event_data(
      "ssi",
      metadata$users,
      metadata$dataElements,
      dataset_options,
      "NEOIPC_SSI_") |>
    read_ssi_event_data(dataset_options)

  events <- events |>
    dplyr::select(!c("notes","dataValues")) |>
    ensure_events_schema(dataset_options)

  list(
    events = events,
    notes = notes,
    admissionData = admissionData,
    surveillanceEndData = surveillanceEndData,
    sepsisData = sepsisData,
    necData = necData,
    pneumoniaData = pneumoniaData,
    surgeryData = surgeryData,
    ssiData = ssiData#,
    # infectiousAgentFindings = infectiousAgentFindings,
    # substanceDays = substanceDays
  )
}

expand_event_data <- function (x, event_type_key, users, dataElements, dataset_options, remove_prefixes, remove_codes = NULL) {
  x <- x |>
    dplyr::filter(.data$event_type_key == !!event_type_key) |>
    dplyr::select(c("event_key","dataValues")) |>
    tidyr::unnest_longer("dataValues") |>
    tidyr::unnest_wider("dataValues")|>
    dplyr::inner_join(
      dataElements |>
        dplyr::select("dataElement","code"),
      dplyr::join_by("dataElement")) |>
    dplyr::select(!"dataElement") |>
    dplyr::filter(!(.data$code %in% remove_codes)) |>
    dplyr::mutate(
      code = tolower(
        stringr::str_replace(
          .data$code,
          paste0("^(?:", paste0(remove_prefixes, collapse = "|"),")(.+)$", collapse = ""),
          "\\1"))
    )

  if(dataset_options$include_timestamps) {
    x <- x |>
      dplyr::mutate(dplyr::across(tidyselect::contains("At", ignore.case = FALSE), readr::parse_datetime))
  } else {
    x <- x |>
      dplyr::select(!tidyselect::contains("At", ignore.case = FALSE))
  }

  if(dataset_options$include_user != "no") {
    x <- x |>
    tidyr::hoist("createdBy", createdBy = 1, .remove = FALSE) |>
    dplyr::left_join(
      users |>
        dplyr::select("user_key", "username"),
      dplyr::join_by("createdBy" == "username")) |>
    dplyr::mutate(createdBy = .data$user_key, .keep = "unused") |>
    tidyr::hoist("updatedBy", updatedBy = 1, .remove = FALSE) |>
    dplyr::left_join(
      users |>
        dplyr::select("user_key", "username"),
      dplyr::join_by("updatedBy" == "username")) |>
    dplyr::mutate(updatedBy = .data$user_key, .keep = "unused")

    # For some reason we don't have storedBy in our dataset although
    # the DHIS2 docs claim that it should be available and we request it
    # via the API.
    # Maybe some bug?
    # Keep the code to process it hoping it to magically appear at some point.
    if ("completedBy" %in% rlang::names2(x))
      x <- x |>
        dplyr::left_join(
          users |>
            dplyr::select("user_key", "username"),
          dplyr::join_by("storedBy" == "username")) |>
        dplyr::mutate(a_storedBy = .data$user_key, .keep = "unused")
  }

  x |>
    tidyr::pivot_wider(
      names_from = "code",
      values_from = !c("event_key","code"),
      names_glue = "{code}_{.value}",
      names_vary = "slowest") |>
    dplyr::rename_with(
      ~stringr::str_replace(
        .x,
        "^(.+)_value$",
        "\\1"),
      tidyselect::ends_with("_value"))
}

read_event_notes <- function(x, users, dataset_options) {
  if(!("events" %in% dataset_options$include_notes))
    return(
      tibble::tibble() |>
        ensure_event_notes_schema(dataset_options)
    )

  x <- x |>
    tidyr::unnest_longer("notes") |>
    tidyr::unnest_wider("notes")

  if(dataset_options$include_timestamps)
    x <- x |>
    dplyr::mutate(storedAt = readr::parse_datetime(.data$storedAt))

  if(dataset_options$include_user != "no")
    x <- x |>
    tidyr::hoist("createdBy", createdBy = 1, .remove = FALSE) |>
    dplyr::left_join(
      users |>
        dplyr::select("user", "user_key"),
      dplyr::join_by("createdBy" == "user")) |>
    dplyr::mutate(createdBy = .data$user_key, .keep = "unused") |>
    dplyr::left_join(
      users |>
        dplyr::select("username", "user_key"),
      dplyr::join_by("storedBy" == "username")) |>
    dplyr::mutate(storedBy = .data$user_key, .keep = "unused")

  if(!("notes" %in% dataset_options$include_dhis2_ids))
    x <- x |>
    dplyr::select(!"note")

  x |>
    ensure_event_notes_schema(dataset_options)
}

read_admission_event_data <- function(x, dataset_options) {
  x |>
    dplyr::mutate(
      type = factor(.data$type, levels = c("1","2","3")),
      dol = as.integer(.data$dol)
    ) |>
    ensure_admission_event_data_schema(dataset_options)
}

read_surveillance_end_event_data <- function(x, dataset_options) {
  x |>
    dplyr::mutate(
      reason = factor(.data$reason, levels = c("1","2")),
      dplyr::across(tidyselect::ends_with("_days"), as.integer)
    ) |>
    ensure_surveillance_end_event_data_schema(dataset_options)
}

read_sepsis_event_data <- function(x, dataset_options) {
  x |>
    dplyr::mutate(
      dev_ass = factor(.data$dev_ass, levels = c("0","1","2")),
      dplyr::across(c("los","dol"), as.integer),
      dplyr::across(!c("event_key","dev_ass","los","dol"), as.logical)
    ) |>
    ensure_sepsis_event_data_schema(dataset_options)
}

read_nec_event_data <- function(x, dataset_options) {
  x |>
    dplyr::mutate(
      secondary_bsi = factor(.data$secondary_bsi, levels = c("1","0","-1")),
      dplyr::across(c("los","dol"), as.integer),
      dplyr::across(!c("event_key","secondary_bsi","los","dol"), as.logical)
    ) |>
    ensure_nec_event_data_schema(dataset_options)
}

read_pneumonia_event_data <- function(x, dataset_options) {
  x |>
    dplyr::rename("dev_ass" = "device_association") |>
    dplyr::mutate(
      dev_ass = factor(.data$dev_ass, levels = c("0","1","2")),
      dplyr::across(c("microbiological_test_result","secondary_bsi"), ~factor(.x, levels = c("1","0","-1"))),
      dplyr::across(c("los","dol"), as.integer),
      dplyr::across(!c("event_key","dev_ass","microbiological_test_result","secondary_bsi","los","dol"), as.logical)
    ) |>
    ensure_pneumonia_event_data_schema(dataset_options)
}

read_surgery_event_data <- function(x, dataset_options) {
  browser()
  x |>
    dplyr::mutate(
      reason = factor(.data$reason, levels = c("1","2")),
      dplyr::across(tidyselect::ends_with("_days"), as.integer)
    ) |>
    ensure_surgery_event_data_schema(dataset_options)
}

read_ssi_event_data <- function(x, dataset_options) {
  browser()
  x |>
    dplyr::mutate(
      reason = factor(.data$reason, levels = c("1","2")),
      dplyr::across(tidyselect::ends_with("_days"), as.integer)
    ) |>
    ensure_ssi_event_data_schema(dataset_options)
}

read_event_details <- function(events, processed_events, metadata, dataset_options) {
  events <- events |>
    dplyr::inner_join(
      processed_events |>
        dplyr::select("event_key", "event"),
      dplyr::join_by("event"))

  if(dataset_options$include_user != "no")
    events <- events |>
      tidyr::hoist("createdBy", createdBy = 1, .remove = FALSE) |>
      tidyr::hoist("updatedBy", updatedBy = 1, .remove = FALSE) |>
      dplyr::left_join(
        metadata$users |>
          dplyr::select("user_key", "username"),
        dplyr::join_by("storedBy" == "username")) |>
      dplyr::mutate(storedBy = .data$user_key, .keep = "unused") |>
      dplyr::left_join(
        metadata$users |>
          dplyr::select("user_key", "username"),
        dplyr::join_by("createdBy" == "username")) |>
      dplyr::mutate(createdBy = .data$user_key, .keep = "unused") |>
      dplyr::left_join(
        metadata$users |>
          dplyr::select("user_key", "username"),
        dplyr::join_by("updatedBy" == "username")) |>
      dplyr::mutate(updatedBy = .data$user_key, .keep = "unused") |>
      dplyr::mutate(dplyr::across(dplyr::ends_with("At"), readr::parse_datetime))

  events |>
    dplyr::select(
      tidyselect::any_of(
        c(
          "event_key","scheduledAt","createdAt","updatedAt","completedAt",
          "storedBy","createdBy","updatedBy","followup","deleted")))
}

read_event_notes_old <- function(events, processed_events, metadata, dataset_options) {
  if(!("events" %in% dataset_options$include_notes))
    return(NULL)

  events <- events |>
    dplyr::inner_join(
      processed_events |>
        dplyr::select("event_key", "event"),
      dplyr::join_by("event")) |>
    dplyr::select("event_key", "notes") |>
    tidyr::unnest_longer("notes") |>
    tidyr::unnest_wider("notes")

  if(dataset_options$include_user != "no")
    events <- events |>
    tidyr::hoist("createdBy", createdBy = 1, .remove = FALSE) |>
    dplyr::left_join(
      metadata$users |>
        dplyr::select("user", "user_key"),
      dplyr::join_by("createdBy" == "user")) |>
    dplyr::mutate(createdBy = .data$user_key, .keep = "unused") |>
    dplyr::left_join(
      metadata$users |>
        dplyr::select("username", "user_key"),
      dplyr::join_by("storedBy" == "username")) |>
    dplyr::mutate(storedBy = .data$user_key, .keep = "unused") |>
    dplyr::mutate(storedAt = readr::parse_datetime(.data$storedAt))

  events |>
    dplyr::select(!"note")
}

read_event_data <- function(events, processed_events, metadata, dataset_options, event_type_key) {
  events <- events |>
    dplyr::select("event", "dataValues") |>
    dplyr::inner_join(
      processed_events |>
        dplyr::filter(.data$event_type_key == !!event_type_key) |>
        dplyr::select("event", "event_key"),
      dplyr::join_by("event")) |>
    tidyr::unnest_longer("dataValues") |>
    tidyr::unnest_wider("dataValues")

  if(dataset_options$include_user != "no") {
    events <- events |>
      tidyr::hoist("createdBy", createdBy = 1, .remove = FALSE) |>
      dplyr::left_join(
        metadata$users |>
          dplyr::select("username", "user_key"),
        dplyr::join_by("createdBy" == "username")) |>
      dplyr::mutate(
        createdBy = .data$user_key,
        .keep = "unused")
  }

  if(dataset_options$include_timestamps) {
    events <- events |>
      dplyr::mutate(
        createdAt = readr::parse_datetime(.data$createdAt),
        updatedAt = readr::parse_datetime(.data$updatedAt),
        .keep = "unused")
  }

  if(nrow(events) > 0) {
    events <- events |>
      dplyr::inner_join(
        metadata$dataElements |>
          dplyr::select("dataElement","code","valueType","optionSet"),
        dplyr::join_by("dataElement")) |>
      dplyr::left_join(
        metadata$options |>
          dplyr::arrange(.data$optionSet_code, .data$sortOrder) |>
          dplyr::group_by(.data$optionSet_code) |>
          dplyr::summarise(levels = list(.data$code)),
        dplyr::join_by("optionSet" == "optionSet_code")) |>
      dplyr::mutate(
        value = convert_value(.data$value, .data$valueType, .data$levels),
        code = stringr::str_extract(tolower(.data$code), "^neoipc_(admission|surveillance_end|bsi|nec|hap|ssi|surgery)_(.+)$", group = 2),
        .keep = "unused"
      ) |>
      dplyr::select(!c("event","dataElement","optionSet"))

    if(event_type_key == "adm")
      events <- events |>
      dplyr::filter(.data$code != "los")
    else if(event_type_key == "end")
      events <- events |>
      dplyr::filter(stringr::str_starts(.data$code, "ab_subst_", negate = TRUE))
    else if(event_type_key == "bsi")
      events <- events |>
      dplyr::filter(stringr::str_starts(.data$code, "pathogen_\\d", negate = TRUE))
    else if(event_type_key %in% c("nec","hap","ssi"))
      events <- events |>
      dplyr::filter(stringr::str_starts(.data$code, "(sec_bsi_)?pathogen_\\d", negate = TRUE))

    events <- events |>
      tidyr::pivot_wider(
        names_from = "code",
        values_from = !c("code","event_key"),
        names_glue = "{code}_{.value}",
        names_vary = "slowest") |>
      tidyr::unnest_longer(dplyr::ends_with("value"), keep_empty = TRUE) |>
      dplyr::relocate(dplyr::ends_with("value"), .after = "event_key") |>
      dplyr::rename_with(~ stringr::str_extract(.x, "^(.+)_value$", 1),
                         tidyselect::ends_with("_value"))
  }

  if(event_type_key == "adm")
    events <- events |>
    dplyr::select(
      tidyselect::all_of(
        c("event_key","type","dol", sort(tidyselect::peek_vars()))))
  else if(event_type_key == "end")
    events <- events |>
    dplyr::mutate(vs_days = .data$inv_days + .data$niv_days) |>
    dplyr::select(
      tidyselect::all_of(
        c("event_key","reason","patient_days","cvc_days","pvc_days","vs_days",
          "inv_days","niv_days","ab_days","human_milk_days",
          "kangaroo_care_days","probiotic_days", sort(tidyselect::peek_vars()))))
  else if(event_type_key == "bsi")
    events <- events |>
    dplyr::select(
      tidyselect::any_of(
        c("event_key","dev_ass","los","dol", sort(tidyselect::peek_vars()))))
  else if(event_type_key == "nec")
    events <- events |>
    dplyr::rename(tidyselect::any_of(c(sec_bsi = "secondary_bsi"))) |>
    dplyr::select(
      tidyselect::any_of(
        c("event_key","los","dol","sec_bsi", sort(tidyselect::peek_vars()))))
  else if(event_type_key == "hap")
    events <- events |>
    dplyr::rename(tidyselect::any_of(c(
      dev_ass = "device_association",
      sec_bsi = "secondary_bsi"))) |>
    dplyr::select(
      tidyselect::any_of(
        c("event_key","dev_ass","los","dol","sec_bsi","microbiological_test_result", sort(tidyselect::peek_vars()))))
  else if(event_type_key == "ssi")
    events <- events |>
    dplyr::select(
      tidyselect::any_of(
        c("event_key","los","dol","infection_type","sec_bsi","organisms_superf",
          "organisms_organ", sort(tidyselect::peek_vars()))))
  else if(event_type_key == "pro")
    events <- events |>
    dplyr::select(
      tidyselect::any_of(
        c("event_key","los","dol","procedure_description","main_procedure_code",
          "side_procedure_code_1","side_procedure_code_2","asa_score",
          "wound_class","duration","infection_signs", sort(tidyselect::peek_vars()))))

  events
}

read_infectious_agent_findings <- function(events_raw, processed_events, metadata, dataset_options) {
  events_raw |>
    dplyr::select("event", "dataValues") |>
    dplyr::inner_join(
      processed_events |>
        dplyr::filter(.data$event_type_key %in% c("bsi","nec","ssi","hap")) |>
        dplyr::select("event", "event_key", "event_type_key"),
      dplyr::join_by("event")) |>
    tidyr::unnest_longer("dataValues") |>
    tidyr::unnest_wider("dataValues") |>
    dplyr::inner_join(
      metadata$dataElements |>
        dplyr::select("dataElement", "code"),
      dplyr::join_by("dataElement")) |>
    dplyr::filter(stringr::str_detect(.data$code, "PATHOGEN_\\d")) |>
    dplyr::mutate(
      type = factor(tolower(stringr::str_replace(.data$code, "^.+(PATHOGEN)_\\d+(.*)$", "\\1\\2"))),
      index = as.integer(stringr::str_replace(.data$code, "^.+PATHOGEN_(\\d+).*$", "\\1")),
      secondary_bsi = stringr::str_detect(.data$code, "_SEC_BSI_"),
      .keep = "unused"
    ) |>
    dplyr::select("event_key","event_type_key","type","index","secondary_bsi",
                  "value") |>
    tidyr::pivot_wider(names_from = "type", values_from = "value", names_sort = TRUE) |>
    dplyr::rename_with(~ stringr::str_extract(.x, "^pathogen_(.+)$", 1), .cols = tidyselect::starts_with("pathogen_")) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::any_of(
          c("3gcr","car","cor","mrsa","vre")),
        ~ factor(dplyr::case_match(as.integer(.x), 0 ~ "no", 1 ~ "yes", -1 ~ "not_tested"), levels = c("no","yes","not_tested"))),
      dplyr::across(tidyselect::any_of(c("multiple")), as.logical),
      pathogen_key = as.integer(.data$pathogen),
      source = factor(dplyr::case_when(
        as.character(.data$event_type_key) == "bsi" &
          as.integer(.data$source) == 1L ~ "B",
        as.character(.data$event_type_key) == "bsi" &
          as.integer(.data$source) == 2L ~ "C",
        as.character(.data$event_type_key) == "bsi" &
          as.integer(.data$source) == 3L ~ "B+C",
        as.character(.data$event_type_key) == "hap" &
          as.integer(.data$source) == 1L ~ "U",
        as.character(.data$event_type_key) == "hap" &
          as.integer(.data$source) == 2L ~ "L",
        as.character(.data$event_type_key) == "hap" &
          as.integer(.data$source) == 3L ~ "U+L"),
        levels = c("B","C","B+C","U","L","U+L")),
      .keep = "unused") |>
    dplyr::select(
      tidyselect::any_of(
        c("event_key","secondary_bsi","pathogen_key","index","source",
          "multiple","3gcr","car","cor","mrsa","vre")))
}

read_substance_days <- function(events_raw, processed_events, metadata, dataset_options) {
  events_raw |>
  dplyr::select("event", "dataValues") |>
  dplyr::inner_join(
    processed_events |>
      dplyr::select("event", "event_key"),
    dplyr::join_by("event")) |>
  tidyr::unnest_longer("dataValues") |>
  tidyr::unnest_wider("dataValues") |>
  dplyr::inner_join(
    metadata$dataElements |>
      dplyr::select("dataElement", "code") |>
      dplyr::filter(stringr::str_starts( .data$code, "NEOIPC_SURVEILLANCE_END_AB_SUBST_\\d\\d")),
    dplyr::join_by("dataElement")) |>
  dplyr::select(!"dataElement") |>
  dplyr::mutate(
    index = as.integer(
      stringr::str_extract(.data$code,"^NEOIPC_SURVEILLANCE_END_AB_SUBST_\\d(\\d)(_DAYS)?$", 1)),
    name = dplyr::if_else(stringr::str_ends(.data$code, "_DAYS"), "days", "substance_code"),
    .keep = "unused") |>
  tidyr::pivot_wider() |>
  dplyr::mutate(days = as.integer(.data$days)) |>
  dplyr::select("event_key","index","substance_code","days")
}
