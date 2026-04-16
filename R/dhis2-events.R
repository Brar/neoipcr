get_events_request <- function(req_base, dataset_options, programId)
{
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
     !is.null(dataset_options$trial_keys) ||
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
    fields <- paste0(fields,",storedBy,createdBy[username],updatedBy[username]")
    dataValueFields <- paste0(dataValueFields,",createdBy[username]")
  }

  fields <- paste0(fields,",dataValues[", dataValueFields, "]")

  req_base |>
    httr2::req_url_path_append("events") |>
    httr2::req_url_query(program = programId) |>
    httr2::req_url_query(fields = fields)
}

read_events <- function(events, enrollments, patients, metadata, dataset_options)
{
  opts <- dataset_options

  # Entity gate short-circuit: under `include_event = "no"` the public
  # events tibble is 0×0. Downstream readers that consume events
  # (read_event_details, read_event_notes, read_event_data,
  # read_infectious_agent_findings, read_substance_days) must tolerate
  # the empty shape. Matches the pattern in read_patients() /
  # read_enrollments().
  if (opts$include_event == "no")
    return(compile_schema(events_cols, opts))

  # Preconditions: the inner-join substitution path below requires
  # `enrollment_key + enrollment` on enrollments and `patient_key +
  # trackedEntity` on patients. Those are present under
  # `include_enrollment != "no"` + `"enrollments" %in% include_dhis2_ids`
  # and `include_patient != "no"` + `"patients" %in% include_dhis2_ids`
  # respectively. The fully-decoupled matrix (e.g. `include_event =
  # "full"` with `include_enrollment = "no"`) requires
  # `.enrollments_internal_map` / `.patients_internal_map` on the
  # metadata orchestrator; those land in a follow-up sub-task. For now
  # the reader requires both link sides to be at least present as
  # ids, matching the legacy reader's implicit dependency.
  if (opts$include_enrollment == "no" ||
      opts$include_patient    == "no" ||
      !("enrollments" %in% opts$include_dhis2_ids) ||
      !("patients"    %in% opts$include_dhis2_ids))
    rlang::abort(c(
      paste("`read_events()` requires enrollment and patient link",
            "substitution to be available."),
      "i" = paste(
        "Set `include_enrollment` and `include_patient` to a non-\"no\"",
        "value and include \"enrollments\" + \"patients\" in",
        "`include_dhis2_ids`."),
      "i" = paste(
        "Full decoupling (e.g. include_event=\"full\" with",
        "include_enrollment=\"no\") will land in a follow-up via",
        "orchestrator-internal link maps.")))

  # Raw `programStage` → `event_type_key` substitution uses the
  # orchestrator-internal map (`.eventTypes_internal_map`), not
  # `metadata$eventTypes`. The public tibble drops `programStage` when
  # `"event_types"` isn't in `include_dhis2_ids`, so reading the raw id
  # for this join must go through the internal map. Same pattern as
  # the `.users_internal_map` redirects in this file and in
  # `dhis2-trackedEntities.R` / `dhis2-enrollments.R`.
  events <- events |>
    dplyr::inner_join(
      metadata$.eventTypes_internal_map,
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

  # Hierarchy-key fat-lookup: pull each key that events_cols declares
  # under the current opts directly off `metadata$departments`. Under
  # the three-mode schema contract, departments carries exactly the
  # hierarchy keys whose `include_*` option is non-"no" (when
  # `include_department == "full"`), so the column list is a pure
  # function of opts — no legacy branching needed.
  hierarchy_keys <- c("department_key", "hospital_key",
                      "country_key", "world_bank_class_key")
  expected <- names(compile_schema(events_cols, opts))
  needed   <- intersect(hierarchy_keys, expected)

  if (length(needed) > 0L || opts$include_test_data) {
    cols <- "orgUnit"
    if (length(needed) > 0L) cols <- c(cols, needed)
    if (opts$include_test_data) cols <- c(cols, "isTest")

    # Consumer-side assertion at the schema-to-consumer boundary —
    # the hierarchy-key columns we just decided to pull must actually
    # be on `metadata$departments`; silent `any_of` tolerance would
    # turn a schema ↔ reader mismatch into downstream wrong data.
    require_cols(metadata$departments, cols, "departments")
    events <- events |>
      dplyr::left_join(
        metadata$departments |>
          dplyr::select(tidyselect::all_of(cols)),
        dplyr::join_by("orgUnit"))
  }

  if ("events" %in% opts$include_incomplete)
    events <- events |>
      dplyr::mutate(
        status = factor(
          .data$status,
          levels = c(
            "ACTIVE", "COMPLETED", "VISITED", "SCHEDULE", "OVERDUE", "SKIPPED"))
      )

  if (!opts$include_test_data ||
      length(opts$country_filter) > 0 ||
      !is.null(opts$trial_keys))
    events <- events |>
      dplyr::semi_join(metadata$departments, dplyr::join_by("orgUnit"))

  # `orgUnit`, `programStage`, `trackedEntity`, `enrollment`, `isTest`
  # are reader-internal scratch — fetched for the joins / filter above
  # and either substituted (programStage → event_type_key, trackedEntity
  # → patient_key, enrollment → enrollment_key) or used only for the
  # departments lookup (orgUnit) / not declared on events (isTest).
  # `event` stays if the id opt-in is set; the schema gates it.
  events <- events |>
    add_key_column("event_key") |>
    finalize_to_schema(
      events_cols, opts,
      scratch = c(
        "orgUnit", "programStage", "trackedEntity", "enrollment",
        "isTest", "dataValues", "followup", "scheduledAt", "completedAt",
        "createdAt", "createdAtClient", "updatedAt", "updatedAtClient",
        "createdBy", "updatedBy", "storedBy", "deleted", "notes"))

  assert_schema(events, events_cols, opts)

  events
}

read_event_details <- function(events, processed_events, metadata, dataset_options)
{
  events <- events |>
    dplyr::inner_join(
      processed_events |>
        dplyr::select("event_key", "event"),
      dplyr::join_by("event"))

  if(dataset_options$include_user != "no") {
    events <- events |>
      tidyr::hoist("createdBy", createdBy = 1, .remove = FALSE) |>
      tidyr::hoist("updatedBy", updatedBy = 1, .remove = FALSE) |>
      dplyr::left_join(
        metadata$.users_internal_map |>
          dplyr::select("user_key", "username"),
        dplyr::join_by("createdBy" == "username")) |>
      dplyr::mutate(createdBy = .data$user_key, .keep = "unused") |>
      dplyr::left_join(
        metadata$.users_internal_map |>
          dplyr::select("user_key", "username"),
        dplyr::join_by("updatedBy" == "username")) |>
      dplyr::mutate(updatedBy = .data$user_key, .keep = "unused") |>
      dplyr::mutate(dplyr::across(dplyr::ends_with("At"), readr::parse_datetime))

    if("storedBy" %in% names(events))
      events <- events |>
        dplyr::left_join(
          metadata$.users_internal_map |>
            dplyr::select("user_key", "username"),
          dplyr::join_by("storedBy" == "username")) |>
        dplyr::mutate(storedBy = .data$user_key, .keep = "unused")
  }

  events |>
    dplyr::select(
      tidyselect::any_of(
        c(
          "event_key","scheduledAt","createdAt","updatedAt","completedAt",
          "storedBy","createdBy","updatedBy","followup","deleted")))
}

read_event_notes <- function(events, processed_events, metadata, dataset_options)
{
  if(!("events" %in% dataset_options$include_notes))
    return(NULL)

  events <- events |>
    dplyr::inner_join(
      processed_events |>
        dplyr::select("event_key", "event"),
      dplyr::join_by("event")) |>
    dplyr::select("event_key", "notes") |>
    dplyr::filter(!is.na(.data$notes) & lengths(.data$notes) > 0)

  if(nrow(events) == 0)
    return(NULL)

  events <- events |>
    tidyr::unnest_longer("notes") |>
    tidyr::hoist("notes",
      note = "note",
      value = "value",
      storedBy = "storedBy",
      storedAt = "storedAt",
      createdBy = "createdBy",
      .remove = TRUE)

  if(nrow(events) == 0)
    return(NULL)

  if(dataset_options$include_user != "no") {
    events <- events |>
      tidyr::hoist("createdBy", createdBy = 1, .remove = FALSE) |>
      dplyr::left_join(
        metadata$.users_internal_map |>
          dplyr::select("user", "user_key"),
        dplyr::join_by("createdBy" == "user")) |>
      dplyr::mutate(createdBy = .data$user_key, .keep = "unused")

    if("storedBy" %in% names(events))
      events <- events |>
        dplyr::left_join(
          metadata$.users_internal_map |>
            dplyr::select("username", "user_key"),
          dplyr::join_by("storedBy" == "username")) |>
        dplyr::mutate(storedBy = .data$user_key, .keep = "unused")

    if("storedAt" %in% names(events))
      events <- events |>
        dplyr::mutate(storedAt = readr::parse_datetime(.data$storedAt))
  }

  events |>
    dplyr::select(!"note")
}

read_event_data <- function(events, processed_events, metadata, dataset_options, event_type_key)
{
  events <- events |>
    dplyr::select("event", "dataValues") |>
    dplyr::inner_join(
      processed_events |>
        dplyr::filter(.data$event_type_key == !!event_type_key) |>
        dplyr::select("event", "event_key"),
      dplyr::join_by("event")) |>
    tidyr::unnest_longer("dataValues") |>
    tidyr::unnest_wider("dataValues")

  if(dataset_options$include_user != "no" &&
     "createdBy" %in% names(events))
    events <- events |>
      tidyr::hoist("createdBy", createdBy = 1, .remove = FALSE) |>
      dplyr::left_join(
        metadata$.users_internal_map |>
          dplyr::select("username", "user_key"),
        dplyr::join_by("createdBy" == "username")) |>
      dplyr::mutate(
        createdBy = .data$user_key,
        .keep = "unused")

  if(dataset_options$include_timestamps &&
     "createdAt" %in% names(events) &&
     "updatedAt" %in% names(events))
    events <- events |>
      dplyr::mutate(
        createdAt = readr::parse_datetime(.data$createdAt),
        updatedAt = readr::parse_datetime(.data$updatedAt),
        .keep = "unused")

  if(nrow(events) > 0)
  {
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

read_infectious_agent_findings <- function(events_raw, processed_events, metadata, dataset_options)
{
  pathogen_data <- events_raw |>
    dplyr::select("event", "dataValues") |>
    dplyr::inner_join(
      processed_events |>
        dplyr::filter(.data$event_type_key %in% c("bsi","nec","ssi","hap")) |>
        dplyr::select("event", "event_key", "event_type_key"),
      dplyr::join_by("event"))

  empty_result <- tibble::tibble(
    event_key = integer(), secondary_bsi = logical(),
    pathogen_key = integer(), index = integer(),
    source = factor(levels = c("B","C","B+C","U","L","U+L")))

  if (nrow(pathogen_data) == 0)
    return(empty_result)

  pathogen_data <- pathogen_data |>
    tidyr::unnest_longer("dataValues") |>
    tidyr::unnest_wider("dataValues") |>
    dplyr::inner_join(
      metadata$dataElements |>
        dplyr::select("dataElement", "code"),
      dplyr::join_by("dataElement")) |>
    dplyr::filter(stringr::str_detect(.data$code, "PATHOGEN_\\d"))

  if (nrow(pathogen_data) == 0)
    return(empty_result)

  pathogen_data |>
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
          "multiple","3gcr","car","cor","mrsa","vre","name"))) |>
    add_key_column("agent_finding_key")
}

read_substance_days <- function(events_raw, processed_events, metadata, dataset_options)
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
