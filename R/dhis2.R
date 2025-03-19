#' @export
import_dhis2 <- function(connection_options = dhis2_connection_options(), metadata_options = dhis2_metadata_options())
{
  check_neoipcr_dhis2_conopt(connection_options)
  check_neoipcr_dhis2_mdopt(metadata_options)

  d2req_base <- dhis2_request(connection_options)

  user_info <- d2req_base |>
    get_user_info()

  metadata <- get_metadata(d2req_base, user_info, metadata_options)

  tracker_req <- d2req_base |>
    httr2::req_url_path_append("tracker") |>
    httr2::req_url_query(
      ouMode ="ACCESSIBLE",
      skipPaging = "true",
      includeDeleted = tolower(metadata_options$include_deleted))

  reqs <- list(
    get_trackedEntities_request(
      tracker_req,
      metadata_options,
      metadata$trackedEntityTypeId),
    get_enrollments_request(tracker_req, metadata_options),
    get_events_request(tracker_req, metadata_options))

  data <-  reqs |>
    httr2::req_perform_parallel() |>
    httr2::resps_data(\(resp){
      list(httr2::resp_body_json(resp) |>
             tibble::tibble() |>
             tidyr::unnest_longer(1) |>
             tidyr::unnest_wider(1))})

  trackedEntities_raw <- data[[1]]
  enrollments_raw <- data[[2]]
  events_raw <- data[[3]]

  patients <- read_patients(trackedEntities_raw, metadata, metadata_options)
  enrollments <- read_enrollments(enrollments_raw, patients, metadata, metadata_options)
  # read_enrollment_details
  # read_enrollment_notes
  events <- read_events(events_raw, enrollments, patients, metadata, metadata_options)
  eventDetails <- read_event_details(events_raw, events, metadata, metadata_options)
  eventNotes <- read_event_notes(events_raw, events, metadata, metadata_options)
  admissionData <- read_event_data(events_raw, events, metadata, metadata_options, "adm")
  surveillanceEndData <- read_event_data(events_raw, events, metadata, metadata_options, "end")
  sepsisData <- read_event_data(events_raw, events, metadata, metadata_options, "bsi")

  sepsisData <- sepsisData |>
    dplyr::mutate(
      pairs = get_cc_multiple(dplyr::pick(dplyr::matches("^pathogen_\\d_(multiple_)?value$")), ccs),
      cc_info = get_cc_info(.data$pairs),
      cc_only = cc_info$all_cc,
      any_cc_multiple = cc_info$any_cc_multiple,
      all_cc_multiple = cc_info$all_cc_multiple,
      .keep = "unused") |>
    dplyr::mutate(
      subType = as.factor(dplyr::if_else(
        is.finite(.data$cc_only),
        dplyr::if_else(
          as.logical(.data$cc_only),
          "lcbsi_cc",
          "lcbsi_ncc"),
        "clinical")),
      definition = as.factor(dplyr::if_else(
        .data$subType != "lcbsi_cc",
        .data$subType,
        dplyr::case_when(
          .data$all_cc_multiple ~ "all_cc_multiple",
          .data$any_cc_multiple ~ "any_cc_multiple",
          dplyr::if_any(
            dplyr::any_of(
              c("wbc_value","crp_value","procalcitonin_value","it_ratio_value",
                "interleukin_value")), .fns = ~ .x == TRUE) ~ "lab_finding",
          .data$ab_treatment_value ~ "ab_treatment"
        )
      )),
      .after = "event_key",
      .keep = "unused") |>
    dplyr::select(
      !(
        c("pairs","cc_info")
        |
        dplyr::matches("^pathogen_\\d_")))

  necData <- read_event_data(events_raw, events, metadata, metadata_options, "nec")
  pneumoniaData <- read_event_data(events_raw, events, metadata, metadata_options, "hap")
  surgeryData <- read_event_data(events_raw, events, metadata, metadata_options, "pro")
  ssiData <- read_event_data(events_raw, events, metadata, metadata_options, "ssi")

  infectiousAgentFindings <- read_infectious_agent_findings(events_raw, events, metadata, metadata_options)
  # read_infectious_agent_findings_details
  substanceDays <- read_substance_days(events_raw, events, metadata, metadata_options)
  # read_substance_days_details

  class(patients) <- c("neoipcr_pat", class(patients))
  class(enrollments) <- c("neoipcr_enr", class(enrollments))
  class(events) <- c("neoipcr_evt", class(events))
  class(eventDetails) <- c("neoipcr_evd", class(eventDetails))
  if(!is.null(eventNotes))
    class(eventNotes) <- c("neoipcr_evn", class(eventNotes))
  class(admissionData) <- c("neoipcr_adm", class(admissionData))
  class(surveillanceEndData) <- c("neoipcr_end", class(surveillanceEndData))
  class(surgeryData) <- c("neoipcr_pro", class(surgeryData))
  class(sepsisData) <- c("neoipcr_bsi", class(sepsisData))
  class(necData) <- c("neoipcr_nec", class(necData))
  class(ssiData) <- c("neoipcr_ssi", class(ssiData))
  class(pneumoniaData) <- c("neoipcr_hap", class(pneumoniaData))
  class(substanceDays) <- c("neoipcr_sbd", class(substanceDays))
  class(infectiousAgentFindings) <- c("neoipcr_iaf", class(infectiousAgentFindings))
  class(metadata) <- c("neoipcr_metadata", class(metadata))

  structure(
    list(
      patients = patients,
      enrollments = enrollments,
      events = events,
      admissionData = admissionData,
      surveillanceEndData = surveillanceEndData,
      sepsisData = sepsisData,
      necData = necData,
      pneumoniaData = pneumoniaData,
      surgeryData = surgeryData,
      ssiData = ssiData,
      substanceDays = substanceDays,
      infectiousAgentFindings = infectiousAgentFindings,
      metadata = metadata),
    class = c("neoipcr_ds", "list"))
}

#' @export
dhis2_connection_options <- function(
    token, username, session_id, scheme = "https",
    hostname = "neoipc.charite.de", port = NULL, path = "/api")
{
  ret <- list(
    base_url = httr2::url_build(
      structure(list(scheme = scheme, hostname = hostname, port = port, path = path), class = "httr2_url")))

  ret <- switch(
    rlang::check_exclusive(token, username, session_id, .require = FALSE),
    token = c(ret, list(token = read_token(token))),
    username = c(ret, list(username = username, password = get_password(ret$base_url))),
    session_id = c(ret, list(session_id = session_id)),
    c(ret, get_auth_data(ret$base_url))
  )

  structure(ret, class = "neoipcr_dhis2_conopt")
}

#' @export
dhis2_metadata_options <- function(
    include_world_bank_class = c("no","pseudonymised","yes"),
    include_country = c("no","pseudonymised","yes"),
    include_hospital = c("no","pseudonymised","yes"),
    include_department = c("no","pseudonymised","yes"),
    include_user = c("no","pseudonymised","yes"),
    include_patient_id = FALSE,
    include_dhis2_id = FALSE,
    include_timestamps = FALSE,
    include_test_data = FALSE,
    include_incomplete = rlang::chr(),
    include_notes = rlang::chr(),
    include_deleted = FALSE,
    trial_keys = NULL,
    translate = TRUE,
    locale = NULL)
{
  structure(list(
    include_world_bank_class = rlang::arg_match(include_world_bank_class),
    include_country = rlang::arg_match(include_country),
    include_hospital = rlang::arg_match(include_hospital),
    include_department = rlang::arg_match(include_department),
    include_user = rlang::arg_match(include_user),
    include_patient_id = include_patient_id,
    include_dhis2_id = include_dhis2_id,
    include_timestamps = include_timestamps,
    include_test_data = include_test_data,
    include_incomplete = rlang::arg_match(
      include_incomplete,
      c("enrollments","events"),
      multiple = TRUE),
    include_notes = rlang::arg_match(
      include_notes,
      c("enrollments","events"),
      multiple = TRUE),
    include_deleted = include_deleted,
    trial_keys = trial_keys,
    translate = translate,
    locale = locale
  ), class = "neoipcr_dhis2_mdopt")
}

#' @export
print.neoipcr_dhis2_conopt <- function(x, ...)
{
  parts <- paste0("Base URL: ", x$base_url)
  if(!is.null(x$token)) {
    parts <- c(parts, "Authentication: Token")
  } else if(!is.null(x$session_id)) {
    parts <- c(parts, "Authentication: Cookie")
  } else if(!is.null(x$username)) {
    parts <- c(
      parts,
      "Authentication: Basic",
      paste0("Username: ", x$username))
  }

  writeLines(parts)
  invisible(x)
}

get_auth_data <- function(url)
{
  env_session_id <- Sys.getenv("NEOIPC_DHIS2_SESSION_ID", unset = NA)
  if(!is.na(env_session_id)) return(list(session_id = session_id))

  env_token <- Sys.getenv("NEOIPC_DHIS2_TOKEN", unset = NA)
  if(!is.na(env_token)) return(list(token = read_token(env_token)))

  user <- Sys.getenv("NEOIPC_DHIS2_USER", unset = NA)
  if(is.na(user)) user <- askpass::askpass(
    prompt = sprintf(
      "Please enter your username for %s: ", url))

  if(is.null(user)) rlang::abort(
    message = "No username provided",
    body = "Please provide username and password, a personal access token or a session id to authenticate to DHIS2")

  list(username = user, password = get_password(url))
}

get_password <- function(url)
{
  pw <- Sys.getenv("NEOIPC_DHIS2_PASSWORD", unset = NA)
  if(!is.na(pw)) return(pw)

  pw <- askpass::askpass(
    prompt = sprintf("Please enter your password for %s: ", url))

  if(is.null(pw)) rlang::abort(
    message = "No password provided",
    body = "Please provide username and password, a personal access token or a session id to authenticate to DHIS2")

  pw
}

read_token <- function(token)
{
  if(stringr::str_starts(token, "d2pat_") &&  nchar(token) == 48)
    return(token)

  fileInfo <- file.info(token, extra_cols = FALSE)
  if(!rlang::is_na(fileInfo$isdir) && !fileInfo$isdir)
  {
    fileContent <- readChar(token, fileInfo$size)
    if(stringr::str_starts(fileContent, "d2pat_") && nchar(fileContent) == 48)
      return(fileContent)
  }
  rlang::abort("Invalid DHIS2 personal access token.")
}

read_eventData <- function(
    events,
    metadata,
    programStageName,
    prefix = NULL,
    dataElementFilter = NULL,
    keyColumn = NULL,
    keepEventType = TRUE)
{
  e <- events |>
    dplyr::inner_join(
      metadata$eventTypes |>
        dplyr::filter(.data$name == programStageName) |>
        dplyr::select("programStage","event_type_key"),
      dplyr::join_by("programStage")) |>
    dplyr::select(!"programStage") |>
    dplyr::mutate(
      notes = process_notes(.data$notes)) |>
    dplyr::mutate(dplyr::across(tidyselect::any_of(c(
      "createdAt",
      "updatedAt")), readr::parse_datetime)) |>
    dplyr::mutate(dplyr::across(tidyselect::any_of(c(
      "occurredAt",
      "scheduledAt",
      "completedAt")), ~ readr::parse_date(stringr::str_sub(.x, end = 10)))) |>
    dplyr::mutate(dplyr::across(
      tidyselect::any_of(c("followup", "deleted")),
      as.logical)) |>
    tidyr::hoist("createdBy", createdBy = 1, .remove = FALSE) |>
    tidyr::hoist("updatedBy", updatedBy = 1, .remove = FALSE) |>
    dplyr::mutate(
      status = factor(.data$status, levels = c(
        "ACTIVE", "COMPLETED", "VISITED", "SCHEDULE", "OVERDUE", "SKIPPED"))) |>
    dplyr::left_join(
      metadata$users |>
        dplyr::select("username", "user_key"),
      dplyr::join_by("storedBy" == "username")) |>
    dplyr::mutate(storedBy = .data$user_key, .keep = "unused") |>
    dplyr::left_join(
      metadata$users |>
        dplyr::select("username", "user_key"),
      dplyr::join_by("createdBy" == "username")) |>
    dplyr::mutate(createdBy = .data$user_key, .keep = "unused") |>
    dplyr::left_join(
      metadata$users |>
        dplyr::select("username", "user_key"),
      dplyr::join_by("updatedBy" == "username")) |>
    dplyr::mutate(updatedBy = .data$user_key, .keep = "unused") |>
    dplyr::left_join(
      metadata$departments |>
        dplyr::select("orgUnit", "department_key"),
      dplyr::join_by("orgUnit"))

  if(!is.null(keyColumn))
    e <- e |>
    add_key_column(keyColumn)

  if(!keepEventType)
    e <- e |>
    dplyr::select(!"event_type_key")

  if(!is.null(prefix))
    e <- e |> dplyr::rename_with(
      ~ paste0(prefix, .x, recycle0 = TRUE),
      !c("enrollment","trackedEntity", "dataValues"))

  e <- e |>
    tidyr::unnest_longer("dataValues") |>
    tidyr::unnest_wider("dataValues", names_sep = "_") |>
    dplyr::inner_join(
      metadata$dataElements |>
        dplyr::select("dataElement", "code"),
      dplyr::join_by("dataValues_dataElement" == "dataElement")) |>
    dplyr::select(!c("dataValues_dataElement", "dataValues_createdAt", "dataValues_updatedAt", "dataValues_createdBy"))

  if(!is.null(dataElementFilter))
    e <- e |>
    dplyr::filter(dataElementFilter(.data$code))

  e |>
    tidyr::pivot_wider(names_from = "code", values_from = "dataValues_value", names_sort = TRUE) |>
    convert_dataElementColumns(metadata$dataElements, metadata$options)
}

recode_enrollments <- function(events, enrollments)
  events |>
  dplyr::inner_join(
    enrollments |>
      dplyr::select("enrollment_key", "enrollment"),
    dplyr::join_by("enrollment")) |>
  dplyr::select(!"enrollment")

recode_events <- function(events, eventList)
{
  map <- dplyr::bind_rows(lapply(eventList, \(x) {
    x |>
      dplyr::select(dplyr::matches("^((sepsis|nec|ssi|pneumonia|surgery)_key)|(event)$")) |>
      dplyr::rename(infection_key = dplyr::matches("^(sepsis|nec|ssi|pneumonia|surgery)_key$"))
  }))
  events |>
    dplyr::left_join(map, dplyr::join_by("event")) |>
    dplyr::relocate("infection_key") |>
    dplyr::select(!"event")
}

get_pathogen_list <- function()
{
  pc <- internal_pathogen_concepts |>
    dplyr::rename("name" = "concept") |>
    dplyr::mutate(synonym_for = rlang::na_int)

  not_listed <- pc |>
    dplyr::slice_head()

  rest <- pc |>
      dplyr::filter(.data$id != 0) |>
      dplyr::bind_rows(
        internal_pathogen_synonyms |>
          dplyr::inner_join(
            internal_pathogen_concepts |>
              dplyr::select(!c("concept","concept_source","concept_id")),
            dplyr::join_by("synonym_for" == "id")) |>
          dplyr::relocate("concept_type", .before = "concept_source") |>
          dplyr::relocate("synonym_for", .after = "show_coli_r") |>
          dplyr::rename("name" = "synonym")) |>
      dplyr::arrange(.data$name)

  dplyr::bind_rows(not_listed, rest)
}

infer_sepsis_types <- function(sepses, causative_pathogens)
{
  sepses |>
    dplyr::left_join(
      causative_pathogens |>
        dplyr::inner_join(
          get_pathogen_list() |>
            dplyr::select("id", "is_cc"),
          dplyr::join_by("PATHOGEN" == "id")) |>
        dplyr::select("infection_key","event_type_key","is_cc"),
      dplyr::join_by("sepsis_key" == "infection_key", "event_type_key")) |>
    # if a sepsis contains both, a cc and a non-cc pathogen it is a non-cc sepsis
    dplyr::group_by(across(!.data$is_cc)) |>
    dplyr::summarise("is_cc" = as.logical(min(.data$is_cc)), .groups = "drop") |>
    dplyr::mutate(
      bsiType = factor(
        dplyr::case_when(
          is.na(.data$is_cc) ~ "Clin",
          .data$is_cc ~ "CoNS",
          !.data$is_cc ~ "BSI"),
        levels = c("BSI","CoNS","Clin")),
      .before = "NEOIPC_BSI_AB_TREATMENT") |>
    dplyr::select(!"is_cc")
}

convert_dataElementColumns <- function(t, dataElements, options)
{
  t |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::any_of(dataElements |> dplyr::pull("code")),
        ~ convert_dataElementColumn(.x, dplyr::cur_column(), dataElements, options)
        )
      )
}

convert_dataElementColumn <- function(col, col_name, dataElements, options)
{
  col_type <- dataElements |>
    dplyr::filter(.data$code == col_name) |>
    dplyr::select("valueType", "optionSet") |>
    unlist()

  if(!rlang::is_na(col_type[["optionSet"]]))
  {
    o <- options |>
      dplyr::filter(.data$optionSet_code == col_type[["optionSet"]])

    if(nrow(o) > 0)
      return(factor(col, levels = (o |> dplyr::pull("code"))))
  }

  if(stringr::str_starts(col_type[["valueType"]], "INTEGER"))
    return(as.integer(col))

  if(col_type[["valueType"]] %in% c("BOOLEAN", "TRUE_ONLY"))
    return(as.logical(col))

  col
}

process_notes <- function(notes)
{
  sapply(
    notes,
    \(x){
      if(length(x) == 0 || rlang::is_na(x))
        NA
      else
        paste0(
          purrr::map_chr(
            x,
            \(y) {
              sprintf(
                '%s %s (%s): "%s"',
                y[["createdBy"]][["firstName"]],
                y[["createdBy"]][["surname"]],
                format(readr::parse_datetime(y[["storedAt"]]), "%x %X"),
                y[["value"]])
              }), collapse = "; ")})
}

hoist_createdByAndupdatedBy <- function(table)
{
  table |>
    tidyr::hoist("createdBy", createdBy = 1, .remove = FALSE) |>
    tidyr::hoist("updatedBy", updatedBy = 1, .remove = FALSE)
}

get_testUnitIds <- function(metadata)
{
  organisationUnitGroups <- metadata |>
    purrr::pluck("organisationUnitGroups")

  if(is.null(organisationUnitGroups))
    NULL
  else
    organisationUnitGroups |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    dplyr::filter(.data$code == "TEST_UNITS") |>
    dplyr::select("organisationUnits") |>
    tidyr::unnest_longer(1) |>
    tidyr::unnest_wider(1) |>
    dplyr::select("id") |>
    unlist(use.names = FALSE)
}

add_key_column <- function(table, key_name = "key", as_factor = FALSE)
{
  tmp <- table |>
    dplyr::mutate(random = ids::random_id(nrow(table))) |>
    dplyr::arrange(.data$random) |>
    dplyr::select(!"random")

  if (as_factor) tmp <- tmp |>
      dplyr::mutate(!!key_name := as.factor(dplyr::row_number()))
  else tmp <- tmp |>
      dplyr::mutate(!!key_name := dplyr::row_number())

  tmp |>
    dplyr::relocate(key_name)
}

get_users_orgUnits <- function(metadata)
{
  users <- metadata |>
    purrr::pluck("users")

  if(is.null(users))
    NULL
  else
    users |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    dplyr::select(
      c(
        "id",
        "organisationUnits",
        "dataViewOrganisationUnits",
        "teiSearchOrganisationUnits")) |>
    tidyr::pivot_longer(
      cols = c(
        "organisationUnits",
        "dataViewOrganisationUnits",
        "teiSearchOrganisationUnits"),
      names_to = "type",
      values_to = "organisationUnit_id")
}

get_users_roles <- function(metadata)
{
  users <- metadata |>
    purrr::pluck("users")

  if(is.null(users))
    NULL
  else
    users |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    dplyr::select(c("id","userRoles")) |>
    tidyr::unnest_longer(2) |>
    tidyr::unnest_wider(2, names_sep = "_")
}

get_user_info <- function(req)
{
  raw_info <- req |>
    httr2::req_url_path_append("me") |>
    httr2::req_url_query(
      fields = "id,username,firstName,surname,email,created,userCredentials[lastLogin],organisationUnits[id],dataViewOrganisationUnits[id],teiSearchOrganisationUnits[id],userRoles[name,authorities],userGroups[name]") |>
    httr2::req_perform() |>
    httr2::resp_check_status() |>
    httr2::resp_body_json(simplifyVector = TRUE)

  structure(list(
    id = raw_info$id,
    username = raw_info$username,
    firstName = raw_info$firstName,
    surname = raw_info$surname,
    email = raw_info$email,
    lastLogin = readr::parse_datetime(raw_info$userCredentials$lastLogin),
    created = readr::parse_datetime(raw_info$created),
    organisationUnits = raw_info$organisationUnits$id,
    dataViewOrganisationUnits = raw_info$dataViewOrganisationUnits$id,
    teiSearchOrganisationUnits = raw_info$teiSearchOrganisationUnits$id,
    groups = raw_info$userGroups$name |>
      sort(),
    roles = raw_info$userRoles$name |>
      sort(),
    authorities = raw_info$userRoles$authorities |>
      unlist() |>
      unique() |>
      sort()
  ), class = c("neoipc_dhis2_usrinfo", "list"))
}

dhis2_request <- function(connection_options = dhis2_connection_options())
{
  req <- httr2::request(connection_options$base_url)
  if(exists('token', where = connection_options))
    req |>
      httr2::req_headers(Authorization = sprintf("ApiToken %s", connection_options$token), .redact = "Authorization")
  else if(exists('session_id', where = connection_options))
    req |>
      httr2::req_cookies_set(JSESSIONID = connection_options$session_id)
  else
    req |>
      httr2::req_auth_basic(username = connection_options$username, password = connection_options$password)
}

convert_value <- function(values, valueTypes, levelsLists)
{
  ret <- NULL
  for (i in 1:length(values)) {
    value <- values[i]
    valueType <- valueTypes[i]
    levels <- unlist(levelsLists[i])
    if(!is.null(levels))
      value <- factor(value, levels = levels)
    else if (stringr::str_starts(valueType, "INTEGER"))
      value <- as.integer(value)
    else if (valueType == "BOOLEAN" || valueType == "TRUE_ONLY")
      value <- as.logical(value)

    ret <- c(ret, list(value))
  }
  ret
}
