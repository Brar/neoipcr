#' Apply dataset-level filters anchored on a `dhis2_dataset_options`
#' object.
#'
#' Narrows `x` by the date / range / country filters declared on
#' `dataset_options` and then optionally runs `apply_postfilter()` to
#' cascade orphan removal through the hierarchy. The caller typically
#' uses the same `dataset_options` object that drove `import_dhis2()`;
#' the filters here re-apply the configuration post-hoc (e.g. on a
#' narrower birth-weight range than the original import request).
#'
#' Before phase-c-audit the function took individual filter parameters
#' (`birth_weight_from`, `gestational_age_from`, `countries`, `units`,
#' `keep_non_core_patients`). Pre-alpha: the old signature was removed
#' outright, no deprecation shim. See
#' `tasks/neoipcr-schema-arc/phase-c-audit.md` C3.
#'
#' @param x A `neoipcr_ds` object.
#' @param dataset_options A `dhis2_dataset_options` object. Consulted
#'   for `surveillance_end_from/to`, `birth_weight_from/to`,
#'   `gestational_age_from/to`, `country_filter`, and
#'   `include_ineligible_patients`.
#' @param remove_orphans If `TRUE` (default), also runs
#'   `apply_postfilter()` so that hierarchy rows with no surviving
#'   descendants are pruned.
#' @noRd
filter_dataset <- function(x, dataset_options, remove_orphans = TRUE)
{
  opts <- dataset_options

  x$events <- x$events |>
    filter_surveillance_ends(
      opts$surveillance_end_from,
      opts$surveillance_end_to)

  x$admissionData <- x$admissionData |>
    filter_admissions(opts$include_ineligible_patients)

  x$patients <- x$patients |>
    filter_patients(
      opts$birth_weight_from,
      opts$birth_weight_to,
      opts$gestational_age_from,
      opts$gestational_age_to,
      opts$include_ineligible_patients)

  x$metadata$countries <- x$metadata$countries |>
    filter_countries(opts$country_filter)

  if(remove_orphans)
    x <- x |>
    apply_postfilter()

  return(x)
}

filter_surveillance_ends <- function(
    events,
    surveillance_end_from = NULL,
    surveillance_end_to = NULL)
{
  if(is.null(surveillance_end_from) && is.null(surveillance_end_to))
    return(events)

  if(is.null(surveillance_end_from))
    dplyr::bind_rows(
      events |>
        dplyr::filter(.data$event_type_key != "end"),
      events |>
        dplyr::filter(
          .data$event_type_key == "end" &
            .data$occurredAt <= surveillance_end_to))
  else if(is.null(surveillance_end_to))
    dplyr::bind_rows(
      events |>
        dplyr::filter(.data$event_type_key != "end"),
      events |>
        dplyr::filter(
          .data$event_type_key == "end" &
            .data$occurredAt >= surveillance_end_from))
  else
    dplyr::bind_rows(
      events |>
        dplyr::filter(.data$event_type_key != "end"),
      events |>
        dplyr::filter(
          .data$event_type_key == "end" &
            .data$occurredAt >= surveillance_end_from &
            .data$occurredAt <= surveillance_end_to))
}

filter_admissions <- function(
    admission_data,
    include_ineligible_patients = FALSE)
{
  if(include_ineligible_patients)
    return(admission_data)

  admission_data |>
    dplyr::filter(.data$dol < 120)
}

filter_patients <- function(
    patients,
    birth_weight_from = NULL,
    birth_weight_to = NULL,
    gestational_age_from = NULL,
    gestational_age_to = NULL,
    include_ineligible_patients = FALSE)
{
  if(!is.null(birth_weight_from))
    patients <- patients |>
      dplyr::filter(.data$birth_weight >= birth_weight_from)
  if(!is.null(birth_weight_to))
    patients <- patients |>
      dplyr::filter(.data$birth_weight <= birth_weight_to)
  if(!is.null(gestational_age_from))
    patients <- patients |>
      dplyr::filter(.data$total_gestation_days >= (gestational_age_from * 7))
  if(!is.null(gestational_age_to))
    patients <- patients |>
      dplyr::filter(.data$total_gestation_days <= (gestational_age_to * 7))
  if(!include_ineligible_patients)
    patients <- patients |>
      dplyr::filter(
        .data$total_gestation_days < 224 | .data$birth_weight < 1500)
  return(patients)
}

filter_countries <- function(
    countries,
    included_countries)
{
  if(is.null(included_countries) || length(included_countries) < 1)
    return(countries)

  countries |>
    dplyr::filter(.data$code %in% included_countries)
}

apply_postfilter <- function(x)
{
  worldBankClasses <- x$metadata$worldBankClasses
  countries <- x$metadata$countries
  hospitals <- x$metadata$hospitals
  departments <- x$metadata$departments
  patients <- x$patients
  enrollments <- x$enrollments
  events <- x$events
  eventNotes <- x$eventNotes
  enrollment_notes <- x$enrollment_notes
  admissionData <- x$admissionData
  surveillanceEndData <- x$surveillanceEndData
  sepsisData <- x$sepsisData
  necData <- x$necData
  pneumoniaData <- x$pneumoniaData
  surgeryData <- x$surgeryData
  ssiData <- x$ssiData
  infectiousAgentFindings <- x$infectiousAgentFindings
  substanceDays <- x$substanceDays

  surveillance_end_events <- x$events |>
    dplyr::filter(.data$event_type_key != "end") |>
    dplyr::select("enrollment_key", "event_key")

  #############################
  ## First filter enrollments #
  #############################
  # Filtering by patients, admissionData and surveillance_end_events will always work
  enrollments <- enrollments |>
    dplyr::semi_join(patients, dplyr::join_by("patient_key")) |>
    dplyr::semi_join(
      surveillance_end_events |>
        dplyr::semi_join(
          admissionData, dplyr::join_by("event_key")),
      dplyr::join_by("enrollment_key"))

  # Filtering by country will only work if we have country information
  # Keep enrollments with NA country_key (test data without a country).
  # `countries` follows the three-mode schema contract and is always a
  # tibble — guard on column presence, not null-ness.
  if("country_key" %in% names(countries) &&
     "country_key" %in% names(enrollments))
    enrollments <- enrollments |>
    dplyr::filter(
      is.na(.data$country_key) |
      .data$country_key %in% countries$country_key)

  # Filtering by unit will only work if we have unit information
  # `departments` is always a tibble (never NULL) under the three-mode
  # schema contract — guard on column presence, not null-ness.
  if("department_key" %in% names(departments) &&
     "department_key" %in% names(enrollments))
    enrollments <- enrollments |>
    dplyr::semi_join(departments, dplyr::join_by("department_key"))

  ########################################################
  ## Second filter all the other elements by enrollments #
  ########################################################
  if("department_key" %in% names(departments) &&
     "department_key" %in% names(enrollments))
    departments <- departments |>
    dplyr::semi_join(enrollments, dplyr::join_by("department_key"))
  # `hospitals` is always a tibble (never NULL) under the three-mode
  # schema contract — guard on column presence, not null-ness.
  if("hospital_key" %in% names(hospitals) &&
     "hospital_key" %in% names(enrollments))
    hospitals <- hospitals |>
    dplyr::semi_join(enrollments, dplyr::join_by("hospital_key"))
  if("country_key" %in% names(countries) &&
     "country_key" %in% names(enrollments))
    countries <- countries |>
    dplyr::semi_join(enrollments, dplyr::join_by("country_key"))
  # `worldBankClasses` is always a tibble (never NULL) since the reader
  # honors the three-mode schema contract. Guard on column presence
  # instead of null-ness: under "no" the tibble has 0 columns and the
  # semi_join would fail to find `world_bank_class_key`.
  if("world_bank_class_key" %in% names(worldBankClasses) &&
     "world_bank_class_key" %in% names(enrollments))
    worldBankClasses <- worldBankClasses |>
    dplyr::semi_join(enrollments, dplyr::join_by("world_bank_class_key"))

  patients <- patients |>
    dplyr::semi_join(enrollments, dplyr::join_by("patient_key"))

  events <- events |>
    dplyr::semi_join(enrollments, dplyr::join_by("enrollment_key"))

  # (No `eventDetails` semi_join — merged into `events` in
  # phase-b-event-details; entity-level user / timestamp / deleted /
  # followup fields now travel on `events` itself and are carried by the
  # events semi_join above.)

  # eventNotes + enrollment_notes are always tibbles under the schema
  # contract — guard on column presence (0×0 under gate means no
  # event_key / enrollment_key column to semi_join on).
  if("event_key" %in% names(eventNotes))
    eventNotes <- eventNotes |>
    dplyr::semi_join(events, dplyr::join_by("event_key"))

  if("enrollment_key" %in% names(enrollment_notes))
    enrollment_notes <- enrollment_notes |>
    dplyr::semi_join(enrollments, dplyr::join_by("enrollment_key"))

  admissionData <- admissionData |>
    dplyr::semi_join(events, dplyr::join_by("event_key"))

  surveillanceEndData <- surveillanceEndData |>
    dplyr::semi_join(events, dplyr::join_by("event_key"))

  sepsisData <- sepsisData |>
    dplyr::semi_join(events, dplyr::join_by("event_key"))

  necData <- necData |>
    dplyr::semi_join(events, dplyr::join_by("event_key"))

  pneumoniaData <- pneumoniaData |>
    dplyr::semi_join(events, dplyr::join_by("event_key"))

  surgeryData <- surgeryData |>
    dplyr::semi_join(events, dplyr::join_by("event_key"))

  ssiData <- ssiData |>
    dplyr::semi_join(events, dplyr::join_by("event_key"))

  infectiousAgentFindings <- infectiousAgentFindings |>
    dplyr::semi_join(events, dplyr::join_by("event_key"))

  substanceDays <- substanceDays |>
    dplyr::semi_join(events, dplyr::join_by("event_key"))

  x$metadata$worldBankClasses <- worldBankClasses
  x$metadata$countries <- countries
  x$metadata$hospitals <- hospitals
  x$metadata$departments <- departments
  x$patients <- patients
  x$enrollments <- enrollments
  x$events <- events
  x$eventNotes <- eventNotes
  x$enrollment_notes <- enrollment_notes
  x$admissionData <- admissionData
  x$surveillanceEndData <- surveillanceEndData
  x$sepsisData <- sepsisData
  x$necData <- necData
  x$pneumoniaData <- pneumoniaData
  x$surgeryData <- surgeryData
  x$ssiData <- ssiData
  x$infectiousAgentFindings <- infectiousAgentFindings
  x$substanceDays <- substanceDays

  return(x)
}
