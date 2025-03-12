read_events <- function(events, eventTypes, enrollments, patients, departments)
{
  events |>
    dplyr::inner_join(
      eventTypes |>
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
    dplyr::inner_join(
      departments |>
        dplyr::select("department_key", "orgUnit"),
      dplyr::join_by("orgUnit")) |>
    dplyr::mutate(
      status = factor(
        .data$status,
        levels = c(
          "ACTIVE", "COMPLETED", "VISITED", "SCHEDULE", "OVERDUE", "SKIPPED")),
      occurredAt = readr::parse_date(
        stringr::str_sub(.data$occurredAt, end = 10))
    )|>
    dplyr::select(
      c(
        "event",
        "occurredAt",
        "status",
        "event_type_key",
        "enrollment_key",
        "patient_key",
        "department_key")) |>
    add_key_column("event_key")
}

read_event_details <- function(events, processed_events, users)
{
  events |>
    dplyr::inner_join(
      processed_events |>
        dplyr::select("event_key", "event"),
      dplyr::join_by("event")) |>
    tidyr::hoist("createdBy", createdBy = 1, .remove = FALSE) |>
    tidyr::hoist("updatedBy", updatedBy = 1, .remove = FALSE) |>
    dplyr::left_join(users, dplyr::join_by("storedBy" == "username")) |>
    dplyr::mutate(storedBy = .data$user_key, .keep = "unused") |>
    dplyr::left_join(users, dplyr::join_by("createdBy" == "username")) |>
    dplyr::mutate(createdBy = .data$user_key, .keep = "unused") |>
    dplyr::left_join(users, dplyr::join_by("updatedBy" == "username")) |>
    dplyr::mutate(updatedBy = .data$user_key, .keep = "unused") |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("At"), readr::parse_datetime)) |>
    dplyr::select(
      "event_key",
      "scheduledAt",
      "createdAt",
      "updatedAt",
      "completedAt",
      "storedBy",
      "createdBy",
      "updatedBy",
      "followup",
      "deleted"
      )
}

read_event_notes <- function(events, processed_events, users)
{
  events |>
    dplyr::inner_join(
      processed_events |>
        dplyr::select("event_key", "event"),
      dplyr::join_by("event")) |>
    dplyr::select("event_key", "notes") |>
    tidyr::unnest_longer("notes") |>
    tidyr::unnest_wider("notes") |>
    tidyr::hoist("createdBy", createdBy = 1, .remove = FALSE) |>
    dplyr::left_join(users |> dplyr::select("user", "user_key"), dplyr::join_by("createdBy" == "user")) |>
    dplyr::mutate(createdBy = .data$user_key, .keep = "unused") |>
    dplyr::left_join(users |> dplyr::select("username", "user_key"), dplyr::join_by("storedBy" == "username")) |>
    dplyr::mutate(storedBy = .data$user_key, .keep = "unused") |>
    dplyr::mutate(storedAt = readr::parse_datetime(.data$storedAt)) |>
    dplyr::select(!"note")
}

read_event_data <- function(events, processed_events, dataElements, options, users, event_type_key)
{
  events |>
    dplyr::select("event", "dataValues") |>
    dplyr::inner_join(
      processed_events |>
        dplyr::filter(.data$event_type_key == !!event_type_key) |>
        dplyr::select("event", "event_key"),
      dplyr::join_by("event")) |>
    tidyr::unnest_longer("dataValues") |>
    tidyr::unnest_wider("dataValues") |>
    tidyr::hoist("createdBy", createdBy = 1, .remove = FALSE) |>
    dplyr::left_join(
      users |>
        dplyr::select("username", "user_key"),
      dplyr::join_by("createdBy" == "username")) |>
    dplyr::inner_join(
      dataElements |>
        dplyr::select("dataElement","code","valueType","optionSet"),
      dplyr::join_by("dataElement")) |>
    dplyr::left_join(
      options |>
        dplyr::arrange("optionSet_code", "sortOrder") |>
        dplyr::group_by(.data$optionSet_code) |>
        dplyr::summarise(levels = list(.data$code)),
      dplyr::join_by("optionSet" == "optionSet_code")) |>
    dplyr::mutate(
      createdBy = .data$user_key,
      value = convert_value(.data$value, .data$valueType, .data$levels),
      code = stringr::str_extract(tolower(.data$code), "^neoipc_(admission|surveillance_end|bsi|nec|hap|ssi|surgery)_(.+)$", group = 2),
      createdAt = readr::parse_datetime(.data$createdAt),
      updatedAt = readr::parse_datetime(.data$updatedAt),
      .keep = "unused"
    ) |>
    dplyr::select(!c("event","dataElement","optionSet")) |>
    tidyr::pivot_wider(
      names_from = "code",
      values_from = !c("code","event_key"),
      names_glue = "{code}_{.value}",
      names_vary = "slowest") |>
    tidyr::unnest_longer(dplyr::ends_with("value"), keep_empty = TRUE) |>
    dplyr::arrange(.data$event_key) |>
    dplyr::relocate(dplyr::ends_with("value"), .after = "event_key")
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

bla <- function(x)
{
  x
}
