apply_data_removal <- function(x, dataset_options)
{
  if(!("id" %in% dataset_options$patient_columns))
  {
    x$patients <- x$patients |>
      dplyr::select(!tidyselect::any_of("patient_id"))
  }

  if(!("patients" %in% dataset_options$include_dhis2_ids))
  {
    x$patients <- x$patients |>
      dplyr::select(!tidyselect::any_of("trackedEntity"))
  }

  if(!("enrollments" %in% dataset_options$include_dhis2_ids))
  {
    x$enrollments <- x$enrollments |>
      dplyr::select(!tidyselect::any_of("enrollment"))
  }

  if(!("departments" %in% dataset_options$include_dhis2_ids))
  {
    x$metadata$departments <- x$metadata$departments |>
      dplyr::select(!tidyselect::any_of("orgUnit"))
  }

  if(dataset_options$include_department == "no")
  {
    x$metadata$departments <- NULL

    x$patients <- x$patients |>
      dplyr::select(!tidyselect::any_of("department_key"))
    x$enrollments <- x$enrollments |>
      dplyr::select(!tidyselect::any_of("department_key"))
    x$events <- x$events |>
      dplyr::select(!tidyselect::any_of("department_key"))
  }
  else if(dataset_options$include_department == "pseudo")
  {
    if("departments" %in% dataset_options$include_dhis2_ids)
      x$metadata$departments <- x$metadata$departments |>
        dplyr::select(tidyselect::all_of(c("department_key","orgUnit")))
    else
      x$metadata$departments <- NULL
  }

  if(dataset_options$include_hospital == "no")
  {
    x$metadata$hospitals <- NULL
    if(!is.null(x$metadata$departments))
      x$metadata$departments <- x$metadata$departments |>
        dplyr::select(!tidyselect::any_of("hospital_key"))
    x$patients <- x$patients |>
      dplyr::select(!tidyselect::any_of("hospital_key"))
    x$enrollments <- x$enrollments |>
      dplyr::select(!tidyselect::any_of("hospital_key"))
    x$events <- x$events |>
      dplyr::select(!tidyselect::any_of("hospital_key"))
  }
  if(dataset_options$include_hospital == "pseudo")
    x$metadata$hospitals <- NULL

  # `include_country` — the tibble shape in `metadata$countries` is now
  # reader-owned via `R/schema-orgunits.R::countries_cols`. The three
  # mode shapes (0×0 / 1-col / full) are produced by
  # `read_metadata_countries()` with a tail `assert_schema()`. The
  # guardian keeps the FK-scrub cascade on fact and adjacent-metadata
  # tables until those entities land in their own Phase B sub-tasks.
  if(dataset_options$include_country == "no")
  {
    if(!is.null(x$metadata$hospitals))
      x$metadata$hospitals <- x$metadata$hospitals |>
        dplyr::select(!tidyselect::any_of("country_key"))
    if(!is.null(x$metadata$departments))
      x$metadata$departments <- x$metadata$departments |>
        dplyr::select(!tidyselect::any_of("country_key"))
    x$patients <- x$patients |>
      dplyr::select(!tidyselect::any_of("country_key"))
    x$enrollments <- x$enrollments |>
      dplyr::select(!tidyselect::any_of("country_key"))
    x$events <- x$events |>
      dplyr::select(!tidyselect::any_of("country_key"))
  }

  # `include_world_bank_class` — downstream narrowing is now handled by the
  # reader via the schema contract. `x$metadata$worldBankClasses` follows
  # the three-mode shape (0×0 / 1-col / full) produced by
  # `read_metadata_wb_classes()`; the remaining per-fact-table scrubbing of
  # `world_bank_class_key` will move into the fact-table schemas as those
  # entities land in Phase B. Until then, keep the fact-table columns
  # scrubbed here so the guardian still catches leaks via
  # `include_world_bank_class` on entities that haven't been schematized.
  if(dataset_options$include_world_bank_class == "no")
  {
    if(!is.null(x$metadata$countries))
      x$metadata$countries <- x$metadata$countries |>
        dplyr::select(!tidyselect::any_of("world_bank_class_key"))
    if(!is.null(x$metadata$hospitals))
      x$metadata$hospitals <- x$metadata$hospitals |>
        dplyr::select(!tidyselect::any_of("world_bank_class_key"))
    if(!is.null(x$metadata$departments))
      x$metadata$departments <- x$metadata$departments |>
        dplyr::select(!tidyselect::any_of("world_bank_class_key"))
    x$patients <- x$patients |>
      dplyr::select(!tidyselect::any_of("world_bank_class_key"))
    x$enrollments <- x$enrollments |>
      dplyr::select(!tidyselect::any_of("world_bank_class_key"))
    x$events <- x$events |>
      dplyr::select(!tidyselect::any_of("world_bank_class_key"))
  }

  if(!("events" %in% dataset_options$include_dhis2_ids))
  {
    x$events <- x$events |>
      dplyr::select(!tidyselect::any_of("event"))
    if(!is.null(x$eventDetails))
      x$eventDetails <- x$eventDetails |>
        dplyr::select(!tidyselect::any_of("event"))
  }

  if(!("notes" %in% dataset_options$include_dhis2_ids))
  {
    if(!is.null(x$eventNotes))
      x$eventNotes <- x$eventNotes |>
        dplyr::select(!tidyselect::any_of("note"))
  }

  if(!("event_types" %in% dataset_options$include_dhis2_ids))
  {
    if(!is.null(x$metadata$eventTypes))
      x$metadata$eventTypes <- x$metadata$eventTypes |>
        dplyr::select(!tidyselect::any_of("programStage"))
  }

  if(!("users" %in% dataset_options$include_dhis2_ids))
  {
    if(!is.null(x$metadata$users))
      x$metadata$users <- x$metadata$users |>
        dplyr::select(!tidyselect::any_of("user"))
  }

  if(!("id" %in% dataset_options$patient_columns))
    x$patients <- x$patients |>
      dplyr::select(!tidyselect::any_of("neoipc_patient_id"))

  return(x)
}
