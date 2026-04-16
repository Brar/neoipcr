apply_data_removal <- function(x, dataset_options)
{
  # `include_patient` / `patient_columns` / `"patients" %in%
  # include_dhis2_ids` — tibble shape is now reader-owned via
  # `R/schema-patients.R::patients_cols`. Patient-attribute columns
  # (`patient_id`, `sex`, ...), their companion columns, the entity
  # flags (`inactive`, `potentialDuplicate`), and the
  # `trackedEntity` id are all gated at the schema level. Legacy
  # scrubs for `patient_id` (via `"id" %in% patient_columns`) and
  # `trackedEntity` (via `"patients" %in% include_dhis2_ids`) are
  # redundant and removed.

  # `include_enrollment` / `"enrollments" %in% include_dhis2_ids` —
  # tibble shape is reader-owned via
  # `R/schema-enrollments.R::enrollments_cols`. The `enrollment` id is
  # gated on `"enrollments" %in% include_dhis2_ids` at the schema
  # level; legacy scrub removed.

  # `include_event` / `"events" %in% include_dhis2_ids` — tibble shape
  # is reader-owned via `R/schema-events.R::events_cols`. The `event`
  # id, `event_type_key`, `status`, and the link / hierarchy keys are
  # all gated at the schema level. Legacy scrubs for `event` on events
  # and for each hierarchy key on events are redundant and removed.
  # The scrub for `event` on `eventDetails` stays until that tibble
  # schematizes in its own sub-task.

  # `include_department` — tibble shape is reader-owned via
  # `R/schema-orgunits.R::departments_cols`. The `orgUnit` column is
  # gated by `"departments" %in% include_dhis2_ids` at the schema level,
  # so `finalize_to_schema()` in the metadata orchestrator drops it when
  # needed; the legacy scrub here is redundant and removed. The guardian
  # keeps the FK-scrub cascade on fact tables as belt-and-suspenders for
  # fixture-based test surfaces; Phase C turns these into assertions.
  if(dataset_options$include_department == "no")
  {
    x$patients <- x$patients |>
      dplyr::select(!tidyselect::any_of("department_key"))
    x$enrollments <- x$enrollments |>
      dplyr::select(!tidyselect::any_of("department_key"))
    x$events <- x$events |>
      dplyr::select(!tidyselect::any_of("department_key"))
  }

  # `include_hospital` — tibble shape is reader-owned via
  # `R/schema-orgunits.R::hospitals_cols` + each schematized fact
  # entity's cols. The guardian keeps the FK-scrub cascade on
  # metadata cross-refs and fact tables as belt-and-suspenders for
  # fixture-based test surfaces; Phase C turns these into assertions.
  if(dataset_options$include_hospital == "no")
  {
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

  # `include_country` — tibble shape is reader-owned via
  # `R/schema-orgunits.R::countries_cols` + each schematized fact
  # entity's cols. The guardian keeps the FK-scrub cascade on
  # metadata cross-refs and fact tables as belt-and-suspenders for
  # fixture-based test surfaces; Phase C turns these into assertions.
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

  # `include_world_bank_class` — tibble shape is reader-owned via
  # `R/schema-orgunits.R::worldBankClasses_cols` + each schematized
  # fact entity's cols. The guardian keeps the FK-scrub cascade on
  # metadata cross-refs and fact tables as belt-and-suspenders for
  # fixture-based test surfaces; Phase C turns these into assertions.
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

  # `eventDetails` still carries `event` until it schematizes in its
  # own sub-task; keep the scrub for that table only. The events scrub
  # moved into `events_cols` via the schema contract.
  if(!("events" %in% dataset_options$include_dhis2_ids))
  {
    if(!is.null(x$eventDetails))
      x$eventDetails <- x$eventDetails |>
        dplyr::select(!tidyselect::any_of("event"))
  }

  # `"notes" %in% include_dhis2_ids` — `note` column on eventNotes /
  # enrollment_notes is reader-owned via
  # `R/schema-notes.R::event_notes_cols` + `enrollment_notes_cols`,
  # gated at the schema level. Legacy scrub removed.

  # `"event_types" %in% include_dhis2_ids` — tibble shape is now
  # reader-owned via `R/schema-orgunits.R::eventTypes_cols`. The
  # `programStage` column is gated at the schema level; the reader
  # narrows via `finalize_to_schema()`, and the internal FK-resolution
  # lookup (`programStage` → `event_type_key`) travels on
  # `.eventTypes_internal_map`. Legacy scrub here is redundant.

  # `include_user` / `include_dhis2_ids == "users"` — tibble shape is now
  # reader-owned via `R/schema-orgunits.R::users_cols`. The `user` column
  # is gated on `"users" %in% include_dhis2_ids` at the schema level, and
  # the rest of the three-mode shape is driven by `include_user`. The
  # legacy scrub here is redundant and removed.

  # Metadata tibbles are curated by the NeoIPC team, not by partner-site
  # data entry, so they must never carry per-row author/timestamp
  # companion columns. Assert loudly in the guardian so an accidental
  # reader regression that leaks these columns surfaces here. When this
  # function is renamed to `assert_data_protection()` in Phase C the
  # assertion stays with it.
  metadata_companion_cols <- c(
    "createdBy", "updatedBy", "createdAt", "updatedAt")
  metadata_tables <- c(
    "worldBankClasses", "countries", "hospitals",
    "departments", "users", "eventTypes")
  for (tbl in metadata_tables) {
    t <- x$metadata[[tbl]]
    if (!is.null(t)) {
      leaked <- intersect(metadata_companion_cols, names(t))
      if (length(leaked) > 0L)
        rlang::abort(c(
          sprintf(
            paste0("Metadata tibble `%s` carries companion column(s) ",
                   "that are reserved for partner-site-entered entities:"),
            tbl),
          "x" = paste(leaked, collapse = ", "),
          "i" = paste0("Metadata entities are curated by NeoIPC, not ",
                       "partner sites. Drop these columns from the reader.")
        ))
    }
  }

  return(x)
}
