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

  # `include_department` — tibble shape is reader-owned via
  # `R/schema-orgunits.R::departments_cols`. The `orgUnit` column is
  # gated by `"departments" %in% include_dhis2_ids` at the schema level,
  # so `finalize_to_schema()` in the metadata orchestrator drops it when
  # needed; the legacy scrub here is redundant and removed. The guardian
  # keeps the FK-scrub cascade on fact tables until those entities
  # schematize.
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
  # `R/schema-orgunits.R::hospitals_cols`. The guardian keeps the
  # FK-scrub cascade on fact / adjacent-metadata tables until those
  # entities schematize.
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
