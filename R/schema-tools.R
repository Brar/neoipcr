get_schema_registry <- function(dataset_options) {
  list(
    patients = get_patients_schema(dataset_options),
    enrollments = tibble::tibble(
      enrollment_key = integer(),
      enrolledAt = as.Date(character()),
      followUp = logical(),
      patient_key = integer()
    ),
    events = tibble::tibble(
      event_key = integer(),
      occurredAt = as.Date(character()),
      event_type_key = factor(
        levels = c("adm", "pro", "bsi", "nec", "ssi", "hap", "end")),
      enrollment_key = integer()
    ),
    eventDetails = tibble::tibble(
      event_key = integer(),
      followup = logical()
    ),
    admissionData = tibble::tibble(
      event_key = integer(),
      type = factor(levels = c("1", "2", "3")),
      dol = integer()
    ),
    surveillanceEndData = tibble::tibble(
      event_key = integer(),
      reason = factor(levels = c("1", "2")),
      patient_days = integer(),
      cvc_days = integer(),
      pvc_days = integer(),
      vs_days = integer(),
      inv_days = integer(),
      niv_days = integer(),
      ab_days = integer(),
      human_milk_days = integer(),
      kangaroo_care_days = integer(),
      probiotic_days = integer()
    ),
    substanceDays = tibble::tibble(
      event_key = integer(),
      index = integer(),
      substance_code = character(),
      days = integer()
    ),
    sepsisData = tibble::tibble(
      event_key = integer(),
      dev_ass = factor(levels = c("0", "1", "2")),
      los = integer(),
      dol = integer(),
      ab_treatment = logical(),
      acidosis = logical(),
      apnoea = logical(),
      bradycardia = logical(),
      crp = logical(),
      feeding_intolerance = logical(),
      hyperglycaemia = logical(),
      interleukin = logical(),
      irritability = logical(),
      no_pos_culture = logical(),
      perfusion = logical(),
      platelet_count = logical(),
      temperature = logical(),
      wbc = logical()
    ),
    necData = tibble::tibble(
      event_key = integer(),
      los = integer(),
      dol = integer(),
      sec_bsi = factor(levels = c("1", "0", "-1")),
      abdominal_distension = logical(),
      abdominal_skin_tone = logical(),
      bilious_aspirate = logical(),
      bowel_necrosis = logical(),
      fixed_loop = logical(),
      pneumatosis_intestinalis_img = logical(),
      pneumatosis_intestinalis_surg = logical(),
      pneumoperitoneum = logical()
    ),
    pneumoniaData = tibble::tibble(
      event_key = integer(),
      dev_ass = factor(levels = c("0", "1", "2")),
      los = integer(),
      dol = integer(),
      sec_bsi = factor(levels = c("1", "0", "-1")),
      microbiological_test_result = factor(levels = c("1", "0", "-1")),
      bradycardia = logical(),
      fever = logical(),
      imaging_findings = logical(),
      increased_respiratory_secretion = logical(),
      laboratory_findings = logical(),
      purulent_tracheal_aspirate = logical(),
      respiratory_distress = logical(),
      respiratory_support = logical(),
      tachypnoea = logical()
    ),
    surgeryData = tibble::tibble(
      event_key = integer(),
      los = integer(),
      dol = integer(),
      procedure_description = character(),
      main_procedure_code = character(),
      side_procedure_code_1 = character(),
      side_procedure_code_2 = character(),
      asa_score = factor(levels = c("1", "2", "3", "4", "5")),
      wound_class = factor(levels = c("1", "2", "3", "4")),
      duration = integer(),
      emergency_procedure = logical(),
      endoscopic_procedure = logical(),
      implant = logical(),
      primary_closure = logical(),
      revision_procedure = logical()
    ),
    ssiData = tibble::tibble(
      event_key = integer(),
      los = integer(),
      dol = integer(),
      infection_type = factor(levels = c("1", "2", "3")),
      sec_bsi = factor(levels = c("1", "0", "-1")),
      organisms_superf = factor(levels = c("1", "0", "-1")),
      abscess_deep = logical(),
      fever = logical(),
      inc_dehisces_deep = logical(),
      inc_opened_superf = logical(),
      infection_present = logical(),
      localized_erythema = logical(),
      localized_heat = logical(),
      localized_pain_deep = logical(),
      localized_pain_superf = logical(),
      localized_swelling = logical(),
      organisms_deep = factor(levels = c("1", "0", "-1")),
      physician_diag_superf = logical(),
      purulent_drainage_superf = logical()
    ),
    infectiousAgentFindings = tibble::tibble(
      event_key = integer(),
      secondary_bsi = logical(),
      pathogen_key = integer(),
      index = integer(),
      source = factor(levels = c("B", "C", "B+C", "U", "L", "U+L")),
      `3gcr` = factor(levels = c("no", "yes", "not_tested")),
      car = factor(levels = c("no", "yes", "not_tested")),
      cor = factor(levels = c("no", "yes", "not_tested")),
      mrsa = factor(levels = c("no", "yes", "not_tested")),
      vre = factor(levels = c("no", "yes", "not_tested"))
    ),
    validationResults = tibble::tibble(
      rule_id = integer(),
      patient_key = integer(),
      enrollment_key = integer(),
      event_key = integer(),
      context = list()
    ),
    metadata = list(
      worldBankClasses = tibble::tibble(
        world_bank_class_key = integer(),
        class = factor(levels = c("L", "LM", "UM", "H")),
        fiscal_year = integer(),
        displayShortName = character(),
        displayDescription = character(),
        displayName = character()
      ),
      countries = tibble::tibble(
        country_key = integer(),
        code = character(),
        displayShortName = character(),
        displayDescription = character(),
        displayName = character()
      ),
      hospitals = tibble::tibble(
        hospital_key = integer(),
        orgUnit = character(),
        code = character(),
        displayName = character(),
        displayShortName = character(),
        displayDescription = character(),
        comment = character(),
        longitude = numeric(),
        latitude = numeric()
      ),
      departments = tibble::tibble(
        department_key = integer(),
        code = character(),
        openingDate = as.Date(character()),
        displayShortName = character(),
        displayName = character(),
        displayDescription = character(),
        comment = character(),
        longitude = numeric(),
        latitude = numeric(),
        hospital_key = integer(),
        isTest = logical()
      ),
      eventTypes = tibble::tibble(
        event_type_key = factor(
          levels = c("adm", "pro", "bsi", "nec", "ssi", "hap", "end")),
        programStage = character(),
        name = factor(
          levels = c(
            "Admission",
            "Surgical Procedure",
            "Primary Sepsis/BSI",
            "Necrotizing enterocolitis",
            "Surgical Site Infection",
            "Pneumonia",
            "Surveillance-End")),
        displayDescription = character(),
        displayFormName = character(),
        displayName = character()
      )
    )
  )
}

get_patients_schema <- function(dataset_options = NULL) {
  check_neoipcr_dhis2_dsopt(dataset_options, allow_null = TRUE)

  schema <- tibble::tibble(
    patient_key = integer(),
    sex = factor(levels = c("f", "m", "u")),
    siblings = integer(),
    total_gestation_days = integer(),
    birth_weight = integer(),
    delivery_mode = factor(levels = c("1", "2", "3")),
    inactive = logical(),
    potentialDuplicate = logical()
  )

  if(is.null(dataset_options)){
    return(
      schema |>
        # relocate_patients_columns is our reference for column order
        relocate_patients_columns()
      )
  }

  if(dataset_options$include_patient_id) {
    schema <- schema |>
    dplyr::bind_cols(patient_id = character())

    if(dataset_options$include_user != "no")
      schema <- schema |>
        dplyr::bind_cols(
          patient_id_storedBy = integer()
        )

    if(dataset_options$include_timestamps)
      schema <- schema |>
        dplyr::bind_cols(
          patient_id_createdAt = as.POSIXct(character(), tz = "UTC"),
          patient_id_updatedAt = as.POSIXct(character(), tz = "UTC"),
        )
  }

  if("patients" %in% dataset_options$include_dhis2_ids)
    schema <- schema |>
    dplyr::bind_cols(trackedEntity = character())

  if(dataset_options$include_user != "no")
    schema <- schema |>
    dplyr::bind_cols(
      createdBy = integer(),
      updatedBy = integer(),
      sex_storedBy = integer(),
      siblings_storedBy = integer(),
      total_gestation_days_storedBy = integer(),
      birth_weight_storedBy = integer(),
      delivery_mode_storedBy = integer()
    )

  if(dataset_options$include_timestamps)
    schema <- schema |>
    dplyr::bind_cols(
      createdAt = as.POSIXct(character(), tz = "UTC"),
      createdAtClient = as.POSIXct(character(), tz = "UTC"),
      updatedAt = as.POSIXct(character(), tz = "UTC"),
      updatedAtClient = as.POSIXct(character(), tz = "UTC"),
      sex_createdAt = as.POSIXct(character(), tz = "UTC"),
      sex_updatedAt = as.POSIXct(character(), tz = "UTC"),
      siblings_createdAt = as.POSIXct(character(), tz = "UTC"),
      siblings_updatedAt = as.POSIXct(character(), tz = "UTC"),
      total_gestation_days_createdAt = as.POSIXct(character(), tz = "UTC"),
      total_gestation_days_updatedAt = as.POSIXct(character(), tz = "UTC"),
      birth_weight_createdAt = as.POSIXct(character(), tz = "UTC"),
      birth_weight_updatedAt = as.POSIXct(character(), tz = "UTC"),
      delivery_mode_createdAt = as.POSIXct(character(), tz = "UTC"),
      delivery_mode_updatedAt = as.POSIXct(character(), tz = "UTC")
    )

  if(dataset_options$include_department != "no")
    schema <- schema |>
    dplyr::bind_cols(
      department_key = integer()
    )

  if(dataset_options$include_hospital != "no")
    schema <- schema |>
    dplyr::bind_cols(
      hospital_key = integer()
    )

  if(dataset_options$include_country != "no")
    schema <- schema |>
    dplyr::bind_cols(
      country_key = integer()
    )

  if(dataset_options$include_world_bank_class != "no")
    schema <- schema |>
    dplyr::bind_cols(
      world_bank_class_key = integer()
    )

  schema |>
    relocate_patients_columns()
}

ensure_patients_schema <- function(x, dataset_options = NULL) {
  vctrs::vec_rbind(get_patients_schema(dataset_options), x) |>
    dplyr::mutate(
      siblings = tidyr::replace_na(.data$siblings, 1L)
    )
}

relocate_patients_columns <- function(x) {
  x |>
    dplyr::relocate(tidyselect::any_of(
      c("patient_key",
        "trackedEntity","createdBy","createdAt","createdAtClient","updatedBy","updatedAt","updatedAtClient",
        "patient_id","patient_id_storedBy","patient_id_createdAt","patient_id_updatedAt",
        "sex","sex_storedBy","sex_createdAt","sex_updatedAt",
        "siblings","siblings_storedBy","siblings_createdAt","siblings_updatedAt",
        "total_gestation_days","total_gestation_days_storedBy","total_gestation_days_createdAt","total_gestation_days_updatedAt",
        "birth_weight","birth_weight_storedBy","birth_weight_createdAt","birth_weight_updatedAt",
        "delivery_mode","delivery_mode_storedBy","delivery_mode_createdAt","delivery_mode_updatedAt",
        "inactive",
        "potentialDuplicate",
        "department_key","hospital_key","country_key","world_bank_class_key"
      )
    ), .before = 1)
}

get_enrollments_schema <- function(dataset_options = NULL) {
  check_neoipcr_dhis2_dsopt(dataset_options, allow_null = TRUE)

  schema <- tibble::tibble(
    enrollment_key = integer(),
    enrolledAt = as.Date(character()),
    occurredAt = as.Date(character()),
    followUp = logical(),
    patient_key = integer()
  )

  if(is.null(dataset_options)){
    return(
      schema |>
        # relocate_enrollments_columns is our reference for column order
        relocate_enrollments_columns()
    )
  }

  if("enrollments" %in% dataset_options$include_dhis2_ids)
    schema <- schema |>
    dplyr::bind_cols(enrollment = character())

  if("enrollments" %in% dataset_options$include_incomplete)
    schema <- schema |>
    dplyr::bind_cols(
      status = factor(
        character(),
        levels = c("ACTIVE", "COMPLETED", "CANCELLED")))

  if(dataset_options$include_user != "no")
    schema <- schema |>
    dplyr::bind_cols(
      createdBy = integer(),
      updatedBy = integer(),
      completedBy = integer(),
      storedBy = integer()
    )

  if(dataset_options$include_timestamps)
    schema <- schema |>
    dplyr::bind_cols(
      createdAt = as.POSIXct(character(), tz = "UTC"),
      createdAtClient = as.POSIXct(character(), tz = "UTC"),
      updatedAt = as.POSIXct(character(), tz = "UTC"),
      updatedAtClient = as.POSIXct(character(), tz = "UTC"),
      completedAt = as.POSIXct(character(), tz = "UTC")
    )

  if(dataset_options$include_department != "no")
    schema <- schema |>
    dplyr::bind_cols(
      department_key = integer()
    )

  if(dataset_options$include_hospital != "no")
    schema <- schema |>
    dplyr::bind_cols(
      hospital_key = integer()
    )

  if(dataset_options$include_country != "no")
    schema <- schema |>
    dplyr::bind_cols(
      country_key = integer()
    )

  if(dataset_options$include_world_bank_class != "no")
    schema <- schema |>
    dplyr::bind_cols(
      world_bank_class_key = integer()
    )

  schema |>
    relocate_enrollments_columns()
}

ensure_enrollments_schema <- function(x, dataset_options = NULL) {
  vctrs::vec_rbind(get_enrollments_schema(dataset_options), x)
}

relocate_enrollments_columns <- function(x) {
  x |>
    dplyr::relocate(tidyselect::any_of(
      c("enrollment_key",
        "enrollment","createdBy","createdAt","createdAtClient","updatedBy","updatedAt","updatedAtClient","storedBy","completedBy","completedAt",
        "enrolledAt",
        "occurredAt",
        "status",
        "followUp",
        "patient_key"
      )
    ), .before = 1)
}

get_enrollment_notes_schema <- function(dataset_options = NULL) {
  check_neoipcr_dhis2_dsopt(dataset_options, allow_null = TRUE)

  schema <- tibble::tibble(
    enrollment_key = integer(),
    value = character()
  )

  if(is.null(dataset_options)){
    return(
      schema |>
        # relocate_enrollments_columns is our reference for column order
        relocate_enrollment_notes_columns()
    )
  }

  if("notes" %in% dataset_options$include_dhis2_ids)
    schema <- schema |>
    dplyr::bind_cols(note = character())

  if(dataset_options$include_user != "no")
    schema <- schema |>
    dplyr::bind_cols(
      createdBy = integer(),
      storedBy = integer()
    )

  if(dataset_options$include_timestamps)
    schema <- schema |>
    dplyr::bind_cols(
      storedAt = as.POSIXct(character(), tz = "UTC")
    )

  schema |>
    relocate_enrollment_notes_columns()
}

ensure_enrollment_notes_schema <- function(x, dataset_options = NULL) {
  vctrs::vec_rbind(get_enrollment_notes_schema(dataset_options), x)
}

relocate_enrollment_notes_columns <- function(x) {
  x |>
    dplyr::relocate(tidyselect::any_of(
      c("enrollment_key",
        "note","createdBy","storedBy","storedAt",
        "value"
      )
    ), .before = 1)
}

get_events_schema <- function(dataset_options = NULL) {
  check_neoipcr_dhis2_dsopt(dataset_options, allow_null = TRUE)

  schema <- tibble::tibble(
    event_key = integer(),
    event_type_key = factor(character(), levels = c("adm","pro","bsi","nec","ssi","hap","end")),
    scheduledAt = as.Date(character()),
    occurredAt = as.Date(character()),
    followUp = logical(),
    patient_key = integer(),
    enrollment_key = integer()
  )

  if(is.null(dataset_options)){
    return(
      schema |>
        # relocate_events_columns is our reference for column order
        relocate_events_columns()
    )
  }

  if("events" %in% dataset_options$include_dhis2_ids)
    schema <- schema |>
    dplyr::bind_cols(event = character())

  if("events" %in% dataset_options$include_incomplete)
    schema <- schema |>
    dplyr::bind_cols(
      status = factor(
        character(),
        levels = c("ACTIVE", "COMPLETED", "VISITED", "SCHEDULE", "OVERDUE", "SKIPPED")))

  if(dataset_options$include_user != "no")
    schema <- schema |>
    dplyr::bind_cols(
      createdBy = integer(),
      updatedBy = integer(),
      completedBy = integer(),
      storedBy = integer()
    )

  if(dataset_options$include_timestamps)
    schema <- schema |>
    dplyr::bind_cols(
      createdAt = as.POSIXct(character(), tz = "UTC"),
      createdAtClient = as.POSIXct(character(), tz = "UTC"),
      updatedAt = as.POSIXct(character(), tz = "UTC"),
      updatedAtClient = as.POSIXct(character(), tz = "UTC"),
      completedAt = as.POSIXct(character(), tz = "UTC")
    )

  if(dataset_options$include_department != "no")
    schema <- schema |>
    dplyr::bind_cols(
      department_key = integer()
    )

  if(dataset_options$include_hospital != "no")
    schema <- schema |>
    dplyr::bind_cols(
      hospital_key = integer()
    )

  if(dataset_options$include_country != "no")
    schema <- schema |>
    dplyr::bind_cols(
      country_key = integer()
    )

  if(dataset_options$include_world_bank_class != "no")
    schema <- schema |>
    dplyr::bind_cols(
      world_bank_class_key = integer()
    )

  schema |>
    relocate_events_columns()
}

ensure_events_schema <- function(x, dataset_options = NULL) {
  vctrs::vec_rbind(get_events_schema(dataset_options), x)
}

relocate_events_columns <- function(x) {
  x |>
    dplyr::relocate(tidyselect::any_of(
      c("event_key","event_type_key",
        "event","createdBy","createdAt","createdAtClient","updatedBy","updatedAt","updatedAtClient","scheduledAt","storedBy","completedBy","completedAt",
        "enrolledAt",
        "occurredAt",
        "status",
        "followUp",
        "patient_key",
        "enrollment_key"
      )
    ), .before = 1)
}

get_event_notes_schema <- function(dataset_options = NULL) {
  check_neoipcr_dhis2_dsopt(dataset_options, allow_null = TRUE)

  schema <- tibble::tibble(
    event_key = integer(),
    value = character()
  )

  if(is.null(dataset_options)){
    return(
      schema |>
        # relocate_event_notes_columns is our reference for column order
        relocate_event_notes_columns()
    )
  }

  if("notes" %in% dataset_options$include_dhis2_ids)
    schema <- schema |>
    dplyr::bind_cols(note = character())

  if(dataset_options$include_user != "no")
    schema <- schema |>
    dplyr::bind_cols(
      createdBy = integer(),
      storedBy = integer()
    )

  if(dataset_options$include_timestamps)
    schema <- schema |>
    dplyr::bind_cols(
      storedAt = as.POSIXct(character(), tz = "UTC")
    )

  schema |>
    relocate_event_notes_columns()
}

ensure_event_notes_schema <- function(x, dataset_options = NULL) {
  vctrs::vec_rbind(get_event_notes_schema(dataset_options), x)
}

relocate_event_notes_columns <- function(x) {
  x |>
    dplyr::relocate(tidyselect::any_of(
      c("event_key",
        "note","createdBy","storedBy","storedAt",
        "value"
      )
    ), .before = 1)
}

get_event_data_column_names <- function(x) {
  lapply(x, \(x)paste0(x,c("","_createdBy","_createdAt","_updatedBy","_updatedAt","_storedBy"))) |> unlist()
}

relocate_event_data_columns <- function(x) {
  x |>
    dplyr::relocate(tidyselect::any_of(
      c("event_key",
        x |>
          dplyr::select(!"event_key") |>
          names() |>
          get_event_data_column_names()
      )
    ), .before = 1)

}

get_event_data_columns <- function(x, dataset_options) {
  if(is.null(dataset_options)) {
    return(x)
  }
  if(dataset_options$include_user != "no") {
    for (c in c("_createdBy","_updatedBy","_storedBy")) {
      x <- x |>
        dplyr::bind_cols(!!c := integer())
    }
  }

  if(dataset_options$include_timestamps) {
    for (c in c("_createdAt","_updatedAt")) {
      x <- x |>
        dplyr::bind_cols(!!c := as.POSIXct(character(), tz = "UTC"))
    }
  }

  x |>
    relocate_event_data_columns()
}

get_admission_event_data_schema <- function(dataset_options = NULL) {
  check_neoipcr_dhis2_dsopt(dataset_options, allow_null = TRUE)

  schema <- tibble::tibble(
    event_key = integer(),
    type = factor(character(), levels = c("1","2","3")),
    dol = integer()
  )

  if(is.null(dataset_options)){
    return(
      schema |>
        # relocate_admission_event_data_columns is our reference for column order
        relocate_admission_event_data_columns()
    )
  }

  if(dataset_options$include_user != "no")
    schema <- schema |>
    dplyr::bind_cols(
      type_createdBy = integer(),
      type_updatedBy = integer(),
      type_storedBy = integer(),
      dol_createdBy = integer(),
      dol_updatedBy = integer(),
      dol_storedBy = integer()
    )

  if(dataset_options$include_timestamps)
    schema <- schema |>
    dplyr::bind_cols(
      type_createdAt = as.POSIXct(character(), tz = "UTC"),
      type_updatedAt = as.POSIXct(character(), tz = "UTC"),
      dol_createdAt = as.POSIXct(character(), tz = "UTC"),
      dol_updatedAt = as.POSIXct(character(), tz = "UTC")
    )

  schema |>
    relocate_admission_event_data_columns()
}

ensure_admission_event_data_schema <- function(x, dataset_options = NULL) {
  vctrs::vec_rbind(get_admission_event_data_schema(dataset_options), x)
}

relocate_admission_event_data_columns <- function(x) {
  x |>
    dplyr::relocate(tidyselect::any_of(
      c("event_key",
        c("type","dol") |> get_event_data_column_names()
      )
    ), .before = 1)
}

get_surveillance_end_event_data_schema <- function(dataset_options = NULL) {
  check_neoipcr_dhis2_dsopt(dataset_options, allow_null = TRUE)

  schema <- tibble::tibble(
    event_key = integer(),
    reason = factor(character(), levels = c("1","2")),
    patient_days = integer(),
    cvc_days = integer(),
    pvc_days = integer(),
    inv_days = integer(),
    niv_days = integer(),
    human_milk_days = integer(),
    kangaroo_care_days = integer(),
    probiotic_days = integer(),
    ab_days = integer()
  )

  if(is.null(dataset_options)){
    return(
      schema |>
        # relocate_surveillance_end_event_data_columns is our reference for column order
        relocate_surveillance_end_event_data_columns()
    )
  }

  if(dataset_options$include_user != "no")
    schema <- schema |>
    dplyr::bind_cols(
      reason_createdBy = integer(),
      reason_updatedBy = integer(),
      reason_storedBy = integer(),
      patient_days_createdBy = integer(),
      patient_days_updatedBy = integer(),
      patient_days_storedBy = integer(),
      cvc_days_createdBy = integer(),
      cvc_days_updatedBy = integer(),
      cvc_days_storedBy = integer(),
      pvc_days_createdBy = integer(),
      pvc_days_updatedBy = integer(),
      pvc_days_storedBy = integer(),
      inv_days_createdBy = integer(),
      inv_days_updatedBy = integer(),
      inv_days_storedBy = integer(),
      niv_days_createdBy = integer(),
      niv_days_updatedBy = integer(),
      niv_days_storedBy = integer(),
      human_milk_days_createdBy = integer(),
      human_milk_days_updatedBy = integer(),
      human_milk_days_storedBy = integer(),
      kangaroo_care_days_createdBy = integer(),
      kangaroo_care_days_updatedBy = integer(),
      kangaroo_care_days_storedBy = integer(),
      probiotic_days_createdBy = integer(),
      probiotic_days_updatedBy = integer(),
      probiotic_days_storedBy = integer(),
      ab_days_createdBy = integer(),
      ab_days_updatedBy = integer(),
      ab_days_storedBy = integer()
    )

  if(dataset_options$include_timestamps)
    schema <- schema |>
    dplyr::bind_cols(
      reason_createdAt = as.POSIXct(character(), tz = "UTC"),
      reason_updatedAt = as.POSIXct(character(), tz = "UTC"),
      patient_days_createdAt = as.POSIXct(character(), tz = "UTC"),
      patient_days_updatedAt = as.POSIXct(character(), tz = "UTC"),
      cvc_days_createdAt = as.POSIXct(character(), tz = "UTC"),
      cvc_days_updatedAt = as.POSIXct(character(), tz = "UTC"),
      pvc_days_createdAt = as.POSIXct(character(), tz = "UTC"),
      pvc_days_updatedAt = as.POSIXct(character(), tz = "UTC"),
      inv_days_createdAt = as.POSIXct(character(), tz = "UTC"),
      inv_days_updatedAt = as.POSIXct(character(), tz = "UTC"),
      niv_days_createdAt = as.POSIXct(character(), tz = "UTC"),
      niv_days_updatedAt = as.POSIXct(character(), tz = "UTC"),
      human_milk_days_createdAt = as.POSIXct(character(), tz = "UTC"),
      human_milk_days_updatedAt = as.POSIXct(character(), tz = "UTC"),
      kangaroo_care_days_createdAt = as.POSIXct(character(), tz = "UTC"),
      kangaroo_care_days_updatedAt = as.POSIXct(character(), tz = "UTC"),
      probiotic_days_createdAt = as.POSIXct(character(), tz = "UTC"),
      probiotic_days_updatedAt = as.POSIXct(character(), tz = "UTC"),
      ab_days_createdAt = as.POSIXct(character(), tz = "UTC"),
      ab_days_updatedAt = as.POSIXct(character(), tz = "UTC")
    )

  schema |>
    relocate_surveillance_end_event_data_columns()
}

ensure_surveillance_end_event_data_schema <- function(x, dataset_options = NULL) {
  vctrs::vec_rbind(get_surveillance_end_event_data_schema(dataset_options), x)
}

relocate_surveillance_end_event_data_columns <- function(x) {
  x |>
    dplyr::relocate(tidyselect::any_of(
      c("event_key",
        c(
          "reason","patient_days","cvc_days","pvc_days","inv_days","niv_days",
          "human_milk_days","kangaroo_care_days","probiotic_days","ab_days"
        ) |> get_event_data_column_names()
      )
    ), .before = 1)
}

get_sepsis_event_data_schema <- function(dataset_options = NULL) {
  check_neoipcr_dhis2_dsopt(dataset_options, allow_null = TRUE)

  schema <- tibble::tibble(
    event_key = integer(),
    dol = integer(),
    los = integer(),
    dev_ass = factor(levels = c("0","1","2")),
    no_pos_culture = logical(),
    ab_treatment = logical(),
    temperature = logical(),
    bradycardia = logical(),
    perfusion = logical(),
    apnoea = logical(),
    feeding_intolerance = logical(),
    irritability = logical(),
    acidosis = logical(),
    hyperglycaemia = logical(),
    platelet_count = logical(),
    wbc = logical(),
    crp = logical(),
    procalcitonin = logical(),
    it_ratio = logical(),
    interleukin = logical()
  ) |>
    get_event_data_columns(dataset_options)
}

ensure_sepsis_event_data_schema <- function(x, dataset_options = NULL) {
  vctrs::vec_rbind(get_sepsis_event_data_schema(dataset_options), x)
}

relocate_sepsis_event_data_columns <- function(x) {
  x |>
    dplyr::relocate(tidyselect::any_of(
      get_sepsis_event_data_schema(NULL) |>
        names()
    ), .before = 1)
}

get_nec_event_data_schema <- function(dataset_options = NULL) {
  check_neoipcr_dhis2_dsopt(dataset_options, allow_null = TRUE)

  schema <- tibble::tibble(
    event_key = integer(),
    dol = integer(),
    los = integer(),
    secondary_bsi = factor(levels = c("1","0","-1")),
    pneumoperitoneum = logical(),
    pneumatosis_intestinalis_img = logical(),
    portal_venous_gas = logical(),
    fixed_loop = logical(),
    abdominal_distension = logical(),
    abdominal_skin_tone = logical(),
    bloody_stools = logical(),
    vomiting = logical(),
    gastric_residuals = logical(),
    bilious_aspirate = logical(),
    bowel_necrosis = logical(),
    pneumatosis_intestinalis_surg = logical()
  ) |>
    get_event_data_columns(dataset_options)
}

ensure_nec_event_data_schema <- function(x, dataset_options = NULL) {
  vctrs::vec_rbind(get_nec_event_data_schema(dataset_options), x)
}

relocate_nec_event_data_columns <- function(x) {
  x |>
    dplyr::relocate(tidyselect::any_of(
      get_nec_event_data_schema(NULL) |>
        names()
    ), .before = 1)
}

get_pneumonia_event_data_schema <- function(dataset_options = NULL) {
  check_neoipcr_dhis2_dsopt(dataset_options, allow_null = TRUE)

  schema <- tibble::tibble(
    event_key = integer(),
    dol = integer(),
    los = integer(),
    dev_ass = factor(levels = c("0","1","2")),
    microbiological_test_result = factor(levels = c("1","0","-1")),
    secondary_bsi = factor(levels = c("1","0","-1")),
    imaging_findings = logical(),
    respiratory_support = logical(),
    bradycardia = logical(),
    tachypnoea = logical(),
    purulent_tracheal_aspirate = logical(),
    respiratory_distress = logical(),
    fever = logical(),
    increased_respiratory_secretion = logical(),
    laboratory_findings = logical()
  ) |>
    get_event_data_columns(dataset_options)
}

ensure_pneumonia_event_data_schema <- function(x, dataset_options = NULL) {
  vctrs::vec_rbind(get_pneumonia_event_data_schema(dataset_options), x)
}

relocate_pneumonia_event_data_columns <- function(x) {
  x |>
    dplyr::relocate(tidyselect::any_of(
      get_pneumonia_event_data_schema(NULL) |>
        names()
    ), .before = 1)
}
