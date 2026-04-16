get_metadata <- function(d2_req_base, user_info, dataset_options)
{
  md_req_base <- d2_req_base |>
    httr2::req_url_query(
      paging = "false",
      translate = tolower(dataset_options$translate))

  if(dataset_options$translate &&
     rlang::is_character(dataset_options$locale, n = 1))
    md_req_base <- md_req_base |>
      httr2::req_url_query(
        locale = dataset_options$locale)

  requests <- list(
    get_metadata_request(md_req_base, user_info, dataset_options),
    get_organisationUnit_request(md_req_base, user_info, dataset_options)) |>
    httr2::req_perform_parallel(on_error = "continue") |>
    read_metadata_reponses(user_info, dataset_options)
}

# Creates the overall query to get most of the NeoIPC-related metadata that
# every NeoIPC user should be allowed to see
get_metadata_request <- function(req_base, user_info, dataset_options)
{
  req <- req_base |>
    httr2::req_url_path_append("metadata")

  if(length(dataset_options$trial_keys) > 0 ||
     dataset_options$include_world_bank_class != "no")
  {
    if(length(dataset_options$trial_keys) == 0)
    {
      if(dataset_options$include_world_bank_class == "full")
        req <- req |>
          httr2::req_url_query(
            `organisationUnitGroupSets:fields` = "code,organisationUnitGroups[code,displayName,displayShortName,displayDescription,organisationUnits[id]]")
      else # pseudonymise
        req <- req |>
          httr2::req_url_query(
            `organisationUnitGroupSets:fields` = "code,organisationUnitGroups[code,organisationUnits[id]]")

      req <- req |>
        httr2::req_url_query(
          `organisationUnitGroupSets:filter` = "code:eq:WORLD_BANK_CLASSES")
    }
    else
    {
      req <- req |>
        httr2::req_url_query(
          `organisationUnitGroupSets:fields` = "code,organisationUnitGroups[code,displayName,displayShortName,displayDescription,organisationUnits[id]]")

      if(dataset_options$include_world_bank_class == "no")
        req <- req |>
          httr2::req_url_query(
            `organisationUnitGroupSets:filter` = "code:eq:NEOIPC_TRIALS")
      else
        req <- req |>
          httr2::req_url_query(
            `organisationUnitGroupSets:filter` = "code:in:[NEOIPC_TRIALS,WORLD_BANK_CLASSES]")
    }
  }

  if(length(dataset_options$country_filter) > 0 ||
     dataset_options$include_country != "no" ||
     length(dataset_options$country_filter) > 0 ||
     dataset_options$include_world_bank_class != "no")
  {
    if(dataset_options$include_country == "full")
      req <- req |>
        httr2::req_url_query(
          `organisationUnitGroups:fields` = "code,organisationUnits[id,code,displayName,displayShortName,displayDescription]")
    else if(length(dataset_options$country_filter) > 0)
      req <- req |>
        httr2::req_url_query(
          `organisationUnitGroups:fields` = "code,organisationUnits[id,code]")
    else # pseudonymised, or include_world_bank_class != "no"
      req <- req |>
        httr2::req_url_query(
          `organisationUnitGroups:fields` = "code,organisationUnits[id]")

    req <- req |>
      httr2::req_url_query(
        `organisationUnitGroups:filter` = "code:in:[COUNTRY,TEST_UNITS]")
  }
  else
    req <- req |>
      httr2::req_url_query(
        `organisationUnitGroups:fields` = "code,organisationUnits[id]",
        `organisationUnitGroups:filter` = "code:eq:TEST_UNITS")

  req <- req |>
    httr2::req_url_query(
      `programs:fields` = "id,programTrackedEntityAttributes[trackedEntityAttribute[id,valueType,code,displayName,displayShortName,displayFormName,displayDescription,optionSet[code]]],programStages[id,name,displayName,displayFormName,displayDescription,programStageDataElements[dataElement[id,valueType,code,displayName,displayShortName,displayFormName,displayDescription,optionSet[code]]]]",
      `programs:filter` = "code:eq:NEOIPC_CORE",
      `trackedEntityTypes:fields` = "id",
      `trackedEntityTypes:filter` = "name:eq:NeoIPC Patient",
      `optionGroupSets:fields` = "code,optionGroups[code,displayName,displayShortName,displayDescription,options[code]]",
      `optionGroupSets:filter` = "code:in:[ATC5,WHO_AWARE]",
      `options:fields` = "code,displayName,displayFormName,displayDescription,sortOrder,optionSet[code]",
      `options:filter` = "optionSet.code:in:[NEOIPC_ASA_SCORE,NEOIPC_ADMISSION_TYPES,NEOIPC_ANTIMICROBIAL_SUBSTANCES,NEOIPC_BSI_DEVICE_ASS,NEOIPC_BSI_PATHOGEN_RECOVERED_FROM,NEOIPC_DELIVERY_MODES,NEOIPC_HAP_DEVICE_ASS,NEOIPC_HAP_RESPIRATORY_TRACT_SAMPLE_SOURCES,NEOIPC_SSI_TYPE,NEOIPC_SEX_VALUES,NEOIPC_SURVEILLANCE_END_REASON,NEOIPC_WOUND_CLASSES,NEOIPC_YES_NO_NO_FOLLOWUP,NEOIPC_YES_NO_NOT_TESTED]"
    )

  # We only read the complete user information via the metadata endpoint if we
  # have the required authorities to do so
  if (dataset_options$include_user != "no" && length(intersect(c("ALL","F_METADATA_EXPORT","F_USER_VIEW"), user_info$authorities)) > 0) {
    if(dataset_options$include_user == "full")
      req <- req |>
        httr2::req_url_query(
          `users:fields` = "id,username,firstName,surname,email,created,lastLogin,organisationUnits[id],dataViewOrganisationUnits[id],teiSearchOrganisationUnits[id],userRoles[id]")
    else # pseudonymise
      req <- req |>
        httr2::req_url_query(
          `users:fields` = "id,username")
  }
  req
}

read_metadata_reponses <- function(resps, user_info, dataset_options)
{
  metadata <- resps |>
    lapply(read_metadata_reponse, dataset_options) |>
    unlist(recursive = FALSE)

  # `metadata$.countries_internal_map` is the orchestrator-internal
  # countries lookup — it carries the raw DHIS2 `country` id + `code` +
  # `country_key` used by every post-read country/hospital/department
  # join that can't reach into `metadata$countries` (which is the
  # schema-conformant public tibble without the raw id or `code`). The
  # field is kept on `metadata` through the rest of `import_dhis2()` so
  # that request-building code (e.g. country_filter → event request
  # URLs) can consume it, and stripped at the `import_dhis2()` exit
  # just before the final dataset is assembled.

  if (!("users" %in% names(metadata)))
    metadata$users <- read_user_info_table(
      user_info,
      dataset_options$include_user)

  if(dataset_options$include_test_data)
    metadata$departments <- metadata$departments |>
      dplyr::mutate(isTest = .data$orgUnit %in% metadata$testUnitIds)
  else
    metadata$departments <- metadata$departments |>
      dplyr::filter(!(.data$orgUnit %in% metadata$testUnitIds))

  # Filter departments by department_filter
  if (length(dataset_options$department_filter) > 0)
    metadata$departments <- metadata$departments |>
      dplyr::filter(.data$code %in% dataset_options$department_filter)

  # Filter countries by country_filter and remove departments not in those countries
  if (length(dataset_options$country_filter) > 0 &&
      !is.null(metadata$.countries_internal_map))
  {
    surviving_keys <- metadata$.countries_internal_map |>
      dplyr::filter(.data$code %in% dataset_options$country_filter) |>
      dplyr::pull("country_key")

    # Narrow the public tibble only when it actually carries the key
    # (i.e. include_country != "no"). The map is always narrowed so
    # downstream joins see the filtered country set.
    if ("country_key" %in% names(metadata$countries))
      metadata$countries <- metadata$countries |>
        dplyr::filter(.data$country_key %in% surviving_keys)

    metadata$.countries_internal_map <- metadata$.countries_internal_map |>
      dplyr::filter(.data$country_key %in% surviving_keys)

    if (!is.null(metadata$hospitals) &&
        "hospital_key" %in% names(metadata$departments))
    {
      filtered_hospital_keys <- metadata$hospitals |>
        dplyr::semi_join(
          metadata$.countries_internal_map, dplyr::join_by("country")) |>
        dplyr::pull("hospital_key")

      if (dataset_options$include_test_data &&
          "isTest" %in% names(metadata$departments))
        metadata$departments <- metadata$departments |>
          dplyr::filter(
            .data$isTest | .data$hospital_key %in% filtered_hospital_keys)
      else
        metadata$departments <- metadata$departments |>
          dplyr::filter(.data$hospital_key %in% filtered_hospital_keys)
    }
  }

  # Filter hospitals to only those with remaining departments
  if(dataset_options$include_hospital != "no" ||
     length(dataset_options$country_filter) > 0 ||
     dataset_options$include_country != "no" ||
     dataset_options$include_world_bank_class != "no")
  {
    metadata$hospitals <- metadata$hospitals |>
      dplyr::semi_join(
        metadata$departments |>
          dplyr::select("hospital_key"),
        dplyr::join_by("hospital_key"))
  }

  # Join country_key into hospitals via the raw `country` id held by the
  # orchestrator-internal map. The public countries tibble no longer
  # carries `country` under the schema contract, so the join has to
  # consume the map. `metadata$hospitals` is still the reader's
  # `processed` tibble at this point (narrowed to the public schema
  # below, after all joins run).
  if ((dataset_options$include_country != "no" ||
       length(dataset_options$country_filter) > 0 ||
       dataset_options$include_world_bank_class != "no") &&
      !is.null(metadata$.countries_internal_map) &&
      "country" %in% names(metadata$hospitals))
  {
    metadata$hospitals <- metadata$hospitals |>
      dplyr::left_join(
        metadata$.countries_internal_map |>
          dplyr::select("country", "country_key"),
        dplyr::join_by("country")) |>
      dplyr::select(!"country")
  }

  # Hospitals WB-class inheritance: under `include_country = "no"` +
  # `include_world_bank_class != "no"`, countries is 0×0 so it can't
  # relay `world_bank_class_key`. The inheritance rule makes hospitals
  # carry the key directly; populate it from the raw WB-class →
  # country-id membership map held in `.wb_country_map`, joined through
  # the hospitals internal map's `country` column.
  if (dataset_options$include_world_bank_class != "no" &&
      dataset_options$include_country == "no" &&
      !is.null(metadata$.wb_country_map) &&
      !is.null(metadata$.hospitals_internal_map) &&
      "country" %in% names(metadata$.hospitals_internal_map))
  {
    wb_country_lookup <- metadata$.wb_country_map |>
      tidyr::unnest_longer("organisationUnits") |>
      tidyr::hoist("organisationUnits", country = list(1L)) |>
      dplyr::select("country", "world_bank_class_key")

    wb_hospital_lookup <- metadata$.hospitals_internal_map |>
      dplyr::left_join(wb_country_lookup, dplyr::join_by("country")) |>
      dplyr::select("hospital_key", "world_bank_class_key")

    metadata$hospitals <- metadata$hospitals |>
      dplyr::left_join(wb_hospital_lookup, dplyr::join_by("hospital_key"))
  }

  # Narrow `metadata$hospitals` from the reader's `processed` tibble to
  # the public three-mode shape declared by `hospitals_cols`. The
  # containing-entity gate on `hospitals_cols` short-circuits to 0×0
  # under `include_hospital = "no"`, dropping every internal-only
  # column in one step. Tail `assert_schema()` confirms the result.
  metadata$hospitals <- metadata$hospitals |>
    finalize_to_schema(hospitals_cols, dataset_options)
  assert_schema(metadata$hospitals, hospitals_cols, dataset_options)

  # Pre-join hierarchy into departments so that read_patients/enrollments/events
  # can use a single flat left_join instead of cascading joins.
  # Gate on column presence — under `include_hospital = "no"` hospitals
  # is 0×0 and the hospitals-relay join is skipped; under
  # `include_country = "no"` countries is 0×0 and the wb_class_key relay
  # is skipped. `departments_cols` declares the relay columns only under
  # `include_department = "full"`, so `finalize_to_schema()` below drops
  # them in pseudo mode regardless of whether the join fired.
  if ("hospital_key" %in% names(metadata$departments) &&
      all(c("hospital_key", "country_key") %in% names(metadata$hospitals)))
  {
    metadata$departments <- metadata$departments |>
      dplyr::left_join(
        metadata$hospitals |>
          dplyr::select("hospital_key", "country_key"),
        dplyr::join_by("hospital_key"))

    if ("world_bank_class_key" %in% names(metadata$countries))
      metadata$departments <- metadata$departments |>
        dplyr::left_join(
          metadata$countries |>
            dplyr::select("country_key", "world_bank_class_key"),
          dplyr::join_by("country_key"))
  }

  # Narrow `metadata$departments` from the reader's working tibble to
  # the public three-mode shape declared by `departments_cols`. Tail
  # `assert_schema()` confirms. Under `include_department = "no"` the
  # entity-gate short-circuits to 0×0 regardless of any columns still
  # present from the raw reader path.
  metadata$departments <- metadata$departments |>
    finalize_to_schema(departments_cols, dataset_options)
  assert_schema(metadata$departments, departments_cols, dataset_options)

  metadata$testUnitIds <- NULL

  metadata
}

read_metadata_reponse <- function(resp, dataset_options)
{
  path <- httr2::resp_url_path(resp)
  json <- httr2::resp_body_json(resp)

  if(stringr::str_ends(path, "/metadata"))
    return(json |> read_metadata(dataset_options))
  else if(stringr::str_ends(path, "/organisationUnits"))
    return(json |> read_organisationUnits(dataset_options))

  rlang::abort("Unexpected DHIS2 metadata response.")
}

read_metadata <- function(metadata, dataset_options)
{
  system <- read_metadata_system(metadata)
  programId <- read_metadata_program_id(metadata)
  trackedEntityTypeId <- metadata$trackedEntityTypes |>
    unlist(use.names = FALSE)
  eventTypes <- read_metadata_programStages(metadata)
  dataElements <- read_metadata_dataElements(metadata)
  options <- read_metadata_options(metadata)
  admissionTypes <- read_metadata_admissionTypes(options)
  asaScores <- read_metadata_asaScores(options)
  sepsisDeviceAssociation <- read_metadata_sepsisDeviceAssociation(options)
  sepsisPathogenSources  <- read_metadata_sepsis_pathogen_sources(options)
  deliveryModes <- read_metadata_deliveryModes(options)
  pneumoniaDeviceAssociation <- read_metadata_pneumoniaDeviceAssociation(
    options)
  pneumoniaPathogenSources <- read_metadata_pneumonia_pathogen_sources(options)
  sexes <- read_metadata_sexes(options)
  ssiTypes <- read_metadata_ssiTypes(options)
  surveillanceEndReasons <- read_metadata_surveillanceEndReasons(options)
  woundClasses <- read_metadata_woundClasses(options)
  testResults <- read_metadata_testResults(options)
  surveillanceResults <- read_metadata_surveillanceResults(options)
  trackedEntityAttributes <- read_metadata_trackedEntityAttributes(metadata)
  antimicrobialSubstances <- read_metadata_AntimicrobialSubstances(metadata)
  awareCategories <- read_metadata_AWaReCategories(metadata)
  atc5Categories <- read_metadata_atc5Categories(metadata)
  testUnitIds <- read_metadata_test_unit_ids(
    metadata, dataset_options$include_test_data)

  users <- read_metadata_users(
    metadata,
    dataset_options$include_user)

  trials <- read_metadata_trials(
    metadata,
    dataset_options$trial_keys)

  wb_result <- read_metadata_wb_classes(metadata, dataset_options)
  world_bank_classes <- wb_result$public
  wb_country_map     <- wb_result$country_map

  countries_result <- read_metadata_countries(
    metadata, dataset_options, wb_country_map)
  countries              <- countries_result$public
  countries_internal_map <- countries_result$internal_map

  ret <- list(
    system = system,
    programId = programId,
    trackedEntityTypeId = trackedEntityTypeId,
    eventTypes = eventTypes,
    options = options,
    dataElements = dataElements,
    trackedEntityAttributes = trackedEntityAttributes,
    antimicrobialSubstances = antimicrobialSubstances,
    awareCategories = awareCategories,
    atc5Categories = atc5Categories,
    testUnitIds = testUnitIds,
    admissionTypes = admissionTypes,
    asaScores = asaScores,
    sepsisDeviceAssociation = sepsisDeviceAssociation,
    sepsisPathogenSources = sepsisPathogenSources,
    deliveryModes = deliveryModes,
    pneumoniaDeviceAssociation = pneumoniaDeviceAssociation,
    pneumoniaPathogenSources = pneumoniaPathogenSources,
    sexes = sexes,
    ssiTypes = ssiTypes,
    surveillanceEndReasons = surveillanceEndReasons,
    woundClasses = woundClasses,
    testResults = testResults,
    surveillanceResults = surveillanceResults
  )

  if(!is.null(users))
    ret <- c(ret, list(users = users))
  if(!is.null(trials))
    ret <- c(ret, list(trials = trials))
  # `world_bank_classes` is always a tibble (never NULL) — the three-mode
  # shape is the signal: 0×0 under "no", 1-col under "pseudo", full schema
  # under "full". See `R/schema-orgunits.R::worldBankClasses_cols`.
  ret <- c(ret, list(worldBankClasses = world_bank_classes))
  # `countries` follows the same three-mode contract via
  # `R/schema-orgunits.R::countries_cols`. The orchestrator-internal
  # `countries_internal_map` (with raw DHIS2 `country` id + `code` used
  # by post-read joins and `country_filter`) is threaded through via
  # `.countries_internal_map` and stripped at the top of the multi-response
  # orchestrator's post-processing.
  ret <- c(ret, list(countries = countries))
  if (!is.null(countries_internal_map))
    ret <- c(ret, list(.countries_internal_map = countries_internal_map))
  # `.wb_country_map` is the raw WB-class → country-id membership
  # lookup. Threaded through so `read_metadata_reponses()` can populate
  # `world_bank_class_key` on hospitals under the inheritance case
  # (`include_country = "no"` + `include_world_bank_class != "no"`)
  # where the countries tibble is empty and can't serve as a join
  # relay. Stripped at `import_dhis2()` exit.
  if (!is.null(wb_country_map))
    ret <- c(ret, list(.wb_country_map = wb_country_map))

  ret
}
