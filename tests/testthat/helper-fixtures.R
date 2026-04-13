# Test fixtures and builders for neoipcr tests.
# Auto-loaded by testthat 3.x before test files run.

# Valid values for the `exclude` parameter of read_test_metadata()
.valid_exclusions <- c(
  "system", "program", "program_id", "program_stages",
  "stage_data_elements", "tracked_entity_attributes",
  "countries", "test_units", "antimicrobials"
)

#' Read static JSON fixtures and return processed metadata.
#'
#' Loads fixture files from tests/testthat/fixtures/, merges them into a single
#' metadata list, and passes the result through read_metadata().
#'
#' @param exclude Character vector of components to omit. See .valid_exclusions
#'   for allowed values.
#' @param dataset_options A neoipcr_dhis2_dsopt object (default: all defaults).
#' @return A neoipcr_metadata object.
read_test_metadata <- function(
    exclude = character(),
    dataset_options = dhis2_dataset_options())
{
  bad <- setdiff(exclude, .valid_exclusions)
  if (length(bad) > 0L)
    stop("Unknown exclusion(s): ", paste(bad, collapse = ", "),
         "\nValid values: ", paste(.valid_exclusions, collapse = ", "))

  fixture_path <- testthat::test_path("fixtures")
  read_fixture <- function(name) {
    jsonlite::fromJSON(
      file.path(fixture_path, name),
      simplifyVector = FALSE)
  }

  metadata <- list()

  # --- system ---
  if (!("system" %in% exclude))
    metadata <- utils::modifyList(metadata, read_fixture("system.json"))

  # --- program ---
  if (!("program" %in% exclude)) {
    prog <- read_fixture("program.json")

    if ("program_id" %in% exclude)
      prog$programs[[1L]]$id <- NULL

    if ("program_stages" %in% exclude) {
      prog$programs[[1L]]$programStages <- NULL
    } else if ("stage_data_elements" %in% exclude) {
      prog$programs[[1L]]$programStages <- lapply(
        prog$programs[[1L]]$programStages,
        function(s) { s$programStageDataElements <- NULL; s })
    }

    if ("tracked_entity_attributes" %in% exclude)
      prog$programs[[1L]]$programTrackedEntityAttributes <- NULL

    metadata <- utils::modifyList(metadata, prog)
  }

  # --- org units ---
  if (!("countries" %in% exclude && "test_units" %in% exclude)) {
    ou <- read_fixture("org-units.json")

    if ("countries" %in% exclude)
      ou$organisationUnitGroups <- Filter(
        function(g) g$code != "COUNTRY",
        ou$organisationUnitGroups)

    if ("test_units" %in% exclude)
      ou$organisationUnitGroups <- Filter(
        function(g) g$code != "TEST_UNITS",
        ou$organisationUnitGroups)

    metadata <- utils::modifyList(metadata, ou)
  }

  # --- antimicrobials (options + optionGroupSets) ---
  if (!("antimicrobials" %in% exclude)) {
    am <- read_fixture("antimicrobials.json")
    metadata$options <- c(metadata$options, am$options)
    metadata$optionGroupSets <- c(metadata$optionGroupSets, am$optionGroupSets)
  }

  read_metadata(metadata, dataset_options)
}


#' Construct a minimal structurally valid neoipcr_ds object.
#'
#' Produces a list with the same structure and S3 classes as import_dhis2().
#' All data tibbles default to empty. Override individual slots via named
#' arguments.
#'
#' @param metadata A neoipcr_metadata object (default: read_test_metadata()).
#' @param patients Override the patients tibble.
#' @param enrollments Override the enrollments tibble.
#' @param events Override the events tibble.
#' @param ... Additional named slots merged into the list.
#' @return A list of class c("neoipcr_ds", "list").
make_test_ds <- function(
    metadata    = read_test_metadata(),
    patients    = tibble::tibble(),
    enrollments = tibble::tibble(),
    events      = tibble::tibble(),
    ...)
{
  empty_tbl <- function(cls) {
    structure(tibble::tibble(), class = c(cls, "tbl_df", "tbl", "data.frame"))
  }

  base <- list(
    patients                = structure(patients, class = c("neoipcr_pat", class(patients))),
    enrollments             = structure(enrollments, class = c("neoipcr_enr", class(enrollments))),
    events                  = structure(events, class = c("neoipcr_evt", class(events))),
    eventDetails            = empty_tbl("neoipcr_evd"),
    eventNotes              = NULL,
    admissionData           = empty_tbl("neoipcr_adm"),
    surveillanceEndData     = empty_tbl("neoipcr_end"),
    sepsisData              = empty_tbl("neoipcr_bsi"),
    necData                 = empty_tbl("neoipcr_nec"),
    pneumoniaData           = empty_tbl("neoipcr_hap"),
    surgeryData             = empty_tbl("neoipcr_pro"),
    ssiData                 = empty_tbl("neoipcr_ssi"),
    substanceDays           = empty_tbl("neoipcr_sbd"),
    infectiousAgentFindings = empty_tbl("neoipcr_iaf"),
    metadata                = structure(metadata, class = c("neoipcr_metadata", class(metadata))),
    .cache                  = new.env(parent = emptyenv())
  )

  overrides <- list(...)
  if (length(overrides) > 0L)
    base <- utils::modifyList(base, overrides)

  structure(base, class = c("neoipcr_ds", "list"))
}
