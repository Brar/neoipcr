is_neoipcr_ds <- function(x) inherits(x, "neoipcr_ds")
is_scalar_neoipcr_ds <- function(x) inherits(x, "neoipcr_ds") && rlang::is_scalar_list(x)
is_neoipcr_dhis2_conopt <- function(x) inherits(x, "neoipcr_dhis2_conopt")
is_scalar_neoipcr_dhis2_conopt <- function(x) inherits(x, "neoipcr_dhis2_conopt") && rlang::is_scalar_list(x)
is_neoipcr_dhis2_dsopt <- function(x) inherits(x, "neoipcr_dhis2_dsopt")
is_scalar_neoipcr_dhis2_dsopt <- function(x) inherits(x, "neoipcr_dhis2_dsopt") && rlang::is_scalar_list(x)
is_neoipcr_rep_ds <- function(x) inherits(x, "neoipcr_rep_ds")
is_scalar_neoipcr_rep_ds <- function(x) inherits(x, "neoipcr_rep_ds") && rlang::is_scalar_list(x)
is_neoipcr_ref_ds <- function(x) inherits(x, "neoipcr_ref_ds")
is_scalar_neoipcr_ref_ds <- function(x) inherits(x, "neoipcr_ref_ds") && rlang::is_scalar_list(x)
is_neoipcr_bnch_ds <- function(x) inherits(x, "neoipcr_bnch_ds")
is_scalar_neoipcr_bnch_ds <- function(x) inherits(x, "neoipcr_bnch_ds") && rlang::is_scalar_list(x)

is_neoipcr_pat <- function(x, dataset_options = NULL) {
  if (!inherits(x, "neoipcr_pat")) {
    return(FALSE)
  }

  schema <- get_patients_schema(dataset_options)
  x_names <- names(x)
  schema_names <- names(schema)

  # 1. Missing columns
  if (length(setdiff(schema_names, x_names)) > 0) {
    return(FALSE)
  }

  # 2. Unexpected columns
  if (length(setdiff(x_names, schema_names)) > 0) {
    return(FALSE)
  }

  # 3. Type compatibility (strict, via vctrs)
  for (nm in schema_names) {
    ok <- tryCatch({
      vctrs::vec_cast(x[[nm]], schema[[nm]])
      TRUE
    }, error = function(e) FALSE)

    if (!ok) return(FALSE)
  }

  TRUE
}
