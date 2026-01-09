# Connection options
is_neoipcr_dhis2_conopt <- function(x) inherits(x, "neoipcr_dhis2_conopt")
is_scalar_neoipcr_dhis2_conopt <- function(x) inherits(x, "neoipcr_dhis2_conopt") && rlang::is_scalar_list(x)

# Dataset options
is_neoipcr_dhis2_dsopt <- function(x) inherits(x, "neoipcr_dhis2_dsopt")
is_scalar_neoipcr_dhis2_dsopt <- function(x) inherits(x, "neoipcr_dhis2_dsopt") && rlang::is_scalar_list(x)

# Raw NeoIPC dataset
is_neoipcr_ds <- function(x) inherits(x, "neoipcr_ds")
is_scalar_neoipcr_ds <- function(x) inherits(x, "neoipcr_ds") && rlang::is_scalar_list(x)

# Calculated reference dataset
is_neoipcr_ref_ds <- function(x) inherits(x, "neoipcr_ref_ds")
is_scalar_neoipcr_ref_ds <- function(x) inherits(x, "neoipcr_ref_ds") && rlang::is_scalar_list(x)
