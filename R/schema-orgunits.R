#' @include schema-cols-shared.R
NULL

# Schema declarations for org-unit-derived entities: World Bank income
# classes, countries, hospitals, departments. Loaded after
# `schema-cols-shared.R` so the cross-entity atoms (`col_wb_class_key`,
# `col_country_key`, `col_hospital_key`, `col_department_key`, `col_isTest`)
# and the inheritance helper (`col_inherited_from`) are in scope.
#
# Topological order within this file: WB classes → countries → hospitals →
# departments. A child entity's column list may reference a parent's
# compiled schema via `col_inherited_from(..., parent_cols)`, so parents
# must be declared before their children. Populated incrementally across
# the Phase B sub-tasks; each sub-task extends this file with the next
# entity rather than creating a parallel per-entity file.
#
# Internal — no `@export`.

# ---- World Bank income classes --------------------------------------------
#
# Top of the org-unit hierarchy; no parent, so no inheritance rule applies.
# Each row represents a `{class, fiscal_year}` bucket of countries per the
# World Bank income classification (L = low, LM = lower-middle,
# UM = upper-middle, H = high). The reader narrows to the most recent
# fiscal year that has data in the DHIS2 metadata.
#
# Three-mode shape progression (strict `0 → 1 → N`):
#   "no"     — 0-column, 0-row tibble. Nothing to leak, nothing to join.
#   "pseudo" — single `world_bank_class_key` column; rows = distinct keys
#              surviving upstream filtering. Consumers that want a
#              human-readable label must either render from the key alone
#              or call `pseudonymise_labels(ds, "worldBankClasses")` (to
#              land in a later Phase).
#   "full"   — `world_bank_class_key`, `class`, `fiscal_year`. The `class`
#              factor uses protocol-declared levels (fixed); `fiscal_year`
#              is an integer year.

worldBankClasses_cols <- list(
  col_wb_class_key,
  schema_col(
    "class", factor(),
    include_when  = \(opts) opts$include_world_bank_class == "full",
    factor_levels = c("L", "LM", "UM", "H")
  ),
  schema_col(
    "fiscal_year", integer(),
    include_when = \(opts) opts$include_world_bank_class == "full"
  )
)

get_worldBankClasses_schema <- function(opts)
  compile_schema(worldBankClasses_cols, opts)
