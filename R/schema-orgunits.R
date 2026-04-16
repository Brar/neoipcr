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

worldBankClasses_cols <- with_entity_gate(
  list(
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
  ),
  gate = \(opts) opts$include_world_bank_class != "no"
)

get_worldBankClasses_schema <- function(opts)
  compile_schema(worldBankClasses_cols, opts)

# ---- Countries ------------------------------------------------------------
#
# Second tier of the org-unit hierarchy. Each country belongs to exactly
# one World Bank income class (via the WB-class membership lookup), so
# `world_bank_class_key` is the direct parent-link FK — not an inherited
# key. The plan's `col_inherited_from()` helper is for hierarchy keys
# *further up* the chain that the immediate parent might or might not
# carry (e.g. patients inheriting `country_key` from departments). For a
# direct parent-child link the FK is *always* present when both sides
# exist; per plan.md's "gated on both sides of the link" rule for link
# FKs. The shared `col_wb_class_key` atom encodes the "WB side exists"
# half (its predicate is `include_world_bank_class != "no"`); the
# "countries side exists" half is supplied by the containing-entity gate
# declared below via `with_entity_gate()`.
#
# Display columns are ordered factors with data-derived levels, matching
# the current reader's `dplyr::across(!"id", ordered)` conversion. Under
# the three-mode contract the reader's `finalize_to_schema()` narrows
# any extra columns (e.g. the intermediate `country` DHIS2 id used for
# orchestrator-level joins) that aren't in the public schema.
#
# Three-mode shape:
#   "no"     — 0×0 tibble (via the entity gate's short-circuit).
#   "pseudo" — `country_key` only, plus `world_bank_class_key` when
#              `include_world_bank_class != "no"` (direct link-FK).
#   "full"   — adds `code`, `displayName`, `displayShortName`,
#              `displayDescription`.

countries_cols <- with_entity_gate(
  list(
    col_country_key,
    schema_col(
      "code", ordered(),
      include_when  = \(opts) opts$include_country == "full",
      levels_source = "data"
    ),
    schema_col(
      "displayName", ordered(),
      include_when  = \(opts) opts$include_country == "full",
      levels_source = "data"
    ),
    schema_col(
      "displayShortName", ordered(),
      include_when  = \(opts) opts$include_country == "full",
      levels_source = "data"
    ),
    schema_col(
      "displayDescription", ordered(),
      include_when  = \(opts) opts$include_country == "full",
      levels_source = "data"
    ),
    col_wb_class_key
  ),
  gate = \(opts) opts$include_country != "no"
)

get_countries_schema <- function(opts)
  compile_schema(countries_cols, opts)

# ---- Hospitals ------------------------------------------------------------
#
# Third tier of the org-unit hierarchy. Each hospital belongs to exactly
# one country (via the DHIS2 parent-org-unit link). Inherits
# `world_bank_class_key` from countries per the hierarchy-key
# inheritance rule: hospitals carries it directly only when the
# immediate parent (countries) doesn't. Under `include_country != "no"`,
# countries carries `world_bank_class_key` (either directly or via its
# own inheritance), so hospitals does not duplicate it — downstream
# analyses reach WB class via `hospital.country_key → country.world_bank_class_key`.
# Under `include_country = "no"`, countries is 0×0, and hospitals
# materializes `world_bank_class_key` directly to preserve
# classifiability; the orchestrator populates it using the raw
# WB-class→country membership map.
#
# Display / geometry columns are character and numeric (not factors),
# matching the current reader's passthrough (it does not ordered-cast
# display strings for hospitals).
#
# Three-mode shape:
#   "no"     — 0×0 tibble (via the entity gate).
#   "pseudo" — `hospital_key`, `orgUnit` (iff `"hospitals" %in% include_dhis2_ids`),
#              `country_key` (iff include_country != "no"), and `world_bank_class_key`
#              (by inheritance when countries doesn't carry it).
#   "full"   — adds `code`, `displayName`, `displayShortName`,
#              `displayDescription`, `comment`, `longitude`, `latitude`.

hospitals_cols <- with_entity_gate(
  list(
    col_hospital_key,
    schema_col(
      "orgUnit", character(),
      include_when = \(opts) "hospitals" %in% opts$include_dhis2_ids
    ),
    schema_col(
      "code", character(),
      include_when = \(opts) opts$include_hospital == "full"
    ),
    schema_col(
      "displayName", character(),
      include_when = \(opts) opts$include_hospital == "full"
    ),
    schema_col(
      "displayShortName", character(),
      include_when = \(opts) opts$include_hospital == "full"
    ),
    schema_col(
      "displayDescription", character(),
      include_when = \(opts) opts$include_hospital == "full"
    ),
    schema_col(
      "comment", character(),
      include_when = \(opts) opts$include_hospital == "full"
    ),
    schema_col(
      "longitude", double(),
      include_when = \(opts) opts$include_hospital == "full"
    ),
    schema_col(
      "latitude", double(),
      include_when = \(opts) opts$include_hospital == "full"
    ),
    col_country_key,
    col_inherited_from(
      "world_bank_class_key",
      "include_world_bank_class",
      countries_cols
    )
  ),
  gate = \(opts) opts$include_hospital != "no"
)

get_hospitals_schema <- function(opts)
  compile_schema(hospitals_cols, opts)
