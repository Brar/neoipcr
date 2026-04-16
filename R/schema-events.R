#' @include schema-cols-shared.R schema-enrollments.R
NULL

# Schema declarations for events.
#
# Third fact-layer entity. Follows the three-mode `include_event`
# contract:
#   "no"     — 0×0 (via the entity gate).
#   "pseudo" — strictly `event_key` only.
#   "full"   — `event_key`, `event` (id-opt-in via `"events" %in%
#              include_dhis2_ids`), `occurredAt`, `status` (gated on
#              `"events" %in% include_incomplete`), `event_type_key`,
#              `enrollment_key` (link FK to enrollments, gated on
#              `include_enrollment != "no"`), `patient_key` (secondary
#              link FK to patients, gated on `include_patient != "no"`),
#              and the hierarchy keys (`department_key`, `hospital_key`,
#              `country_key`, `world_bank_class_key`) via direct
#              materialization — see `event_hierarchy_col()` below for
#              the fat-lookup rationale.
#
# Events is a lean tibble: entity-level user / timestamp / deleted /
# followup fields on events belong to a separate `eventDetails` tibble
# (schematized in its own sub-task under phase-b-event-type-data). No
# per-attribute wrapper is needed on events itself — there are no
# per-DE companion columns at this level; those appear on per-event-
# type data tibbles (also phase-b-event-type-data).
#
# `isTest` is declared on events (matches enrollments). The legacy
# reader's `read_events()` actively fetches `isTest` via the
# departments fat-lookup under `include_test_data = TRUE` — dropping
# it in the final `select()` was an accidental omission (the cols
# list includes "isTest", the semi_join / left_join carry it through,
# but the tail select filters it out). The schema treats that as a
# bug and declares `isTest` directly, so downstream consumers that
# need the flag on events (same pattern as on enrollments) don't have
# to detour through `metadata$departments`.
#
# Every non-PK atom predicate ANDs against `include_event == "full"`
# so pseudo mode narrows strictly to `event_key`. The entity gate
# closes "no" mode.

# Direct-materialization helper for event hierarchy keys.
#
# Mirrors the "fat departments" / fat-enrollments deviation: direct
# materialization under the option's own gate, not strict inheritance
# from `enrollments_cols`. Rationale: downstream analytics (calc-rates
# joins built from `names(x$events)` + `intersect(group_cols, ...)`
# — see [R/calc-rates.R:798]) read hierarchy keys directly off events.
# Strict inheritance from enrollments under the full-chain case would
# silently remove these keys and break every such consumer. Same
# pattern as enrollments, same rationale.
event_hierarchy_col <- function(col_name, opts_key, type = integer())
{
  schema_col(
    col_name, type,
    include_when = \(opts)
      opts$include_event == "full" &&
      opts[[opts_key]] != "no"
  )
}

events_cols <- with_entity_gate(
  list(
    col_event_key,

    # Raw DHIS2 event id — id-opt-in axis.
    schema_col(
      "event", character(),
      include_when = \(opts) opts$include_event == "full" &&
                             "events" %in% opts$include_dhis2_ids
    ),

    # Event date — always present under "full" (parsed from the raw
    # `occurredAt` string to Date in the reader).
    schema_col(
      "occurredAt", as.Date(character()),
      include_when = \(opts) opts$include_event == "full"
    ),

    # `status` is protocol-fixed with six levels (covers completed and
    # every pre-completion state). Only present when the caller opted
    # in to incomplete events; otherwise every row is COMPLETED by
    # construction (API filter), and the column is omitted.
    schema_col(
      "status", factor(),
      factor_levels = c(
        "ACTIVE", "COMPLETED", "VISITED", "SCHEDULE", "OVERDUE", "SKIPPED"),
      include_when  = \(opts) opts$include_event == "full" &&
                              "events" %in% opts$include_incomplete
    ),

    # Event type — protocol-fixed factor. Populated by the reader via
    # the orchestrator-internal `.eventTypes_internal_map` regardless
    # of whether `"event_types"` is in `include_dhis2_ids` (the public
    # `metadata$eventTypes` may not carry `programStage`, but the
    # internal map always does).
    schema_col(
      "event_type_key", factor(),
      factor_levels = c("adm", "pro", "bsi", "nec", "ssi", "hap", "end"),
      include_when  = \(opts) opts$include_event == "full"
    ),

    # Link FK to enrollments. Gated by both sides: events exists AND
    # enrollments exists. Under pseudo-event, the link is dropped
    # (strict 1-col progression).
    schema_col(
      "enrollment_key", integer(),
      include_when = \(opts) opts$include_event == "full" &&
                             opts$include_enrollment != "no"
    ),

    # Secondary link FK to patients. Same compound gating — events
    # carries `patient_key` only when patients itself is non-empty.
    # Redundant with the one-hop `enrollment_key → enrollments →
    # patient_key` path, but consumers that group events by patient
    # without going through enrollments read it here.
    schema_col(
      "patient_key", integer(),
      include_when = \(opts) opts$include_event == "full" &&
                             opts$include_patient != "no"
    ),

    # Hierarchy keys via direct materialization (see
    # `event_hierarchy_col()` above for the fat-lookup rationale).
    event_hierarchy_col("department_key",       "include_department"),
    event_hierarchy_col("hospital_key",         "include_hospital"),
    event_hierarchy_col("country_key",          "include_country"),
    event_hierarchy_col("world_bank_class_key",
                        "include_world_bank_class"),

    # `isTest` — populated by the reader via the departments fat-lookup
    # under `include_test_data = TRUE`, same pattern as enrollments.
    # Direct materialization on the same fat-lookup rationale.
    schema_col(
      "isTest", logical(),
      include_when = \(opts) opts$include_event == "full" &&
                             isTRUE(opts$include_test_data)
    )
  ),
  gate = \(opts) opts$include_event != "no"
)

get_events_schema <- function(opts) compile_schema(events_cols, opts)
