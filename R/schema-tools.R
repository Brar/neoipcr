# Schema engine for neoipcr tibbles.
#
# Each column of each schematized tibble is declared as a `schema_col()`
# atom. Entity schemas are lists of atoms. `compile_schema(cols, opts)`
# assembles a 0-row tibble representing the final shape under the given
# `dhis2_dataset_options`. Readers pin factor levels pre-pivot via
# `schema_codes()`, normalize output via `finalize_to_schema()`, and
# assert the tail via `assert_schema()`.
#
# Internal — no `@export`. Used by `R/schema-cols-shared.R` and the
# per-domain `R/schema-*.R` files landed in Phase B.

# Construct a schema_col atom.
#
# name           — column name (character scalar)
# type           — zero-length vector of the column's R type, e.g.
#                  `integer()`, `character()`, `as.POSIXct(character())`.
#                  For factor columns pass `factor()` and supply
#                  `factor_levels`.
# include_when   — `function(opts) logical(1)`. TRUE means the column
#                  appears in the compiled schema under `opts`.
# factor_levels  — NULL, or character vector of levels for factor columns.
# levels_source  — "fixed" (protocol-declared — asserted by assert_schema)
#                  or "data" (data-derived — level list determined by
#                  surviving rows; apply `droplevels()` after filtering;
#                  not asserted in schema).
schema_col <- function(name, type,
                       include_when = function(opts) TRUE,
                       factor_levels = NULL,
                       levels_source = c("fixed", "data"))
{
  if (!is.character(name) || length(name) != 1L)
    rlang::abort("`name` must be a single character string.")
  if (length(type) != 0L)
    rlang::abort("`type` must be a zero-length vector (e.g. `integer()`).")
  if (!is.function(include_when))
    rlang::abort("`include_when` must be a function of `opts`.")
  if (!is.null(factor_levels) && !is.character(factor_levels))
    rlang::abort("`factor_levels` must be NULL or a character vector.")
  levels_source <- rlang::arg_match(levels_source)

  structure(
    list(
      name          = name,
      type          = type,
      include_when  = include_when,
      factor_levels = factor_levels,
      levels_source = levels_source
    ),
    class = "neoipcr_schema_col"
  )
}

# Compile a list of schema_col atoms into a 0-row tibble under `opts`.
# Columns appear in declaration order, filtered by each atom's
# `include_when(opts)`. Factor columns are built with their declared
# levels.
compile_schema <- function(cols, opts)
{
  included <- purrr::keep(cols, \(c) isTRUE(c$include_when(opts)))

  if (length(included) == 0L)
    return(tibble::tibble())

  fields <- purrr::map(included, \(c) {
    if (!is.null(c$factor_levels))
      factor(character(), levels = c$factor_levels)
    else
      c$type
  })
  names(fields) <- purrr::map_chr(included, \(c) c$name)

  tibble::tibble(!!!fields)
}

# Column names from the compiled schema — used to pin factor levels on a
# `pivot_wider(names_from = …)` column via `factor(x, levels = schema_codes(...))`
# before `pivot_wider(..., names_expand = TRUE)`.
#
# For now returns every compiled column name. The events/per-event-type
# phases may refine this to only pivot-produced codes (vs. link keys and
# inherited hierarchy keys) when the need is concrete.
schema_codes <- function(cols, opts)
{
  names(compile_schema(cols, opts))
}

# Loud tail check: assert that `x` matches `compile_schema(cols, opts)`
# exactly — same column names in the same order, same base class
# per column, same factor levels for fixed-levels factors.
# Data-derived-levels factors are not asserted on levels (use
# `droplevels()` to regenerate them post-filter).
assert_schema <- function(x, cols, opts)
{
  expected  <- compile_schema(cols, opts)
  exp_names <- names(expected)
  act_names <- names(x)

  if (!identical(act_names, exp_names))
    rlang::abort(c(
      "Schema mismatch: column names / order differ.",
      "i" = paste("expected:", paste(exp_names, collapse = ", ")),
      "x" = paste("actual:  ", paste(act_names, collapse = ", "))
    ))

  included <- purrr::keep(cols, \(c) isTRUE(c$include_when(opts)))
  col_map  <- stats::setNames(included, purrr::map_chr(included, "name"))

  for (nm in exp_names) {
    if (!identical(class(x[[nm]]), class(expected[[nm]])))
      rlang::abort(c(
        sprintf("Schema mismatch on column `%s`: class differs.", nm),
        "i" = paste("expected:", paste(class(expected[[nm]]), collapse = "/")),
        "x" = paste("actual:  ", paste(class(x[[nm]]), collapse = "/"))
      ))

    if (is.factor(expected[[nm]]) &&
        col_map[[nm]]$levels_source == "fixed" &&
        !identical(levels(x[[nm]]), levels(expected[[nm]])))
      rlang::abort(c(
        sprintf("Schema mismatch on factor column `%s`: levels differ.", nm),
        "i" = paste("expected:", paste(levels(expected[[nm]]), collapse = ", ")),
        "x" = paste("actual:  ", paste(levels(x[[nm]]), collapse = ", "))
      ))
  }

  invisible(x)
}

# Normalize `x` to `compile_schema(cols, opts)`: select declared columns
# in declaration order, drop extras, and apply declared factor levels.
# Errors via `dplyr::select(all_of(...))` if any declared column is
# missing from `x` — the pre-pivot `names_expand = TRUE` contract
# makes missing columns a real bug, not silent drift.
finalize_to_schema <- function(x, cols, opts)
{
  included  <- purrr::keep(cols, \(c) isTRUE(c$include_when(opts)))
  exp_names <- purrr::map_chr(included, "name")

  x <- x |>
    dplyr::select(tidyselect::all_of(exp_names))

  for (c in included) {
    if (!is.null(c$factor_levels))
      x[[c$name]] <- factor(x[[c$name]], levels = c$factor_levels)
  }

  x
}
