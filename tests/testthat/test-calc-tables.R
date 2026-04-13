# Tests for R/calc-tables.R — per-table unit tests.
# Uses make_calc_test_ds() from helper-fixtures.R.

calc_ds <- make_calc_test_ds()

# --- Figure data builders ---

test_that("get_birthweight_figure_data returns expected structure", {
  result <- neoipcr:::get_birthweight_figure_data(calc_ds)
  expect_type(result, "list")
  expect_true(all(c("density", "frequency", "location_parameters", "scale")
                  %in% names(result)))
  expect_s3_class(result$density, "tbl_df")
  expect_s3_class(result$frequency, "tbl_df")
})

test_that("get_gestational_age_figure_data returns expected structure", {
  result <- neoipcr:::get_gestational_age_figure_data(calc_ds)
  expect_type(result, "list")
  expect_true(all(c("density", "frequency", "location_parameters", "scale")
                  %in% names(result)))
})

# --- Rate tables: each must return a non-empty tibble ---

table_fns <- list(
  list(name = "get_usage_density_rate_table",
       fn = get_usage_density_rate_table, has_q = TRUE),
  list(name = "get_antibiotic_utilisation_table",
       fn = get_antibiotic_utilisation_table, has_q = TRUE),
  list(name = "get_surgery_rate_table",
       fn = get_surgery_rate_table, has_q = FALSE),
  list(name = "get_incidence_density_rate_table",
       fn = get_incidence_density_rate_table, has_q = TRUE),
  list(name = "get_dev_ass_incidence_density_rate_table",
       fn = get_dev_ass_incidence_density_rate_table, has_q = TRUE),
  list(name = "get_infectious_agent_detection_rate_per_inf_type_table",
       fn = get_infectious_agent_detection_rate_per_inf_type_table, has_q = TRUE),
  list(name = "get_infectious_agent_detection_rate_per_agent_table",
       fn = get_infectious_agent_detection_rate_per_agent_table, has_q = TRUE),
  list(name = "get_abr_infection_rate_table",
       fn = get_abr_infection_rate_table, has_q = TRUE),
  list(name = "get_organism_resistance_rate_table",
       fn = get_organism_resistance_rate_table, has_q = TRUE),
  list(name = "get_resistance_test_rate_table",
       fn = get_resistance_test_rate_table, has_q = TRUE),
  list(name = "get_secondary_bsi_rate_table",
       fn = get_secondary_bsi_rate_table, has_q = TRUE)
)

for (entry in table_fns) {
  local({
    nm <- entry$name
    fn <- entry$fn
    hq <- entry$has_q

    test_that(paste0(nm, " returns a tibble"), {
      if (hq)
        result <- fn(calc_ds, use_cache = FALSE, include_quartiles = FALSE)
      else
        result <- fn(calc_ds, use_cache = FALSE)
      expect_s3_class(result, "tbl_df")
      expect_true(ncol(result) > 0L)
    })
  })
}

# --- Numerical spot-check: usage density CVC ---

test_that("usage_density_rate_table CVC days match fixture", {
  result <- get_usage_density_rate_table(calc_ds, use_cache = FALSE,
    include_quartiles = FALSE)
  cvc_row <- result[result$factor == "cvc", ]
  # 3 enrollments × 3 cvc_days each = 9
  expect_equal(cvc_row$n, 9L)
})

test_that("usage_density_rate_table has 12 factor rows", {
  result <- get_usage_density_rate_table(calc_ds, use_cache = FALSE,
    include_quartiles = FALSE)
  expect_equal(nrow(result), 12L)
})
