quartile_probs <- c(0.25,0.5,0.75)

#' Calculate a NeoIPC reference data set
#'
#' @param x The neoipcr_ds object containing the data
#' @param use_cache Use the cache
#' @param redact Redact potentially sensitive information
#'
#' @returns A NeoIPC reference data set
#' @export
calculate_reference_data <- function(x, use_cache = TRUE, redact = TRUE)
{
  check_neoipcr_ds(x)

  if(is.null(x$enrollments$department_key))
    rlang::abort("Cannot calculate reference data without department information. You need to include at least pseudonymised department information.")

  if(is.null(x$metadata$countries))
    rlang::warn("The data is missing country metadata. The resulting dataset connot be used to create reference reports")

  pd <- get_risk_time(x, use_cache = use_cache)$patient_days
  pd_dept <- get_risk_time(
    x,
    group_cols = "department_key",
    use_cache = use_cache)$patient_days
  pd_q <- pd_dept |>
    stats::quantile(probs = quartile_probs) |>
    as.integer()

  rp <- x |>
    get_risk_population(use_cache = use_cache)
  rp_dept <- x |>
    get_risk_population(group_cols = "department_key", use_cache = use_cache)
  pat_q <- rp_dept$n_patients |>
    stats::quantile(probs = quartile_probs) |>
    as.integer()
  enr_q <- rp_dept$n_enrollments |>
    stats::quantile(probs = quartile_probs) |>
    as.integer()

  sr <- x |>
    get_surgery_risk(use_cache = use_cache)

  sr_dept <- x |>
    get_surgery_risk(
      group_cols = "department_key",
      use_cache = use_cache) |>
    dplyr::full_join(
      x$enrollments |>
        dplyr::select("department_key") |>
        dplyr::distinct(),
      dplyr::join_by("department_key")) |>
    dplyr::mutate(
      n_patients = tidyr::replace_na(.data$n_patients, 0L),
      n_procedures = tidyr::replace_na(.data$n_procedures, 0L))
  sur_pat_q <- sr_dept$n_patients |>
    stats::quantile(probs = quartile_probs) |>
    as.integer()
  sur_proc_q <- sr_dept$n_procedures |>
    stats::quantile(probs = quartile_probs) |>
    as.integer()

  ds_opts <- x$metadata$dataset_options
  if(redact && typeof(ds_opts$include_invalid_patients) != "logical")
    ds_opts$include_invalid_patients <- "redacted"

  n_infections <- dplyr::bind_rows(
    dplyr::bind_cols(
      tibble::tibble(event_type_key = "overall"),
      x |>
        get_infection_counts() |>
        dplyr::bind_cols(
          x |>
            get_infection_counts(group_cols = "department_key") |>
            dplyr::summarise(
              pooled_mean = as.integer(round(mean(.data$n))),
              q = list(
                stats::quantile(.data$n, quartile_probs, names = FALSE))) |>
            tidyr::unnest_wider(q, names_sep = "", transform = as.integer))),
    x |>
      get_infection_counts(group_cols = c("event_type_key")) |>
      dplyr::inner_join(
        x |>
          get_infection_counts(
            group_cols = c("department_key", "event_type_key")) |>
          dplyr::group_by(.data$event_type_key) |>
          dplyr::summarise(
            pooled_mean = as.integer(round(mean(.data$n))),
            q = list(
              stats::quantile(.data$n, quartile_probs, names = FALSE))) |>
          tidyr::unnest_wider(q, names_sep = "", transform = as.integer),
        dplyr::join_by("event_type_key"))) |>
    dplyr::rename(inf_type = "event_type_key", total = "n")

  structure(
    list(
      metadata = list(
        calculated = lubridate::now("UTC"),
        dataset_options = ds_opts,
        data_up_to = x$metadata$system$date,
        countries = x$metadata$countries$displayName |> sort()
      ),
      birth_weight_figure = x|> get_birthweight_figure_data(),
      gestational_age_figure = x|> get_gestational_age_figure_data(),
      n_departments = x$enrollments$department_key |> unique() |> length(),
      n_patients = tibble::tibble(
        total = rp$n_patients,
        pooled_mean = rp_dept$n_patients |>
          mean() |>
          round() |>
          as.integer(),
        q1 = pat_q[1],
        q2 = pat_q[2],
        q3 = pat_q[3]),
      n_enrollments = tibble::tibble(
        total = rp$n_enrollments,
        pooled_mean = rp_dept$n_enrollments |>
          mean() |>
          round() |>
          as.integer(),
        q1 = enr_q[1],
        q2 = enr_q[2],
        q3 = enr_q[3]),
      n_patient_days = tibble::tibble(
        total = pd,
        pooled_mean = pd_dept |>
          mean() |>
          round() |>
          as.integer(),
        q1 = pd_q[1],
        q2 = pd_q[2],
        q3 = pd_q[3]),
      n_surgical_departments = sr$n_departments,
      n_surgical_patients = tibble::tibble(
        total = sr$n_patients,
        pooled_mean = sr_dept$n_patients |>
          mean() |>
          round() |>
          as.integer(),
        q1 = sur_pat_q[1],
        q2 = sur_pat_q[2],
        q3 = sur_pat_q[3]),
      n_surgical_procedures = tibble::tibble(
        total = sr$n_procedures,
        pooled_mean = sr_dept$n_procedures |>
          mean() |>
          round() |>
          as.integer(),
        q1 = sur_proc_q[1],
        q2 = sur_proc_q[2],
        q3 = sur_proc_q[3]),
      n_infections = n_infections,
      usage_density_rate_table =
        get_usage_density_rate_table(x, use_cache),
      surgery_rate_table =
        get_ref_surgery_rate_table(x, use_cache),
      incidence_density_rate_table =
        get_incidence_density_rate_table(x, use_cache),
      dev_ass_incidence_density_rate_table =
        get_dev_ass_incidence_density_rate_table(x, use_cache),
      infectious_agent_detection_rate_per_agent_table =
        get_infectious_agent_detection_rate_per_agent_table(x, use_cache),
      abr_infection_rate_table =
        get_abr_infection_rate_table(x, use_cache),
      infectious_agent_detection_rate_per_inf_type_table =
        get_infectious_agent_detection_rate_per_inf_type_table(x, use_cache),
      resistance_test_rate_table =
        get_resistance_test_rate_table(x, use_cache)
    ),
    class = c("neoipcr_ref_ds", "list"))
}

#' Calculate a NeoIPC department report data set
#'
#' @param x The neoipcr_ds object containing the data
#' @param use_cache Use the cache
#'
#' @returns A NeoIPC reference data set
#' @export
calculate_department_data <- function(x, use_cache = TRUE)
{
  check_neoipcr_ds(x)

  if(nrow(x$metadata$departments) < 1)
    rlang::abort("Cannot calculate department data without departments.")

  rt <- x |>
    get_risk_time(use_cache = use_cache)
  rp <- x |>
    get_risk_population(use_cache = use_cache)
  sr <- x |>
    get_surgery_risk(use_cache = use_cache)

  usage_density_rate_table <- rt |>
    dplyr::select(!"patient_days") |>
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = c("factor","name"),
      names_pattern = "^(.+)_([^_]+)$") |>
    tidyr::pivot_wider() |>
    dplyr::mutate(
      factor = factor(.data$factor,
        levels = c("cvc","pvc","vs","inv","niv","human_milk","probiotic",
                   "kangaroo_care","ab","a","w","r"))
    ) |>
    dplyr::arrange(.data$factor)

  structure(
    list(
      birth_weight_figure = x|> get_birthweight_figure_data(),
      gestational_age_figure = x|> get_gestational_age_figure_data(),
      n_patients = rp$n_patients,
      n_enrollments = rp$n_enrollments,
      n_patient_days = rt$patient_days,
      n_surgical_patients = sr$n_patients,
      n_surgical_procedures = sr$n_procedures,
      usage_density_rate_table = usage_density_rate_table,
      surgery_rate_table =
       get_surgery_rate_table(x, use_cache)
      # incidence_density_rate_table =
      #   get_incidence_density_rate_table(x, use_cache),
      # dev_ass_incidence_density_rate_table =
      #   get_dev_ass_incidence_density_rate_table(x, use_cache),
      # infectious_agent_detection_rate_per_agent_table =
      #   get_infectious_agent_detection_rate_per_agent_table(x, use_cache),
      # abr_infection_rate_table =
      #   get_abr_infection_rate_table(x, use_cache),
      # infectious_agent_detection_rate_per_inf_type_table =
      #   get_infectious_agent_detection_rate_per_inf_type_table(x, use_cache),
      # resistance_test_rate_table =
      #   get_resistance_test_rate_table(x, use_cache)
    ),
    class = c("neoipcr_dept_ds", "list"))
}

#' Creates a NeoIPC benchmark data set from department report datasets and a
#'  reference data set
#'
#' @param ... A set of name-value pairs. Each of them should be either a
#'  neoipcr_dept_ds or a neoipcr_ref_ds (typically it's exactly one
#'  neoipcr_dept_ds and one neoipcr_ref_ds to benchmark department data against
#'  reference data but it can also be multiple neoipcr_dept_ds or multiple
#'  neoipcr_ref_ds to benchmark them agianst each other). The names are used as
#'  prefixes in the resulting tables
#'
#' @returns A neoipcr_bnch_ds
#' @export
get_benchmark_data <- function(...)
{
  x <- list(...)
  n_ds <- length(x)
  ds_names = rlang::names2(x)
  output <- list(dataset_names = ds_names)
    suffixes = ds_names |>
    sapply(\(x)ifelse(x=="",x,paste0("_",x)), USE.NAMES = FALSE)

  for (i in 1:n_ds) {
    ds <- x[[i]]
    suffix <- suffixes[i]
    ds_name <- ds_names[i]
    elements <- names(ds)

    if ("n_patients" %in% elements) {
      tbl <- ds$n_patients
      if (!is.data.frame(tbl)) {
        tbl <- tibble::tibble(n = tbl)
      }
      tbl <- tbl |>
        dplyr::rename_with(~ paste0(.x, suffix))

      output$n_patients <- dplyr::bind_cols(output$n_patients, tbl)
    }
    if ("n_enrollments" %in% elements) {
      tbl <- ds$n_enrollments
      if (!is.data.frame(tbl)) {
        tbl <- tibble::tibble(n = tbl)
      }
      tbl <- tbl |>
        dplyr::rename_with(~ paste0(.x, suffix))

      output$n_enrollments <- dplyr::bind_cols(output$n_enrollments, tbl)
    }
    if ("n_patient_days" %in% elements) {
      tbl <- ds$n_patient_days
      if (!is.data.frame(tbl)) {
        tbl <- tibble::tibble(n = tbl)
      }
      tbl <- tbl |>
        dplyr::rename_with(~ paste0(.x, suffix))

      output$n_patient_days <- dplyr::bind_cols(output$n_patient_days, tbl)
    }
    if ("n_surgical_patients" %in% elements) {
      tbl <- ds$n_surgical_patients
      if (!is.data.frame(tbl)) {
        tbl <- tibble::tibble(n = tbl)
      }
      tbl <- tbl |>
        dplyr::rename_with(~ paste0(.x, suffix))

      output$n_surgical_patients <- dplyr::bind_cols(output$n_surgical_patients, tbl)
    }
    if ("n_surgical_procedures" %in% elements) {
      tbl <- ds$n_surgical_procedures
      if (!is.data.frame(tbl)) {
        tbl <- tibble::tibble(n = tbl)
      }
      tbl <- tbl |>
        dplyr::rename_with(~ paste0(.x, suffix))

      output$n_surgical_procedures <- dplyr::bind_cols(output$n_surgical_procedures, tbl)
    }
    if ("birth_weight_figure" %in% elements) {
      n_tbl <- length(ds$birth_weight_figure)
      tbl_names <- names(ds$birth_weight_figure)
      for (j in 1:n_tbl) {
        tbl_name <- tbl_names[j]
        tbl <- tibble::tibble(dataset = ds_name) |>
          dplyr::bind_cols(ds$birth_weight_figure[[j]])

        if (is.null(output$birth_weight_figure)) {
          output$birth_weight_figure <- list()
        }
        if (is.null(output$birth_weight_figure[[tbl_name]])) {
          output$birth_weight_figure[[tbl_name]] <- tbl
        } else {
          output$birth_weight_figure[[tbl_name]] <- output$birth_weight_figure[[tbl_name]] |>
            dplyr::bind_rows(tbl)
        }
      }
    }
    if ("gestational_age_figure" %in% elements) {
      n_tbl <- length(ds$gestational_age_figure)
      tbl_names <- names(ds$gestational_age_figure)
      for (j in 1:n_tbl) {
        tbl_name <- tbl_names[j]
        tbl <- tibble::tibble(dataset = ds_name) |>
          dplyr::bind_cols(ds$gestational_age_figure[[j]])

        if (is.null(output$gestational_age_figure)) {
          output$gestational_age_figure <- list()
        }
        if (is.null(output$gestational_age_figure[[tbl_name]])) {
          output$gestational_age_figure[[tbl_name]] <- tbl
        } else {
          output$gestational_age_figure[[tbl_name]] <- output$gestational_age_figure[[tbl_name]] |>
            dplyr::bind_rows(tbl)
        }
      }
    }
    if ("usage_density_rate_table" %in% elements) {
      tbl <- ds$usage_density_rate_table
      tbl <- tbl |>
        dplyr::rename_with(~ paste0(.x, suffix), !"factor")

      if (is.null(output$usage_density_rate_table)) {
        output$usage_density_rate_table <- tbl
      } else {
        output$usage_density_rate_table <- output$usage_density_rate_table |>
          dplyr::full_join(tbl, dplyr::join_by("factor"))
      }
    }
    if ("surgery_rate_table" %in% elements) {
      tbl <- ds$surgery_rate_table
      tbl <- tbl |>
        dplyr::rename_with(~ paste0(.x, suffix), !"pro_cat")

      if (is.null(output$surgery_rate_table)) {
        output$surgery_rate_table <- tbl
      } else {
        output$surgery_rate_table <- output$surgery_rate_table |>
          dplyr::full_join(tbl, dplyr::join_by("pro_cat")) |>
          dplyr::mutate(
            dplyr::across(
              !tidyselect::matches("^q1|2|3", ignore.case = F),
              ~ tidyr::replace_na(.x, 0)))
      }
    }
  }
  structure(output, class = c("neoipcr_bnch_ds", "list"))
}

get_birthweight_figure_data <- function(x)
{
  bw_quartiles <- x$patients$birth_weight |>
    stats::quantile(names = FALSE) |>
    as.integer()

  bw_mean = x$patients$birth_weight |>
    mean() |>
    as.integer()

  bw_scale_min <- as.integer(bw_quartiles[1] / 50L) * 50L - 50L
  bw_scale_max <- as.integer(bw_quartiles[5] / 50L) * 50L + 100L

  bw_density <- x$patients$birth_weight |>
    bw50(as_factor = F) |>
    stats::density(from = bw_scale_min, to = bw_scale_max)

  list(
    density = tibble::tibble(
      birth_weight = bw_density$x,
      density = bw_density$y / sum(bw_density$y)
    ),
    frequency = tibble::tibble(
      birth_weight_cat = x$patients$birth_weight |>
        bw50(as_factor = F)
    ) |>
      dplyr::group_by(.data$birth_weight_cat) |>
      dplyr::summarise(n = dplyr::n()),
    location_parameters = tibble::tibble(
      q1 = bw_quartiles[2],
      q2 = bw_quartiles[3],
      q3 = bw_quartiles[4],
      mean = bw_mean),
    scale = tibble::tibble(
      min = bw_scale_min,
      max = bw_scale_max
    )
  )
}

get_gestational_age_figure_data <- function(x)
{
  ga_quartiles <- x$patients$total_gestation_days |>
    stats::quantile(names = FALSE) |>
    as.integer()

  ga_mean = x$patients$total_gestation_days |>
    mean() |>
    as.integer()

  ga_scale_min = as.integer(ga_quartiles[1] / 7L) * 7L - 7L
  ga_scale_max = as.integer(ga_quartiles[5] / 7L) * 7L + 14L

  ga_density <- x$patients$total_gestation_days |>
    ga7() |>
    stats::density(from = ga_scale_min, to = ga_scale_max)

  list(
    density = tibble::tibble(
      gestational_age = ga_density$x,
      density = ga_density$y / sum(ga_density$y)
    ),
    frequency = tibble::tibble(
      gestational_age_cat = x$patients$total_gestation_days |>
        ga7()
    ) |>
      dplyr::group_by(.data$gestational_age_cat) |>
      dplyr::summarise(n = dplyr::n()),
    location_parameters = tibble::tibble(
      q1 = ga_quartiles[2],
      q2 = ga_quartiles[3],
      q3 = ga_quartiles[4],
      mean = ga_mean),
    scale = tibble::tibble(
      min = ga_scale_min,
      max = ga_scale_max
    )
  )
}

#' Get the table with usage density rates of the time dependent risk factors
#'
#' @param x The reference data set which can be either a neoipcr_ref_ds or a
#'  neoipcr_ds object
#' @param use_cache Use the cache. Ignored if ref is a neoipcr_ref_ds object
#'
#' @returns A table containing usage density rates of the time dependent risk
#'  factors
#' @export
get_usage_density_rate_table <- function(x, use_cache = TRUE)
{
  check_neoipcr_ds_or_ref_ds(x)

  if(is_neoipcr_ref_ds(x))
    return(x$usage_density_rate_table)

  if(use_cache && !is.null(r <- get_cached(x, "usage_density_rate_table")))
    return(r)

  pat_days <- x |>
    get_risk_time(group_cols = "department_key", use_cache = use_cache) |>
    dplyr::pull("patient_days")

  n_deps <- length(pat_days)
  median_patient_days <- stats::median(pat_days)

  expected_levels <- c("cvc","pvc","vs","inv","niv","human_milk","probiotic",
                       "kangaroo_care","ab","a","w","r")

  risk_time <- get_risk_time(x, use_cache = use_cache)
  risk_rate_quartiles <- get_risk_time(
    x, group_cols = "department_key", use_cache = use_cache) |>
    dplyr::select(tidyselect::ends_with("_rate")) |>
    dplyr::reframe(
      dplyr::across(
        tidyselect::everything(),
        ~stats::quantile(.x, prob = c(.25,.5,.75))))

  r <- risk_time |>
    dplyr::select(!"patient_days" & tidyselect::ends_with("_days")) |>
    tidyr::pivot_longer(
      cols = tidyselect::ends_with("_days"),
      values_to = "n") |>
    dplyr::mutate(
      factor = .data$name,
      .before = 1,
      .keep = "unused") |>
    dplyr::bind_cols(
      risk_time |>
        dplyr::select(tidyselect::ends_with("_rate")) |>
        tidyr::pivot_longer(
          cols = tidyselect::ends_with("_rate"),
          values_to = "pooled")) |>
    dplyr::bind_cols(
      risk_rate_quartiles |>
        dplyr::bind_cols(tibble::tibble(name = c("q1","q2","q3"))) |>
        tidyr::pivot_wider(
          values_from = tidyselect::ends_with("_rate")) |>
        tidyr::pivot_longer(
          tidyselect::everything(),
          names_pattern = "^(.+)_(q(?:1|2|3))$",
          names_to = c("rate",".value"))) |>
    dplyr::select(!tidyselect::all_of(c("name","rate")))

  missing <- setdiff(paste0(expected_levels, "_days"), r$factor)
  if(length(missing) > 0)
    r <- r |>
    dplyr::bind_rows(tibble::tibble(factor=missing,n=0L,pooled=0))

  r |>
    dplyr::mutate(
      factor = factor(
        stringr::str_extract(.data$factor,"^(.+)_days", 1),
        levels = expected_levels),
      drop_quartiles = n_deps < 5 | round(100 / .data$pooled) >= median_patient_days,
      q1 = dplyr::if_else(
        .data$drop_quartiles,
        NA,
        .data$q1),
      q2 = dplyr::if_else(
        .data$drop_quartiles,
        NA,
        .data$q2),
      q3 = dplyr::if_else(
        .data$drop_quartiles,
        NA,
        .data$q3)) |>
    dplyr::arrange(.data$factor) |>
    dplyr::select(!"drop_quartiles") |>
    add_class("neoipcr_tbl_udr_ref") |>
    cache(x, "usage_density_rate_table")
}

#' Get the table with rates of surgical procedues
#'
#' @param x The data set which can be either a neoipcr_unit_ds or a
#'  neoipcr_ds object
#' @param use_cache Use the cache. Ignored if ref is a neoipcr_unit_ds object
#'
#' @returns A table containing the rates of surgical procedues
#' @export
get_surgery_rate_table <- function(x, use_cache = TRUE)
{
  # ToDo: Class for unit dataset
  check_neoipcr_ds(x)

  # if(is_neoipcr_ref_ds(x))
  #   return(x$surgery_rate_table)

  if(use_cache && !is.null(r <- get_cached(x, "surgery_rate_table")))
    return(r)

  tibble::tibble(
    pro_cat = "overall",
    n = get_procedures(x, use_cache = use_cache) |>
      dplyr::pull()) |>
    dplyr::bind_rows(
      get_procedures(
        x,
        group_cols = "pro_cat",
        use_cache = use_cache)
    ) |>
    dplyr::bind_cols(
      get_risk_population(x, use_cache = use_cache) |>
        dplyr::select("n_patients")
    ) |>
    dplyr::mutate(pooled = .data$n / .data$n_patients * 100) |>
    dplyr::select(!"n_patients") |>
    add_class("neoipcr_tbl_sr") |>
    cache(x, "surgery_rate_table")
}


#' Get the reference table with rates of surgical procedues
#'
#' @param ref The reference data set which can be either a neoipcr_ref_ds or a
#'  neoipcr_ds object
#' @param use_cache Use the cache. Ignored if ref is a neoipcr_ref_ds object
#'
#' @returns A table containing the reference rates of surgical procedues and the
#'  25%, 50%, and 75% quantiles
#' @export
get_ref_surgery_rate_table <- function(ref, use_cache = TRUE)
{
  check_neoipcr_ds_or_ref_ds(ref)

  if(is_neoipcr_ref_ds(ref))
    return(ref$surgery_rate_table)

  if(use_cache && !is.null(r <- get_cached(ref, "ref_surgery_rate_table")))
    return(r)

  pats_per_dept <- ref |>
    get_risk_population(
      group_cols = "department_key",
      use_cache = use_cache) |>
    dplyr::select("department_key", "n_patients")

  n_deps <- nrow(pats_per_dept)
  median_patients <- stats::median(dplyr::pull(pats_per_dept, "n_patients"))

  r <- ref |>
    get_surgery_rate_table(use_cache = use_cache)

  if(nrow(r) == 1 && r$n == 0)
    return(
      dplyr::bind_cols(r, q1 = NA, q2 = NA, q3 = NA) |>
        add_class("neoipcr_tbl_sr_ref") |>
        cache(ref, "ref_surgery_rate_table"))

  r |>
    dplyr::inner_join(
      get_procedures(
        ref,
        group_cols = c("department_key", "pro_cat"),
        use_cache = use_cache) |>
        dplyr::bind_rows(
          get_procedures(
            ref,
            group_cols = "department_key",
            use_cache = use_cache)) |>
        dplyr::right_join(
          pats_per_dept,
          dplyr::join_by("department_key")) |>
        dplyr::mutate(
          n = tidyr::replace_na(.data$n, 0),
          pro_cat = tidyr::replace_na(
            as.character(.data$pro_cat), "overall"),
          pooled = .data$n / .data$n_patients * 100) |>
        dplyr::select(!c("n","n_patients")) |>
        tidyr::pivot_wider(
          names_from = "pro_cat",
          values_from = "pooled",
          values_fill = 0) |>
        dplyr::select(!"department_key") |>
        dplyr::reframe(
          dplyr::across(
            tidyselect::everything(),
            ~stats::quantile(.x, prob = c(.25,.5,.75), na.rm = TRUE))) |>
        dplyr::bind_cols(tibble::tibble(name = c("q1","q2","q3"))) |>
        tidyr::pivot_wider(values_from = !"name") |>
        tidyr::pivot_longer(
          tidyselect::everything(),
          names_pattern = "^(.+)_(q(?:1|2|3))$",
          names_to = c("pro_cat",".value")),
      dplyr::join_by("pro_cat")) |>
    dplyr::mutate(
      drop_quartiles = n_deps < 5 | round(100 / .data$pooled) >= median_patients,
      q1 = dplyr::if_else(
        .data$drop_quartiles,
        NA,
        .data$q1),
      q2 = dplyr::if_else(
        .data$drop_quartiles,
        NA,
        .data$q2),
      q3 = dplyr::if_else(
        .data$drop_quartiles,
        NA,
        .data$q3)) |>
    dplyr::select(!"drop_quartiles") |>
    add_class("neoipcr_tbl_sr_ref") |>
    cache(ref, "ref_surgery_rate_table")
}

#' Get the table with incidence density rates of the infections with time
#'  dependent risks
#'
#' @param ref The reference data set which can be either a neoipcr_ref_ds or a
#'  neoipcr_ds object
#' @param use_cache Use the cache. Ignored if ref is a neoipcr_ref_ds object
#'
#' @returns A table containing incidence density rates of the infections with
#'  time dependent risks
#' @export
get_incidence_density_rate_table <- function(ref, use_cache = TRUE)
{
  check_neoipcr_ds_or_ref_ds(ref)

  if(is_neoipcr_ref_ds(ref))
    return(ref$incidence_density_rate_table)

  if(use_cache && !is.null(r <- get_cached(ref, "incidence_density_rate_table")))
    return(r)

  pat_days <- ref |>
    get_risk_time(group_cols = "department_key", use_cache = use_cache) |>
    dplyr::pull("patient_days")

  n_deps <- length(pat_days)
  median_patient_days <- stats::median(pat_days)

  expected_levels <- c("si","bsi","hap","nec")

  r <- ref |>
    get_incidence_density_rates(use_cache = use_cache) |>
    dplyr::inner_join(
      ref |>
        get_incidence_density_rates(
          group_cols = "department_key",
          use_cache = use_cache) |>
        dplyr::select(!"n") |>
        tidyr::pivot_wider(names_from = "inf", values_from = "rate") |>
        dplyr::select(!"department_key") |>
        dplyr::reframe(
          dplyr::across(
            tidyselect::everything(), ~stats::quantile(.x, prob = c(.25,.5,.75), na.rm = TRUE))) |>
        dplyr::bind_cols(tibble::tibble(name = c("q1","q2","q3"))) |>
        tidyr::pivot_wider(values_from = !"name") |>
        tidyr::pivot_longer(
          tidyselect::everything(),
          names_pattern = "^(.+)_(q(?:1|2|3))$",
          names_to = c("inf",".value")),
      dplyr::join_by("inf"))

  missing <- setdiff(expected_levels, r$inf)
  if(length(missing) > 0)
    r <- r |>
    dplyr::bind_rows(tibble::tibble(inf=missing,n=0L,rate=0))

  r |>
    dplyr::mutate(
      inf = factor(.data$inf, levels = c("si","bsi","hap","nec")),
      drop_quartiles = n_deps < 5 | round(1000 / .data$rate) >= median_patient_days,
      q1 = dplyr::if_else(
        .data$drop_quartiles,
        NA,
        .data$q1),
      q2 = dplyr::if_else(
        .data$drop_quartiles,
        NA,
        .data$q2),
      q3 = dplyr::if_else(
        .data$drop_quartiles,
        NA,
        .data$q3)) |>
    dplyr::select(!"drop_quartiles") |>
    dplyr::rename("pooled"="rate") |>
    dplyr::arrange(.data$inf) |>
    add_class("neoipcr_tbl_idr_ref") |>
    cache(ref, "incidence_density_rate_table")
}

#' Get the table with device associated incidence density rates of the
#'  infections with device associated risks
#'
#' @param ref The reference data set which can be either a neoipcr_ref_ds or a
#'  neoipcr_ds object
#' @param use_cache Use the cache. Ignored if ref is a neoipcr_ref_ds object
#'
#' @returns A table containing device associated incidence density rates of the
#'  infections with device associated risks
#' @export
get_dev_ass_incidence_density_rate_table <- function(ref, use_cache = TRUE)
{
  check_neoipcr_ds_or_ref_ds(ref)

  if(is_neoipcr_ref_ds(ref))
    return(ref$dev_ass_incidence_density_rate_table)

  if(use_cache && !is.null(r <- get_cached(ref, "dev_ass_incidence_density_rate_table")))
    return(r)

  dev_days <- ref |>
    get_risk_time(group_cols = "department_key", use_cache = use_cache) |>
    dplyr::select(
      tidyselect::any_of(
        c("department_key","cvc"="cvc_days","pvc"="pvc_days","vs"="vs_days",
          "inv"="inv_days","niv"="niv_days")))

  dep_stats <- dev_days |>
    tidyr::pivot_longer(cols = !"department_key", names_to = "dev") |>
    dplyr::filter(.data$value > 0) |>
    dplyr::group_by(.data$dev) |>
    dplyr::summarise(n_deps = dplyr::n()) |>
    dplyr::inner_join(
      dev_days |>
        dplyr::select(!"department_key") |>
        dplyr::summarise(dplyr::across(tidyselect::everything(), stats::median)) |>
        tidyr::pivot_longer(
          cols = tidyselect::everything(),
          names_to = "dev",
          values_to = "median"),
      dplyr::join_by("dev")
    )

  ref |>
    get_dev_ass_incidence_density_rates(use_cache = use_cache) |>
    dplyr::inner_join(
      ref |>
        get_dev_ass_incidence_density_rates(
          group_cols = "department_key",
          use_cache = use_cache) |>
        dplyr::select(!"n") |>
        tidyr::pivot_wider(names_from = "dev", values_from = "rate") |>
        dplyr::select(!"department_key") |>
        dplyr::reframe(
          dplyr::across(
            tidyselect::everything(),
            ~stats::quantile(.x, prob = c(.25,.5,.75), na.rm = TRUE))) |>
        dplyr::bind_cols(tibble::tibble(name = c("q1","q2","q3"))) |>
        tidyr::pivot_wider(values_from = !"name") |>
        tidyr::pivot_longer(
          tidyselect::everything(),
          names_pattern = "^(.+)_(q(?:1|2|3))$",
          names_to = c("dev",".value")),
      dplyr::join_by("dev")) |>
    dplyr::inner_join(dep_stats, dplyr::join_by("dev")) |>
    dplyr::mutate(
      dev = factor(.data$dev, levels = c("cvc","pvc","vs","inv","niv")),
      drop_quartiles = .data$n_deps < 5 | round(1000 / .data$rate) >= .data$median,
      q1 = dplyr::if_else(
        .data$drop_quartiles,
        NA,
        .data$q1),
      q2 = dplyr::if_else(
        .data$drop_quartiles,
        NA,
        .data$q2),
      q3 = dplyr::if_else(
        .data$drop_quartiles,
        NA,
        .data$q3)) |>
    dplyr::rename("pooled"="rate") |>
    dplyr::select(!c("drop_quartiles","n_deps","median")) |>
    dplyr::arrange(.data$dev) |>
    add_class("neoipcr_tbl_daidr_ref") |>
    cache(ref, "dev_ass_incidence_density_rate_table")

}

#' Get the table with infectious agent detection rates per type of infection
#'
#' @param ref The reference data set which can be either a neoipcr_ref_ds or a
#'  neoipcr_ds object
#' @param use_cache Use the cache. Ignored if ref is a neoipcr_ref_ds object
#'
#' @returns A table containing infectious agent detection rates per type of
#'  infection
#' @export
get_infectious_agent_detection_rate_per_inf_type_table <- function(ref, use_cache = TRUE)
{
  check_neoipcr_ds_or_ref_ds(ref)

  if(is_neoipcr_ref_ds(ref))
    return(ref$infectious_agent_detection_rate_per_inf_type_table)

  if(use_cache && !is.null(r <- get_cached(ref, "infectious_agent_detection_rate_per_inf_type_table")))
    return(r)


  dep_stats <- dplyr::bind_rows(
    dplyr::bind_cols(
      event_type_key = "all",
      ref |>
        get_infection_counts(group_cols = "department_key") |>
        dplyr::filter(.data$n > 0)
      ),
    ref |>
      get_infection_counts(group_cols = c("event_type_key","department_key")) |>
      dplyr::filter(.data$n > 0)
    ) |>
    dplyr::group_by(.data$event_type_key) |>
    dplyr::summarise(
      median = stats::median(.data$n),
      n = dplyr::n())

  dplyr::bind_rows(
    dplyr::bind_cols(
      event_type_key = "all",
      ref |>
        get_infectious_agent_detection_rates(
          use_cache = use_cache) |>
        dplyr::select("inf_with_pathogen","pooled"="iwp_per_t"),
      ref |>
        get_infectious_agent_detection_rates(
          group_cols = "department_key",
          use_cache = use_cache) |>
        dplyr::reframe(
          value = stats::quantile(
            .data$iwp_per_t,
            prob = c(.25,.5,.75),
            na.rm = TRUE)) |>
        dplyr::mutate(
          name=names(.data$value),
          name=dplyr::case_match(
            .data$name,
            "25%"~"q1",
            "50%"~"q2",
            "75%"~"q3")) |>
        tidyr::pivot_wider()),
    ref |>
      get_infectious_agent_detection_rates(
        group_cols = "event_type_key",
        use_cache = use_cache) |>
      dplyr::select("event_type_key","inf_with_pathogen","pooled"="iwp_per_t") |>
      dplyr::inner_join(
        ref |>
          get_infectious_agent_detection_rates(
            group_cols = c("department_key","event_type_key"),
            use_cache = use_cache) |>
          dplyr::group_by(.data$event_type_key) |>
          dplyr::reframe(
            value = stats::quantile(
              .data$iwp_per_t,
              prob = c(.25,.5,.75),
              na.rm = TRUE)) |>
          dplyr::mutate(
            name=names(.data$value),
            name=dplyr::case_match(
              .data$name,
              "25%"~"q1",
              "50%"~"q2",
              "75%"~"q3")) |>
          tidyr::pivot_wider(),
        dplyr::join_by("event_type_key"))
    ) |>
    dplyr::inner_join(
      dep_stats,
      dplyr::join_by("event_type_key")) |>
    dplyr::mutate(
      drop_quartiles = .data$n < 5 | round(100 / .data$pooled) >= .data$median,
      q1 = dplyr::if_else(
        .data$drop_quartiles,
        NA,
        .data$q1),
      q2 = dplyr::if_else(
        .data$drop_quartiles,
        NA,
        .data$q2),
      q3 = dplyr::if_else(
        .data$drop_quartiles,
        NA,
        .data$q3)
      ) |>
    dplyr::select(!c("drop_quartiles","n","median")) |>
    dplyr::rename("inf"="event_type_key","n"="inf_with_pathogen") |>
    add_class("neoipcr_tbl_iadrpit_ref") |>
    cache(ref, "infectious_agent_detection_rate_per_inf_type_table")
}

#' Get the table with infectious agent detection rates of the pathogens in a
#'  somewhat meaningful taxonomic structure
#'
#' @param ref The reference data set which can be either a neoipcr_ref_ds or a
#'  neoipcr_ds object
#' @param use_cache Use the cache. Ignored if ref is a neoipcr_ref_ds object
#'
#' @returns A table containing infectious agent detection rates
#' @export
get_infectious_agent_detection_rate_per_agent_table <- function(ref, use_cache = TRUE)
{
  check_neoipcr_ds_or_ref_ds(ref)

  if(is_neoipcr_ref_ds(ref))
    return(ref$infectious_agent_detection_rate_per_agent_table)

  if(use_cache && !is.null(r <- get_cached(ref, "infectious_agent_detection_rate_per_agent_table")))
    return(r)

  lv0 <- dplyr::bind_cols(
    lv = 0L,
    tl = "none",
    group = "Total",
    ref |>
      get_infectious_agent_detection_rates_with_department_quartiles(
        use_cache = use_cache) |>
      dplyr::select("n","rate","q1","q2","q3"))
  d <- ref |>
    get_infectious_agent_detection_rates_with_department_quartiles(
      group_cols = "domain",
      use_cache = use_cache) |>
    dplyr::select("group"="domain","n","rate","q1","q2","q3") |>
    dplyr::arrange(dplyr::desc(.data$rate))
  for (i in 1:nrow(d)) {
    di <- d[i,]
    if(!is.na(di$group) && di$group == "Bacteria") {
      lv1 <- dplyr::bind_cols(lv = 1L, tl = "domain", di)
      o <- ref |>
        get_infectious_agent_detection_rates_with_department_quartiles(
          group_cols = c("domain","order"),
          use_cache = use_cache) |>
        dplyr::filter(.data$domain == di$group) |>
        dplyr::select("group"="order","n","rate","q1","q2","q3") |>
        dplyr::arrange(dplyr::desc(.data$rate))
      for (j in 1:nrow(o)) {
        oj <- o[j,]
        lv2 <- dplyr::bind_cols(lv = 2L, tl = "order", oj)
        g <- ref |>
          get_infectious_agent_detection_rates_with_department_quartiles(
            group_cols = c("order","genus"),
            use_cache = use_cache) |>
          dplyr::filter(.data$order == oj$group) |>
          dplyr::select("group"="genus","n","rate","q1","q2","q3") |>
          dplyr::arrange(dplyr::desc(.data$rate))
        for (k in 1:nrow(g)) {
          gk <- g[k,]
          lv3 <- dplyr::bind_cols(lv = 3L, tl = "genus", gk |> dplyr::mutate(group = paste(.data$group, "spp.")))
          if(gk$group == "Staphylococcus") {
            c <- ref |>
              get_infectious_agent_detection_rates_with_department_quartiles(
                group_cols = c("genus","coagulase"),
                use_cache = use_cache) |>
              dplyr::filter(.data$genus == "Staphylococcus") |>
              dplyr::select("group"="coagulase","n","rate","q1","q2","q3") |>
              dplyr::arrange(dplyr::desc(.data$rate))
            for (l in 1:nrow(c)) {
              cl <- c[l,]
              c_text <- switch (as.character(cl$group),
                "n" = "Coagulase-negative staphylococci",
                "p" = "Coagulase-positive staphylococci",
                "Staphylococcus spp. n.o.s.")
              lv4 <- dplyr::bind_cols(
                lv = 4L,
                tl = switch (as.character(cl$group), "n" = "coag_type",
                             "p" = "coag_type", "coag_type_nos"),
                cl |> dplyr::mutate(group = c_text))
              s <- ref |>
                get_infectious_agent_detection_rates_with_department_quartiles(
                  group_cols = c("genus","coagulase","species"),
                  use_cache = use_cache) |>
                dplyr::filter(.data$genus == "Staphylococcus" & .data$coagulase == cl$group) |>
                dplyr::select("group"="species","n","rate","q1","q2","q3") |>
                dplyr::arrange(dplyr::desc(.data$rate))
              lv5 <- dplyr::bind_rows(
                dplyr::bind_cols(
                  lv = 5L,
                  tl = "species",
                  s |> dplyr::filter(!is.na(.data$group))),
                dplyr::bind_cols(
                  lv = 5L,
                  tl = "species_nos",
                  s |> dplyr::filter(is.na(.data$group)) |>
                    dplyr::mutate(group = paste(c_text, "n.o.s."))))
              lv3 <- dplyr::bind_rows(lv3,lv4,lv5)
            }
          }
          else {
            s <- ref |>
              get_infectious_agent_detection_rates_with_department_quartiles(
                group_cols = c("genus","coagulase","species"),
                use_cache = use_cache) |>
              dplyr::filter(.data$genus == gk$group) |>
              dplyr::select("group"="species","n","rate","q1","q2","q3") |>
              dplyr::arrange(dplyr::desc(.data$rate))
            lv4 <- dplyr::bind_rows(
              dplyr::bind_cols(
                lv = 4L,
                tl = "species",
                s |> dplyr::filter(!is.na(.data$group))),
              dplyr::bind_cols(
                lv = 4L,
                tl = "species_nos",
                s |> dplyr::filter(is.na(.data$group)) |>
                  dplyr::mutate(group = paste(gk$group,"spp. n.o.s."))))
            lv3 <- dplyr::bind_rows(lv3,lv4)
          }
          lv2 <- dplyr::bind_rows(lv2,lv3)
        }
        lv1 <- dplyr::bind_rows(lv1,lv2)
      }
      lv0 <- dplyr::bind_rows(lv0,lv1)
    }
    else if (!is.na(di$group)) {
      kd <- ref |>
        get_infectious_agent_detection_rates_with_department_quartiles(
          group_cols = c("domain","kingdom"),
          use_cache = use_cache) |>
        dplyr::filter(.data$domain == di$group) |>
        dplyr::select("group"="kingdom","n","rate","q1","q2","q3") |>
        dplyr::arrange(dplyr::desc(.data$rate))
      for (j in 1:nrow(kd)) {
        kj <- kd[j,]
        lv1 <- dplyr::bind_cols(lv = 1L, tl = "kingdom", kj)
        g <- ref |>
          get_infectious_agent_detection_rates_with_department_quartiles(
            group_cols = c("kingdom","genus"),
            use_cache = use_cache) |>
          dplyr::filter(.data$kingdom == kj$group) |>
          dplyr::select("group"="genus","n","rate","q1","q2","q3") |>
          dplyr::arrange(dplyr::desc(.data$rate))
        for (k in 1:nrow(g)) {
          gk <- g[k,]
          lv2 <- dplyr::bind_cols(lv = 2L, tl = "genus", gk |> dplyr::mutate(group = paste(.data$group, "spp.")))
          s <- ref |>
            get_infectious_agent_detection_rates_with_department_quartiles(
              group_cols = c("genus","coagulase","species"),
              use_cache = use_cache) |>
            dplyr::filter(.data$genus == gk$group) |>
            dplyr::select("group"="species","n","rate","q1","q2","q3") |>
            dplyr::arrange(dplyr::desc(.data$rate))
          lv3 <- dplyr::bind_rows(
            dplyr::bind_cols(
              lv = 3L,
              tl = "species",
              s |> dplyr::filter(!is.na(.data$group))),
            dplyr::bind_cols(
              lv = 3L,
              tl = "species_nos",
              s |> dplyr::filter(is.na(.data$group)) |>
                dplyr::mutate(group = paste(gk$group,"spp. n.o.s."))))
          lv1 <- dplyr::bind_rows(lv1,lv2,lv3)
        }
      }
      lv0 <- dplyr::bind_rows(lv0,lv1)
    }
  }

  lv0 |>
    dplyr::rename("level"="lv","taxon"="tl","pooled"="rate") |>
    add_class("neoipcr_tbl_iadrpa_ref") |>
    cache(ref, "infectious_agent_detection_rate_per_agent_table")
}

#' Get the table of infection rates with antibiotic resistant bacteria in a
#'  somewhat meaningful taxonomic structure
#'
#' @param ref The reference data set which can be either a neoipcr_ref_ds or a
#'  neoipcr_ds object
#' @param use_cache Use the cache. Ignored if ref is a neoipcr_ref_ds object
#'
#' @returns A table containing infection rates with antibiotic resistant
#'  bacteria
#' @export
get_abr_infection_rate_table <- function(ref, use_cache = TRUE)
{
  check_neoipcr_ds_or_ref_ds(ref)

  if(is_neoipcr_ref_ds(ref))
    return(ref$abr_infection_rate_table)

  if(use_cache && !is.null(r <- get_cached(ref, "abr_infection_rate_table")))
    return(r)

  abr_types <- c("3gcr","car","cor")
  tbl <- NULL

  for (abr_type in abr_types) {
    t <- ref |>
      get_resistance_rate_with_department_quartiles(
        resistance = abr_type,
        use_cache = use_cache) |>
      dplyr::select("n","rate","q1","q2","q3")

    lv0 <- dplyr::bind_cols(
      abr_type = abr_type,
      lv = 0L,
      tl = "none",
      group = "Total",
      t)

    if(t$n > 0) {
      o <- ref |>
        get_resistance_rate_with_department_quartiles(
          resistance = abr_type,
          group_cols = "order",
          use_cache = use_cache) |>
        dplyr::select("group"="order","n","rate","q1","q2","q3") |>
        dplyr::arrange(dplyr::desc(.data$rate))

      for (j in 1:nrow(o)) {
        oj <- o[j,]

        if(oj$n > 0) {
          lv1 <- dplyr::bind_cols(
            abr_type = abr_type,
            lv = 2L,
            tl = "order",
            oj)
          g <- ref |>
            get_resistance_rate_with_department_quartiles(
              resistance = abr_type,
              group_cols = c("order","genus"),
              use_cache = use_cache) |>
            dplyr::filter(.data$order == oj$group) |>
            dplyr::select("group"="genus","n","rate","q1","q2","q3") |>
            dplyr::arrange(dplyr::desc(.data$rate))

          for (k in 1:nrow(g)) {
            gk <- g[k,]

            if(gk$n > 0) {
              lv2 <- dplyr::bind_cols(
                abr_type = abr_type,
                lv = 3L,
                tl = "genus",
                gk |>
                  dplyr::mutate(group = paste(.data$group, "spp.")))
              s <- ref |>
                get_resistance_rate_with_department_quartiles(
                  resistance = abr_type,
                  group_cols = c("genus","species"),
                  use_cache = use_cache) |>
                dplyr::filter(.data$genus == gk$group) |>
                dplyr::select("group"="species","n","rate","q1","q2","q3") |>
                dplyr::arrange(dplyr::desc(.data$rate))
              lv3 <- dplyr::bind_rows(
                dplyr::bind_cols(
                  abr_type = abr_type,
                  lv = 4L,
                  tl = "species",
                  s |>
                    dplyr::filter(!is.na(.data$group) & .data$n > 0)),
                dplyr::bind_cols(
                  abr_type = abr_type,
                  lv = 4L,
                  tl = "species_nos",
                  s |>
                    dplyr::filter(is.na(.data$group) & .data$n > 0) |>
                    dplyr::mutate(group = paste(gk$group,"spp. n.o.s."))))
              lv2 <- dplyr::bind_rows(lv2,lv3)
              lv1 <- dplyr::bind_rows(lv1,lv2)
            }
          }
          lv0 <- dplyr::bind_rows(lv0,lv1)
          }
        }
      }
    tbl <- dplyr::bind_rows(tbl,lv0)
  }

  # MRSA only has one species
  tbl <-dplyr::bind_rows(
    tbl,
    dplyr::bind_cols(
      abr_type = "mrsa",
      lv = 0L,
      tl = "species",
      ref |>
        get_resistance_rate_with_department_quartiles(
          resistance = "mrsa",
          group_cols = "species",
          use_cache = use_cache) |>
        dplyr::select("group"="species","n","rate","q1","q2","q3")))

  # VRE only has one genus
  g <- ref |>
    get_resistance_rate_with_department_quartiles(
      resistance = "vre",
      group_cols = "genus",
      use_cache = use_cache) |>
    dplyr::select("group"="genus","n","rate","q1","q2","q3") |>
    dplyr::mutate(group = "Enterococcus spp.")

  lv0 <- dplyr::bind_cols(
    abr_type = "vre",
    lv = 0L,
    tl = "genus",
    g)

  if(g$n > 0) {
    s <- ref |>
      get_resistance_rate_with_department_quartiles(
        resistance = "vre",
        group_cols = c("species"),
        use_cache = use_cache) |>
      dplyr::select("group"="species","n","rate","q1","q2","q3") |>
      dplyr::arrange(dplyr::desc(.data$rate))

    lv1 <- dplyr::bind_rows(
      dplyr::bind_cols(
        abr_type = "vre",
        lv = 1L,
        tl = "species",
        s |>
          dplyr::filter(!is.na(.data$group) & .data$n > 0)),
      dplyr::bind_cols(
        abr_type = "vre",
        lv = 1L,
        tl = "species_nos",
        s |>
          dplyr::filter(is.na(.data$group) & .data$n > 0) |>
          dplyr::mutate(group = paste(gk$group,"spp. n.o.s."))))

    lv0 <- dplyr::bind_rows(lv0,lv1)
  }

  tbl <-dplyr::bind_rows(tbl,lv0)

  tbl |>
    dplyr::rename("abr"="abr_type","level"="lv","taxon"="tl","pooled"="rate") |>
    add_class("neoipcr_tbl_abr_ir_ref") |>
    cache(ref, "abr_infection_rate_table")
}

#' Get the table with resistance test rates of the recorded resistance
#'  mechanisms
#'
#' @param ref The reference data set which can be either a neoipcr_ref_ds or a
#'  neoipcr_ds object
#' @param use_cache Use the cache. Ignored if ref is a neoipcr_ref_ds object
#'
#' @returns A table containing resistance test rates
#' @export
get_resistance_test_rate_table <- function(ref, use_cache = TRUE)
{
  check_neoipcr_ds_or_ref_ds(ref)

  if(is_neoipcr_ref_ds(ref))
    return(ref$resistance_test_rate_table)

  if(use_cache && !is.null(r <- get_cached(ref, "resistance_test_rate_table")))
    return(r)

  c("3gcr","car","cor","mrsa","vre") |>
    lapply(\(r) dplyr::bind_cols(res = r, type = "routine", get_resistance_test_rate_with_department_quartiles(ref, r))) |>
    dplyr::bind_rows() |>
    dplyr::bind_rows(
      ref |>
        get_resistance_test_rate_with_department_quartiles(
          resistance = "car",
          group_cols = "3gcr") |>
        dplyr::filter(.data$`3gcr` == "yes") |>
        dplyr::mutate(type = "if_3gcr", res = "car")) |>
    dplyr::bind_rows(
      ref |>
        get_resistance_test_rate_with_department_quartiles(
          resistance = "cor",
          group_cols = c("3gcr","car")) |>
        dplyr::filter(.data$`3gcr` == "yes" & .data$car == "yes") |>
        dplyr::mutate(type = "if_3gcr&car", res = "cor")) |>
    dplyr::mutate(
      abr = factor(.data$res, levels = c("3gcr","car","cor","mrsa","vre")),
      cond = factor(.data$type, levels = c("routine","if_3gcr","if_3gcr&car")),
      .keep = "unused"
    ) |>
    dplyr::select("abr","cond","n"="tested","pooled"="rate","q1","q2","q3") |>
    dplyr::arrange(.data$abr, .data$cond) |>
    add_class("neoipcr_tbl_rtr_ref") |>
    cache(ref, "resistance_test_rate_table")
}

#' Prettyf the names of a neoipcr object
#'
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#'
#' @returns the same object as x but with pretty and potentially translated
#'  names
#' @export
pretty_names <- function(x, ...){
  UseMethod("pretty_names")
}

#' @export
pretty_names.default <- function(x, ...) x

#' @export
pretty_names.neoipcr_tbl_sr_ref <- function(x, ...)
{
  col_names <- stats::setNames(
    gettext("Procedure category","N","Pooled","Q1","Q2","Q3"),
    c("pro_cat","n","pooled","q1","q2","q3"))

  pairs <- x |>
    dplyr::select("pro_cat") |>
    dplyr::mutate(
      pretty_name = get_procedure_category_pretty(.data$pro_cat))

  row_names <- stats::setNames(pairs$pretty_name, pairs$pro_cat)

  attr(x, "names.pretty") <- col_names
  attr(x, "row.names.pretty") <- row_names

  x |>
    dplyr::inner_join(pairs, dplyr::join_by("pro_cat")) |>
    dplyr::mutate(pro_cat = .data$pretty_name, .keep = "unused") |>
    dplyr::rename_with(
      ~ dplyr::case_match(
        .x,
        "pro_cat"~col_names[["pro_cat"]],
        "n"~col_names[["n"]],
        "pooled"~col_names[["pooled"]],
        "q1"~col_names[["q1"]],
        "q2"~col_names[["q2"]],
        "q3"~col_names[["q3"]],
        .default = .x))
}

get_dev_ass_incidence_density_rates <- function(x, group_cols = NULL, use_cache = TRUE)
{
  if(is.null(group_cols))
    cache_key <- "dev_ass_incidence_density_rates"
  else
    cache_key <- paste0("dev_ass_incidence_density_rates_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  x$events |>
    dplyr::inner_join(
      x$sepsisData |>
        dplyr::select("event_key","dev_ass") |>
        dplyr::filter(.data$dev_ass != 0) |>
        dplyr::mutate(
          dev = dplyr::case_match(
            as.integer(as.character(.data$dev_ass)),
            1 ~ "cvc",
            2 ~ "pvc"),
          .keep = "unused"),
      dplyr::join_by("event_key")) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(
      c(group_cols,"dev")))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::right_join(
      get_risk_time(x, group_cols, use_cache) |>
        dplyr::select(
          tidyselect::all_of(c(group_cols,"cvc_days","pvc_days"))) |>
        tidyr::pivot_longer(
          !tidyselect::all_of(group_cols),
          names_pattern = "^([^_]+)",
          names_to = "dev",
          values_to = "days"),
      by = c(group_cols,"dev")) |>
    dplyr::bind_rows(
      x$events |>
        dplyr::inner_join(
          x$pneumoniaData |>
            dplyr::select("event_key","dev_ass") |>
            dplyr::filter(.data$dev_ass != 0) |>
            dplyr::mutate(
              dev = dplyr::case_match(
                as.integer(as.character(.data$dev_ass)),
                1 ~ "niv",
                2 ~ "inv"),
              .keep = "unused"),
          dplyr::join_by("event_key")) |>
        dplyr::group_by(dplyr::across(tidyselect::all_of(
          c(group_cols,"dev")))) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        tidyr::pivot_wider(
          names_from = "dev",
          values_from = "n",
          values_fill = 0) |>
        dplyr::rowwise() |>
        dplyr::mutate(vs = sum(dplyr::c_across(tidyselect::any_of(c("niv","inv"))))) |>
        dplyr::ungroup() |>
        tidyr::pivot_longer(
          !tidyselect::all_of(c(group_cols)),
          names_to = "dev",
          values_to = "n") |>
        dplyr::right_join(
          get_risk_time(x, group_cols, use_cache) |>
            dplyr::select(
              tidyselect::all_of(c(group_cols,"inv_days","niv_days","vs_days"))) |>
            tidyr::pivot_longer(
              !tidyselect::all_of(group_cols),
              names_pattern = "^([^_]+)",
              names_to = "dev",
              values_to = "days"),
          by = c(group_cols,"dev"))) |>
    dplyr::mutate(
      n = tidyr::replace_na(.data$n, 0),
      rate = .data$n / .data$days * 1000) |>
    dplyr::select(!"days") |>
    cache(x, cache_key)
}

get_incidence_density_rates <- function(x, group_cols = NULL, use_cache = TRUE)
{
  if(is.null(group_cols))
    cache_key <- "incidence_density_rates"
  else
    cache_key <- paste0("incidence_density_rates_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  r <- x$events |>
    dplyr::filter(.data$event_type_key %in% c("bsi","nec","hap")) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(
      c("event_type_key",group_cols)))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = "event_type_key",
      values_from = "n",
      values_fill = 0)  |>
    dplyr::rowwise() |>
    dplyr::mutate(si = sum(dplyr::c_across(tidyselect::any_of(c("bsi","hap"))))) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(!tidyselect::all_of(group_cols), names_to = "inf", values_to = "n")

  if(is.null(group_cols))
    r <- r |>
    dplyr::bind_cols(
      get_risk_time(x, use_cache = use_cache) |>
        dplyr::select("patient_days"))
  else
    r <- r |>
    dplyr::right_join(
      get_risk_time(x, group_cols, use_cache) |>
        dplyr::select(tidyselect::all_of(c(group_cols,"patient_days"))),
      by = group_cols)

  r |>
    dplyr::mutate(
      n = tidyr::replace_na(.data$n, 0),
      rate = .data$n / .data$patient_days * 1000) |>
    dplyr::select(!"patient_days") |>
    cache(x, cache_key)
}

get_infectious_agent_detection_rates_with_department_quartiles <- function(x, group_cols = NULL, use_cache = TRUE)
{
  if(is.null(group_cols))
    cache_key <- "infectious_agent_detection_rates_with_department_quartiles"
  else
    cache_key <- paste0("infectious_agent_detection_rates_with_department_quartiles_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  inf_with_pathogen <- x |>
    get_infection_counts(
      group_cols = c("department_key","with_pathogen"),
      use_cache = use_cache) |>
    dplyr::filter(.data$with_pathogen) |>
    dplyr::pull("n")

  n_deps <- length(inf_with_pathogen)
  median_inf_with_pathogen <- stats::median(inf_with_pathogen)

  r1 <- x |>
    get_infectious_agent_detection_rates(
      group_cols = group_cols,
      use_cache = use_cache) |>
    dplyr::select(c(group_cols,"n","rate"="n_per_iwp")) |>
    dplyr::mutate(
      drop_quartiles = n_deps < 5 | round(100 / .data$rate) >= median_inf_with_pathogen)

  if(nrow(r1) < 1)
  {
    gc <- list(rep(NA_character_, length(group_cols)))
    names(gc) <- group_cols
    return(
      tibble::tibble(
        n = 0,
        rate = NaN,
        drop_quartiles = TRUE,
        q1 = NA,
        q2 = NA,
        q3 = NA
        ) |>
        dplyr::bind_rows(gc)
      )
  }

  r2 <- x |>
    get_infectious_agent_detection_rates(
      group_cols = c("department_key", group_cols),
      use_cache = use_cache) |>
    dplyr::select(c("department_key", group_cols,"n_per_iwp"))

  if (!is.null(group_cols))
  {
    r2 <- r2 |>
    tidyr::pivot_wider(
      names_from = group_cols,
      values_from = "n_per_iwp",
      values_fill = 0)

    glue_spec <- "{.value}_{name}"
  }
  else glue_spec <- NULL

  r2 <- r2 |>
    dplyr::select(!"department_key") |>
    dplyr::reframe(
      dplyr::across(
        tidyselect::everything(),
        ~stats::quantile(.x, prob = c(.25,.5,.75), na.rm = TRUE))) |>
    dplyr::bind_cols(tibble::tibble(name = c("q1","q2","q3"))) |>
    tidyr::pivot_wider(values_from = !"name", names_glue = glue_spec) |>
    tidyr::pivot_longer(
      tidyselect::everything(),
      names_pattern = paste0(
        c("^", rep("(.+)_", length(group_cols)), "(q(?:1|2|3))$"),
        collapse = ""),
      names_to = c(group_cols,".value")) |>
    dplyr::mutate(
      dplyr::across(tidyselect::any_of(group_cols), ~ dplyr::na_if(.x,"NA")))

  if (is.null(group_cols))
    r <- r1 |> dplyr::bind_cols(r2)
  else
    r <- r1 |>
    dplyr::mutate(dplyr::across(tidyselect::any_of(group_cols), as.character)) |>
    dplyr::inner_join(r2 , by = group_cols)

  r |> dplyr::mutate(
    q1 = dplyr::if_else(
      .data$drop_quartiles,
      NA,
      .data$q1),
    q2 = dplyr::if_else(
      .data$drop_quartiles,
      NA,
      .data$q2),
    q3 = dplyr::if_else(
      .data$drop_quartiles,
      NA,
      .data$q3)) |>
    cache(x, cache_key)
}

get_infectious_agent_detection_rates <- function(x, group_cols = NULL, use_cache = TRUE)
{
  if(is.null(group_cols))
    cache_key <- "infectious_agent_detection_rates"
  else
    cache_key <- paste0("infectious_agent_detection_rates_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  r <- x$events |>
    dplyr::inner_join(
      x$infectiousAgentFindings |>
        dplyr::inner_join(
          get_pathogen_taxonomy(
            x$infectiousAgentFindings$pathogen_key |> unique()),
          dplyr::join_by("pathogen_key" == "input_id")),
      dplyr::join_by("event_key")) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(group_cols)))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  # get_infection_counts cannot access specific pathogen related information
  # since this would lead to counting infections with multiple pathogens
  # multiple times
  inf_groups <- setdiff(group_cols, c(names(x$infectiousAgentFindings),names(get_pathogen_taxonomy(-1))))

  inf_counts <- x |>
    get_infection_counts(
      group_cols = c(inf_groups,"with_pathogen"),
      use_cache = use_cache) |>
    tidyr::pivot_wider(names_from = "with_pathogen", values_from = "n") |>
    dplyr::mutate(
      inf_with_pathogen = .data$`TRUE`,
      total_inf = .data$`TRUE` + .data$`FALSE`,
      .keep = "unused")

  if(is.null(group_cols))
    r <- r |> dplyr::bind_cols(inf_counts)
  else
    r <- r |>
    dplyr::right_join(inf_counts, by = inf_groups) |>
    dplyr::mutate(n = tidyr::replace_na(.data$n, 0))

  r |>
    dplyr::mutate(
      n_per_iwp = .data$n / .data$inf_with_pathogen * 100,
      n_per_t = .data$n / .data$total_inf * 100,
      iwp_per_t = .data$inf_with_pathogen / .data$total_inf * 100
    ) |>
    cache(x, cache_key)
}

get_infection_counts <- function(x, group_cols = NULL, use_cache = TRUE)
{
  if(is.null(group_cols))
    cache_key <- "infection_counts"
  else
    cache_key <- paste0("infection_counts_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  inf_events <- c("bsi","nec","hap","ssi")

  event_base <- x$patients |>
    dplyr::mutate(
      bw50 = bw50(.data$birth_weight),
      bw125 = bw125(.data$birth_weight),
      bw250 = bw250(.data$birth_weight),
      bw500 = bw500(.data$birth_weight),
      comp_gw = as.integer(.data$total_gestation_days / 7)) |>
    dplyr::inner_join(
      x$enrollments |>
        dplyr::select(
          c(
            "patient_key",
            !tidyselect::any_of(c(names(x$patients))))
        ) |>
        dplyr::inner_join(
          x$events |>
            dplyr::select(
              c(
                "enrollment_key",
                !tidyselect::any_of(c(names(x$patients),names(x$enrollments))))
            ),
          dplyr::join_by("enrollment_key")),
      dplyr::join_by("patient_key")) |>
    dplyr::filter(.data$event_type_key %in% inf_events) |>
    dplyr::mutate(
      event_type_key = factor(
        as.character(.data$event_type_key),
        levels = inf_events
      ),
      with_pathogen = .data$event_key %in% x$infectiousAgentFindings$event_key)

  counts <- event_base |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  if(is.null(group_cols))
    return(counts |>
             cache(x, cache_key))

  ee_intersect <- intersect(
    intersect(names(event_base), group_cols),
    names(x$enrollments))

  expanded <- x$enrollments |>
    dplyr::left_join(
      event_base |>
        dplyr::select(
          tidyselect::all_of(
            setdiff(c("enrollment_key","event_key",group_cols), ee_intersect))),
      dplyr::join_by("enrollment_key")) |>
    # Make sure both values for with_pathogen are expanded if necessary
    dplyr::bind_rows(list(with_pathogen = c(TRUE,FALSE))) |>
    tidyr::expand(!!! dplyr::syms(group_cols)) |>
    tidyr::drop_na()

  expanded |>
    dplyr::left_join(counts, by = group_cols) |>
    dplyr::mutate(
      n = tidyr::replace_na(.data$n, 0)) |>
    cache(x, cache_key)
}

get_resistance_test_rate_with_department_quartiles <- function(x, resistance, group_cols = NULL, use_cache = TRUE)
{
  rate <- x |>
    get_resistance_test_rate(
      resistance = resistance,
      group_cols = group_cols,
      use_cache = use_cache)

  deps <- x |>
    get_resistance_test_rate(
      resistance = resistance,
      group_cols = c("department_key", group_cols),
      use_cache = use_cache) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols)))

  dep_stats <- deps |>
    dplyr::summarise(
      n_deps = dplyr::n(),
      median = stats::median(.data$total),
      .groups = "drop")

  quartiles <- deps |>
    dplyr::reframe(
      value = stats::quantile(
        .data$rate,
        prob = c(.25,.5,.75),
        na.rm = TRUE)) |>
    dplyr::mutate(
      name=names(.data$value),
      name=dplyr::case_match(
        .data$name,
        "25%"~"q1",
        "50%"~"q2",
        "75%"~"q3")) |>
    tidyr::pivot_wider()

  if(is.null(group_cols))
    rate <- rate |>
    dplyr::bind_cols(dep_stats) |>
    dplyr::bind_cols(quartiles)
  else
    rate <- rate |>
    dplyr::inner_join(dep_stats, by = group_cols) |>
    dplyr::inner_join(quartiles, by = group_cols)

  rate |>
    dplyr::mutate(
      drop_quartiles = .data$n_deps < 5 | round(100 / .data$rate) >= .data$median,
      q1 = dplyr::if_else(
        .data$drop_quartiles,
        NA,
        .data$q1),
      q2 = dplyr::if_else(
        .data$drop_quartiles,
        NA,
        .data$q2),
      q3 = dplyr::if_else(
        .data$drop_quartiles,
        NA,
        .data$q3)) |>
    dplyr::select(!c("n_deps","median", "drop_quartiles"))
}

get_resistance_test_rate <- function(x, resistance, group_cols = NULL, use_cache = TRUE)
{
  res_names <- c("3gcr","car","cor","mrsa","vre")
  resistance <- rlang::arg_match(
    arg = resistance,
    res_names)

  check_character(group_cols, allow_na = FALSE, allow_null = TRUE)

  check_bool(use_cache)

  if(is.null(group_cols))
    cache_key <- paste0("resistance_test_rate_", resistance)
  else
    cache_key <- paste0("resistance_test_rate_", resistance, "_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  x$events |>
    dplyr::inner_join(x$infectiousAgentFindings, dplyr::join_by("event_key")) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(resistance),
        ~ factor(
          dplyr::case_match(
            as.character(.x),
            "yes" ~ "tested",
            "no" ~ "tested",
            "not_tested" ~ "not_tested"),
          levels = c("tested","not_tested"))),
      # For the grouping columns that are resistances, we assume NA to be yes
      # because if we don't ask for resistance that's typically because of a
      # primary resistance
      dplyr::across(
        tidyselect::all_of(intersect(group_cols, res_names)),
        ~ tidyr::replace_na(.x, "yes")
      )) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(group_cols, resistance)))) |>
    dplyr::summarise(
      value=dplyr::n(),
      .groups = "drop") |>
    dplyr::filter(!is.na(.data[[resistance]])) |>
    tidyr::pivot_wider(
      names_from = resistance,
      values_fill = 0L,
      names_expand = TRUE) |>
    dplyr::mutate(
      tested = .data$tested,
      not_tested = .data$not_tested,
      total = .data$tested + .data$not_tested,
      rate = .data$tested / .data$total * 100,
      .keep = "unused") |>
    cache(x, cache_key)
}

get_resistance_rate_with_department_quartiles <- function(x, resistance, group_cols = NULL, use_cache = TRUE)
{
  rate <- x |>
    get_resistance_rate(
      resistance = resistance,
      group_cols = group_cols,
      use_cache = use_cache)

  deps <- x |>
    get_resistance_rate(
      resistance = resistance,
      group_cols = c("department_key", group_cols),
      use_cache = use_cache) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols)))

  if (nrow(deps) < 1) {
    return(
      tibble::tibble(!!!group_cols, .rows = 1, .name_repair = ~ group_cols) |>
        dplyr::bind_cols(
          tibble::tibble(
            n = 0L,
            rate = 0,
            q1 = NA_real_,
            q2 = NA_real_,
            q3 = NA_real_
          ))
    )
  } else {
    dep_stats <- deps |>
      dplyr::summarise(
        n_deps = dplyr::n(),
        median = stats::median(.data$inf_w_ia),
        .groups = "drop")

    quartiles <- deps |>
      dplyr::reframe(
        value = stats::quantile(
          .data$inf_rs_rate,
          prob = c(.25,.5,.75),
          na.rm = TRUE)) |>
      dplyr::mutate(
        name=names(.data$value),
        name=dplyr::case_match(
          .data$name,
          "25%"~"q1",
          "50%"~"q2",
          "75%"~"q3")) |>
      tidyr::pivot_wider()
  }

  if (is.null(group_cols))
    rate <- rate |>
    dplyr::bind_cols(dep_stats) |>
    dplyr::bind_cols(quartiles)
  else
    rate <- rate |>
    dplyr::inner_join(dep_stats, by = group_cols) |>
    dplyr::inner_join(quartiles, by = group_cols)

  rate |>
    dplyr::mutate(
      drop_quartiles = .data$n_deps < 5 | round(100 / .data$inf_rs_rate) >= .data$median,
      q1 = dplyr::if_else(
        .data$drop_quartiles,
        NA,
        .data$q1),
      q2 = dplyr::if_else(
        .data$drop_quartiles,
        NA,
        .data$q2),
      q3 = dplyr::if_else(
        .data$drop_quartiles,
        NA,
        .data$q3)) |>
    dplyr::select(!c("inf_nrs", "inf_tst_tot", "inf_w_ia", "ia_rs", "ia_nrs",
                     "ia_tst_tot", "ia_rs_rate","n_deps","median",
                     "drop_quartiles")) |>
    dplyr::rename("n"="inf_rs","rate"="inf_rs_rate")
}

get_resistance_rate <- function(x, resistance, group_cols = NULL, use_cache = TRUE)
{
  res_names <- c("3gcr","car","cor","mrsa","vre")
  resistance <- rlang::arg_match(
    arg = resistance,
    res_names)

  check_character(group_cols, allow_na = FALSE, allow_null = TRUE)

  check_bool(use_cache)

  if(is.null(group_cols))
    cache_key <- paste0("resistance_rate_", resistance)
  else
    cache_key <- paste0("resistance_rate_", resistance, "_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  inf_group_cols <- c(
    names(x$patients),
    names(x$enrollments),
    names(x$events)) |>
    unique() |>
    intersect(group_cols)

  inf_w_ia <- x |>
    get_infection_counts(
      group_cols = c("with_pathogen", inf_group_cols),
      use_cache = use_cache) |>
    dplyr::filter(.data$with_pathogen) |>
    dplyr::select(!"with_pathogen")

  r <- x$patients |>
    dplyr::mutate(
      bw50 = bw50(.data$birth_weight),
      bw125 = bw125(.data$birth_weight),
      bw250 = bw250(.data$birth_weight),
      bw500 = bw500(.data$birth_weight),
      comp_gw = as.integer(.data$total_gestation_days / 7)) |>
    dplyr::inner_join(
      x$enrollments |>
        dplyr::select(
          c(
            "patient_key",
            !tidyselect::any_of(c(names(x$patients))))
        ) |>
        dplyr::inner_join(
          x$events |>
            dplyr::select(
              c(
                "enrollment_key",
                !tidyselect::any_of(c(names(x$patients),names(x$enrollments))))
            ) |>
            dplyr::inner_join(
              x$infectiousAgentFindings |>
                dplyr::select(
                  c(
                    "event_key","secondary_bsi","pathogen_key","index",
                    "source",tidyselect::all_of(resistance))
                ) |>
                dplyr::inner_join(
                  get_pathogen_taxonomy(),
                  dplyr::join_by("pathogen_key" == "input_id")),
              dplyr::join_by("event_key")),
          dplyr::join_by("enrollment_key")),
      dplyr::join_by("patient_key")) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(group_cols, resistance)))) |>
    dplyr::summarise(
      inf = dplyr::n_distinct(.data$event_key),
      ia_dtct = dplyr::n(),
      .groups = "drop") |>
    dplyr::filter(!is.na(.data[[resistance]]) & .data[[resistance]] != "not_tested") |>
    tidyr::pivot_wider(
      names_from = resistance,
      values_from = c("inf","ia_dtct"),
      values_fill = 0L,
      names_expand = TRUE) |>
    dplyr::select(!tidyselect::ends_with("not_tested"))

  if(length(inf_group_cols) < 1)
    r <- r |>
    dplyr::bind_cols(inf_w_ia)
  else
    r <- r |>
    dplyr::inner_join(inf_w_ia, by = inf_group_cols)

  r |>
    dplyr::mutate(
      inf_rs = .data$inf_yes,
      inf_nrs = .data$inf_no,
      inf_tst_tot = .data$inf_rs + .data$inf_nrs,
      inf_w_ia = .data$n,
      ia_rs = .data$ia_dtct_yes,
      ia_nrs = .data$ia_dtct_no,
      ia_tst_tot = .data$ia_rs + .data$ia_nrs,
      ia_rs_rate = .data$ia_rs / .data$ia_tst_tot * 100,
      inf_rs_rate = .data$inf_rs / .data$inf_w_ia * 100,
      .keep = "unused") |>
    cache(x, cache_key)
}

get_risk_population <- function(x, group_cols = NULL, use_cache = TRUE)
{
  if(is.null(group_cols))
    cache_key <- "risk_population"
  else
    cache_key <- paste0("risk_population_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  x$patients |>
    dplyr::inner_join(
      x$enrollments |>
        dplyr::select(tidyselect::all_of(c("patient_key",setdiff(names(x$enrollments),names(x$patients))))),
      dplyr::join_by("patient_key")) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) |>
    dplyr::summarise(
      n_enrollments = dplyr::n(),
      n_patients = dplyr::n_distinct(.data$patient_key),
      .groups = "drop") |>
    cache(x, cache_key)
}

get_surgery_risk <- function(x, group_cols = NULL, use_cache = TRUE)
{
  if(is.null(group_cols))
    cache_key <- "surgery_risk"
  else
    cache_key <- paste0("surgery_risk_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  x$patients |>
    dplyr::mutate(
      bw50 = bw50(.data$birth_weight),
      bw125 = bw125(.data$birth_weight),
      bw250 = bw250(.data$birth_weight),
      bw500 = bw500(.data$birth_weight),
      comp_gw = as.integer(.data$total_gestation_days / 7)) |>
    dplyr::inner_join(
      x$enrollments |>
        dplyr::select(
          c(
            "patient_key",
            !tidyselect::any_of(c(names(x$patients))))
        ) |>
        dplyr::inner_join(
          x$events |>
            dplyr::select(
              c(
                "enrollment_key",
                !tidyselect::any_of(c(names(x$patients),names(x$enrollments))))
            ) |>
            dplyr::inner_join(
              x$surveillanceEndData |>
                dplyr::inner_join(
                  get_aware_days(x, use_cache),
                  dplyr::join_by("event_key")),
              dplyr::join_by("event_key")),
          dplyr::join_by("enrollment_key")) |>
        dplyr::inner_join(
          x$events |>
            dplyr::select(c("enrollment_key","event_key")) |>
            dplyr::inner_join(
              x$surgeryData |>
                dplyr::mutate(
                  main_procedure_category = get_procedure_category(.data$main_procedure_code, not_surgery_na = TRUE),
                  dplyr::across(
                    tidyselect::any_of(
                      "side_procedure_code_1"),
                    ~ get_procedure_category(.x, not_surgery_na = TRUE),
                    .names = "side_procedure_1_category"),
                  dplyr::across(
                    tidyselect::any_of(
                      "side_procedure_code_2"),
                    ~ get_procedure_category(.x, not_surgery_na = TRUE),
                    .names = "side_procedure_2_category")) |>
                dplyr::filter(dplyr::if_any(tidyselect::matches("procedure_(\\d_)?category$"), ~ !is.na(.x))),
              dplyr::join_by("event_key")),
          dplyr::join_by("enrollment_key")),
      dplyr::join_by("patient_key")) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) |>
    dplyr::summarise(
      dplyr::across(tidyselect::any_of("department_key"), ~ dplyr::n_distinct(.x), .names = "n_departments"),
      dplyr::across(tidyselect::any_of("patient_key"), ~ dplyr::n_distinct(.x), .names = "n_patients"),
      n_procedures = dplyr::n(),
      .groups = "drop") |>
    cache(x, cache_key)
}

get_risk_time <- function(x, group_cols = NULL, use_cache = TRUE)
{
  if(is.null(group_cols))
    cache_key <- "risk_time"
  else
    cache_key <- paste0("risk_time_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
      return(r)

  x$patients |>
    dplyr::mutate(
      bw50 = bw50(.data$birth_weight),
      bw125 = bw125(.data$birth_weight),
      bw250 = bw250(.data$birth_weight),
      bw500 = bw500(.data$birth_weight),
      comp_gw = as.integer(.data$total_gestation_days / 7)) |>
    dplyr::inner_join(
      x$enrollments |>
        dplyr::select(
          c(
            "patient_key",
            !tidyselect::any_of(c(names(x$patients))))
        ) |>
        dplyr::inner_join(
          x$events |>
            dplyr::select(
              c(
                "enrollment_key",
                !tidyselect::any_of(c(names(x$patients),names(x$enrollments))))
            ) |>
            dplyr::inner_join(
              x$surveillanceEndData |>
                dplyr::inner_join(
                  get_aware_days(x, use_cache),
                  dplyr::join_by("event_key")),
              dplyr::join_by("event_key")),
          dplyr::join_by("enrollment_key")),
      dplyr::join_by("patient_key")) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) |>
    dplyr::summarise(dplyr::across(!"total_gestation_days" & tidyselect::ends_with("_days"), sum), .groups = "drop") |>
    dplyr::mutate(
      dplyr::across(
        !tidyselect::all_of(c(group_cols,"patient_days")),
        ~ .x / .data$patient_days * 100,
        .names = "{.col}_rate")) |>
    dplyr::rename_with(~ stringr::str_remove_all(.x, "days_")) |>
    cache(x, cache_key)
}

get_procedures <- function(x, group_cols = NULL, use_cache = TRUE)
{
  if(is.null(group_cols))
    cache_key <- "procedures"
  else
    cache_key <- paste0("procedures_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  x$events |>
    dplyr::inner_join(
      x$surgeryData |>
        dplyr::inner_join(
          get_procedure_categories(x, use_cache = use_cache),
          dplyr::join_by("main_procedure_code" == "procedure_code")),
      dplyr::join_by("event_key")) |>
    dplyr::filter(.data$pro_cat != "not_surgery") |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    cache(x, cache_key)
}

get_aware_days <- function(x, use_cache = TRUE)
{
  if(use_cache && !is.null(r <- get_cached(x, "aware_days")))
    return(r)

  x$substanceDays |>
    dplyr::group_by(.data$event_key, .data$substance_code) |>
    dplyr::summarise(days = sum(.data$days), .groups = "drop") |>
    dplyr::inner_join(
      x$metadata$antimicrobialSubstances |>
        dplyr::mutate(
          AWaRe = factor(
            tolower(
              stringr::str_extract(
                .data$WHO_AWARE,
                "^WHO_AWARE_(A|W|R).+$",
                group = 1)),
            levels = c("a","w","r"))) |>
        dplyr::select(tidyselect::all_of(c("code", "AWaRe"))),
      dplyr::join_by("substance_code" == "code")) |>
    dplyr::group_by(.data$event_key, .data$AWaRe) |>
    dplyr::summarise(days = sum(.data$days), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = "AWaRe",
      values_from = "days",
      names_glue = "{AWaRe}_{.value}",
      values_fill = 0L) |>
    cache(x, "aware_days")
}

get_procedure_categories <- function(x, pretty = FALSE, include_iche = FALSE,
                                     use_cache = TRUE)
{
  cache_key <- "procedure_categories"

  # ToDo: Clarify licensing and inclusion criteria for ICHE information with WHO
  # and add ICHE table
  # if(include_iche)
  #   cache_key <- paste0(cache_key, ".iche")

  if(pretty)
  {
    l <- Sys.getenv("LANGUAGE")
    if(l == "")
      l <- Sys.getlocale("LC_MESSAGES")
    if(l == "")
      pretty <- FALSE # just in case
    else
      cache_key <- paste0(cache_key, ".", l)
  }

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  r <- tibble::tibble(
    procedure_code = c(
      x$surgeryData$main_procedure_code,
      x$surgeryData$side_procedure_code_1,
      x$surgeryData$side_procedure_code_2) |>
      unique() |>
      sort()) |>
    dplyr::mutate(pro_cat = get_procedure_category(.data$procedure_code))

  if(pretty)
  {

    with_pretty <- r |>
      dplyr::select("pro_cat") |>
      dplyr::mutate(
        pretty_name = get_procedure_category_pretty(.data$pro_cat))

    pairs <- with_pretty |> dplyr::distinct()

    col_names <- stats::setNames(
      gettext("Procedure code","Procedure category"),
      c("procedure_code","pro_cat"))
    row_names <- stats::setNames(pairs$pretty_name, pairs$pro_cat)

    attr(r, "names.pretty") <- col_names
    attr(r, "row.names.pretty") <- row_names

    r <- r |>
    dplyr::mutate(
      pro_cat = with_pretty$pretty_name) |>
    dplyr::rename(
      !!col_names[["procedure_code"]] := .data$procedure_code,
      !!col_names[["pro_cat"]] := .data$pro_cat)
  }

  # if(include_iche)
  #   r <- r |>
  #   dplyr::inner_join(
  #     ichi_health_interventions,join_by(main_procedure_code == code))

  r |>
    cache(x, cache_key)
}

get_procedure_category <- function(x, not_surgery_na = FALSE)
{
  target <- stringr::str_extract(x, "^([A-Za-z]{3})\\.", 1)
  action <- stringr::str_extract(x, "^([A-Za-z]{3})\\.([A-Za-z]{2})", 2)
  means <- stringr::str_extract(x, "^([A-Za-z]{3})\\.([A-Za-z]{2})\\.([A-Za-z]{2})", 3)
  if (not_surgery_na) {
    not_surgery <- NA_character_
  } else {
    not_surgery <- "not_surgery"
  }

  dplyr::case_when(
    is.na(x) ~ NA_character_,
    # Neurosurgery
    ############################################################################
    target %in% c(
      "AAE",# Interventions on ventricles of brain
      "AAG",# Interventions on intracranial space
      "ABA",# Interventions on spinal cord
      "ABG",# Interventions on spinal canal
      "MAA" # Interventions on skull
      ) &
      means %in% c("AA","AB","AE") ~ "neurosurgery",

    # Cardiac/large vessel surgery
    ############################################################################
    target == "HIJ" & action == "LA" ~ "cardiac_and_large_vessel_surgery",

    target == "HIK" & means == "AA" ~ "cardiac_and_large_vessel_surgery",

    # Lung/pleural space/thoracic surgery
    ############################################################################
    target %in% c(
      "MCX",# Interventions on diaphragm
      "JBF",# Interventions on lung parenchyma
      "JCA",# Interventions on pleura
      "JCB",# Interventions on pleura
      "JCH" # Interventions on thoracic cavity
    ) &
      means %in% c("AA","AB") ~ "lung_pleural_space_thoracic_surgery",

    # Oesophageal surgery
    ############################################################################
    target == "KBA" & means %in% c("AA","AB") ~ "oesophageal_surgery",

    # Abdominal surgery
    ############################################################################
    target %in% c(
      "KBF",# Interventions on stomach
      "KBI",# Interventions on duodenum
      "KBK",# Interventions on small intestine, not elsewhere classified
      "KBO",# Interventions on appendix
      "KBP",# Interventions on colon
      "KBZ",# Interventions on large intestine, not elsewhere classified
      "KMA",# Interventions on peritoneum
      "PAK",# Interventions on abdomen, not otherwise specified
      "PAL",# Interventions on abdominal wall, not otherwise specified
      "PAO" # Interventions on abdominal wall, umbilical
      ) &
      means %in% c("AA","AB") ~ "abdominal_surgery",

    target %in% c(
      "PTA",
      "PTB"
      ) &
      action == "LA" &
      means == "AC" ~ "abdominal_surgery",

    x == "KMA.JB.AE" ~ "abdominal_surgery",# Percutaneous drainage of peritoneal cavity
    x == "KZZ.MK.AA" ~ "abdominal_surgery",# Repair of intestine, not elsewhere classified
    x == "PAK.JB.AE" ~ "abdominal_surgery",# Percutaneous abdominal drainage

    # Inguinal hernia surgery
    ############################################################################
    x %in% c(
      "PAM.MK.AA",# Repair of inguinal hernia
      "PAM.MK.AB" # Laparoscopic repair of inguinal hernia
    ) ~ "inguinal_hernia_surgery",

    # Other
    ############################################################################
    x %in% c(
      "BCC.GA.AA",# Destruction of retina
      "BCD.DB.AE",# Injection into vitreous body
      "HDG.LG.AF",# Percutaneous transluminal balloon dilatation of pulmonary valve
      "HIB.DL.AF",# Percutaneous transluminal insertion of device into superior vena cava
      "IBD.DL.AF",# Percutaneous transluminal insertion of device into vein of head and neck
      "IZD.DL.AF",# Insertion of a device into a vein, not elsewhere classified
      "JAN.AE.AC",# Laryngoscopy
      "JAN.MK.AD",# Endoscopic repair of larynx
      "JAM.ML.AD",# Endoscopic reconstruction of nasopharynx
      "JBA.AE.AB",# Tracheoscopy through artificial stoma
      "JBA.KA.AC",# Replacement of tracheal device
      "JBA.LI.AA",# Tracheostomy
      "JBA.MK.AA",# Repair of trachea
      "KAA.AD.AA",# Biopsy of lip
      "KAB.FB.AC",# Lingual fraenotomy
      "LAB.JG.AH",# Debridement of skin and subcutaneous cell tissue of trunk, without incision
      "LAB.LL.AA",# Reduction of skin and subcutaneous cell tissue of trunk
      "LCA.JG.AA",# Debridement of breast with incision
      "NAM.MK.AA",# Repair of urethra
      "NGL.LC.AA",# Orchiopexy
      "NMR.MK.AB",# Endoscopic repair of fetal or embryonic structure
      "NZZ.ZZ.ZZ",# Interventions on the genitourinary system, unspecified
      "PAW.JB.AA" # Drainage of perineum
      ) ~ "other",

    # Not considered as surgery (remove)
    ############################################################################
    x %in% c(
      "ABA.BA.BH",# Magnetic resonance imaging of spinal cord
      "JBB.AE.AD",# Bronchoscopy
      "KBA.LG.AD",# Endoscopic dilatation of oesophagus
      "KBF.DL.AC",# Insertion of device into stomach
      "KBF.KA.AC",# Replacement of gastric device
      "KBK.LD.AH",# Manual reduction of ileostomy prolapse
      "LZZ.DK.AH",# Application of dressing to skin or subcutaneous cell tissue, not elsewhere classified
      "MBO.BA.BC",# Computerised tomography of lumbosacral spine, not elsewhere classified
      "PAB.BA.BH",# Magnetic resonance imaging of head or neck
      "PAE.BA.BH",# Magnetic resonance imaging of thorax
      "PAK.BA.BH",# Magnetic resonance imaging of abdomen
      "PTB.SN.AC",# Management of enterostomy
      "PTA.PM.ZZ",# Gastrostomy education
      "PTC.PM.ZZ",# Tracheostomy education
      "PZA.BA.BH" # Magnetic resonance imaging of whole body
      ) ~ not_surgery,

    # To be categorised (default)
    ############################################################################
    .default = "to_be_categorised"
  ) |>
    factor(
      levels = c(
        "abdominal_surgery",
        "neurosurgery",
        "inguinal_hernia_surgery",
        "cardiac_and_large_vessel_surgery",
        "lung_pleural_space_thoracic_surgery",
        "oesophageal_surgery",
        "other",
        not_surgery,
        "to_be_categorised"))
}

get_procedure_category_pretty <- function(x)
{
  dplyr::case_match(
    as.character(x),
    "overall" ~ gettext("Overall"),
    "abdominal_surgery" ~ gettext("Abdominal surgery"),
    "neurosurgery" ~ gettext("Neurosurgery"),
    "inguinal_hernia_surgery" ~ gettext("Inguinal hernia surgery"),
    "cardiac_and_large_vessel_surgery" ~ gettext("Cardiac- / large vessel surgery"),
    "lung_pleural_space_thoracic_surgery" ~ gettext("Lung- / pleural space- / thoracic surgery"),
    "oesophageal_surgery" ~ gettext("Oesophageal surgery"),
    "other" ~ gettext("Other"),
    "not_surgery" ~ gettext("Not a surgical procedure"),
    "to_be_categorised" ~ gettext("Not yet categorised"),
    .default = x
  )
}

ga7 <- function(x) 7 * dplyr::case_match(
  as.integer(x %% 7),
  0L ~ as.integer(x / 7),
  1L ~ as.integer(x / 7),
  2L ~ as.integer(x / 7),
  3L ~ as.integer(x / 7),
  4L ~ as.integer(x / 7) + 1,
  5L ~ as.integer(x / 7) + 1,
  6L ~ as.integer(x / 7) + 1)

bw50 <- function(x, as_factor = TRUE)
{
  m <- floor((x-25)/50)*50+50
  if(!as_factor)
    return(m)

  if(length(x) < 1)
    return(factor())

  lb <- m-25
  ub <- m+24
  ordered(
    m,
    levels = seq(min(m), max(m), 50),
    labels = paste0(format(seq(min(lb), max(lb), 50))," g - ",format(seq(min(ub), max(ub), 50))," g"))
}

bw125 <- function(x, as_factor = TRUE)
{
  m <- floor((x-63)/125)*125+125
  if(!as_factor)
    return(m)

  if(length(x) < 1)
    return(factor())

  lb <- m-62
  ub <- m+62
  ordered(
    m,
    levels = seq(min(m), max(m), 125),
    labels = paste0(format(seq(min(lb), max(lb), 125))," g - ",format(seq(min(ub), max(ub), 125))," g"))
}

bw250 <- function(x, as_factor = TRUE)
{
  m <- floor((x-125)/250)*250+250
  if(!as_factor)
    return(m)

  if(length(x) < 1)
    return(factor())

  lb <- m-125
  ub <- m+124
  ordered(
    m,
    levels = seq(min(m), max(m), 250),
    labels = paste0(format(seq(min(lb), max(lb), 250))," g - ",format(seq(min(ub), max(ub), 250))," g"))
}

bw500 <- function(x, as_factor = TRUE)
{
  m <- as.integer(x/500)*500+250
  if(!as_factor)
    return(m)

  if(length(x) < 1)
    return(factor())

  lb <- m-250
  ub <- m+249
  ordered(
    m,
    levels = seq(min(m), max(m), 500),
    labels = paste0(format(seq(min(lb), max(lb), 500))," g - ",format(seq(min(ub), max(ub), 500))," g"))
}

add_class <- function(x, class_name)
{
  check_string(class_name, allow_empty = FALSE)
  class(x) <- c(class_name, class(x))
  return(x)
}

cache <- function(x, container, key)
{
  container$.cache[[key]] = x
  return(x)
}

clean_cache <- function(x)
{
  rm(list = ls(envir = x$.cache), envir = x$.cache)
}

new_cache <- function(x)
{
  x$.cache <- new.env(parent = emptyenv())
  x
}

get_cached <- function(container, key)
{
  if (!is.null(container$.cache) && !is.null(r <- get0(key, envir = container$.cache)))
    return(r)

  return(NULL)
}
