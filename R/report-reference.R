#' Calculate a NeoIPC reference data set
#'
#' @param ds The neoipcr_ds object containing the data
#' @param use_cache Use the cache
#'
#' @returns A NeoIPC reference data set
#' @export
calculate_reference_data <- function(ds, use_cache = TRUE)
{
  check_neoipcr_ds(ds)

  structure(
    list(
      usage_density_rate_table = get_usage_density_rate_table(ds, use_cache),
      incidence_density_rate_table = get_incidence_density_rate_table(ds, use_cache),
      dev_ass_incidence_density_rate_table = get_dev_ass_incidence_density_rate_table(ds, use_cache)
      ),
    class = "neoipcr_ref_ds")
}

#' Get the table with usage density rates of the time dependent risk factors
#'
#' @param ref The reference data set which can be either a neoipcr_ref_ds or a
#'  neoipcr_ds object
#' @param use_cache Use the cache. Ignored if ref is a neoipcr_ref_ds object
#'
#' @returns A table containing usage density rates of the time dependent risk
#'  factors
#' @export
get_usage_density_rate_table <- function(ref, use_cache = TRUE)
{
  check_neoipcr_ds_or_ref_ds(ref)

  if(is_neoipcr_ref_ds(ref))
    return(ref$usage_density_rate_table)

  if(use_cache && !is.null(r <- get_cached(ref, "usage_density_rate_table")))
    return(r)

  risk_time <- get_risk_time(ref, use_cache = use_cache)
  risk_rate_quartiles <- get_risk_time(
    ref, group_cols = "department_key", use_cache = use_cache) |>
    dplyr::select(tidyselect::ends_with("_rate")) |>
    dplyr::reframe(
      dplyr::across(
        tidyselect::everything(),
        ~quantile(.x, prob = c(.25,.5,.75))))

  risk_time |>
    dplyr::select(!"patient_days" & tidyselect::ends_with("_days")) |>
    tidyr::pivot_longer(
      cols = tidyselect::ends_with("_days"),
      values_to = "days") |>
    dplyr::mutate(
      factor = .data$name,
      .before = 1,
      .keep = "unused") |>
    dplyr::bind_cols(
      risk_time |>
        dplyr::select(tidyselect::ends_with("_rate")) |>
        tidyr::pivot_longer(
          cols = tidyselect::ends_with("_rate"),
          values_to = "mean")) |>
    dplyr::bind_cols(
      risk_rate_quartiles |>
        dplyr::bind_cols(tibble::tibble(name = c("q1","q2","q3"))) |>
        tidyr::pivot_wider(
          values_from = tidyselect::ends_with("_rate")) |>
        tidyr::pivot_longer(
          tidyselect::everything(),
          names_pattern = "^(.+)_(q(?:1|2|3))$",
          names_to = c("rate",".value"))) |>
    dplyr::select(!tidyselect::all_of(c("name","rate"))) |>
    dplyr::mutate(
      factor = factor(
        stringr::str_extract(.data$factor,"^(.+)_days", 1),
        levels = c("cvc","pvc","vs","inv","niv","human_milk","probiotic",
                   "kangaroo_care","ab","a","w","r"))) |>
    dplyr::arrange(.data$factor) |>
    add_class("neoipcr_tbl_udr_ref") |>
    cache(ref, "usage_density_rate_table")
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

  ref |>
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
            tidyselect::everything(), ~quantile(.x, prob = c(.25,.5,.75), na.rm = TRUE))) |>
        dplyr::bind_cols(tibble::tibble(name = c("q1","q2","q3"))) |>
        tidyr::pivot_wider(values_from = !"name") |>
        tidyr::pivot_longer(
          tidyselect::everything(),
          names_pattern = "^(.+)_(q(?:1|2|3))$",
          names_to = c("inf",".value")),
      dplyr::join_by("inf")) |>
    dplyr::mutate(inf = factor(.data$inf, levels = c("si","bsi","hap","nec"))) |>
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
            ~quantile(.x, prob = c(.25,.5,.75), na.rm = TRUE))) |>
        dplyr::bind_cols(tibble::tibble(name = c("q1","q2","q3"))) |>
        tidyr::pivot_wider(values_from = !"name") |>
        tidyr::pivot_longer(
          tidyselect::everything(),
          names_pattern = "^(.+)_(q(?:1|2|3))$",
          names_to = c("dev",".value")),
      dplyr::join_by("dev")) |>
    dplyr::mutate(
      dev = factor(.data$dev, levels = c("cvc","pvc","vs","inv","niv"))) |>
    dplyr::arrange(.data$dev) |>
    add_class("neoipcr_tbl_daidr_ref") |>
    cache(ref, "dev_ass_incidence_density_rate_table")

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
        dplyr::mutate(vs = .data$niv + .data$inv) |>
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
    dplyr::select(!"days")
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
      values_fill = 0) |>
    dplyr::mutate(si = .data$bsi + .data$hap) |>
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
    dplyr::select(!"patient_days")
}

get_risk_time <- function(x, group_cols = NULL, use_cache = TRUE)
{
  if(is.null(group_cols))
    cache_key <- "risk_time"
  else
    cache_key <- paste0("risk_time_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
      return(r)

  aware_days <- get_aware_days(x, use_cache)

  x$surveillanceEndData |>
    dplyr::inner_join(aware_days, dplyr::join_by("event_key")) |>
    dplyr::inner_join(
      x$events |>
        dplyr::select("event_key","enrollment_key"),
      dplyr::join_by("event_key")) |>
    dplyr::inner_join(
      x$enrollments |>
        dplyr::select(tidyselect::any_of(c("enrollment_key", group_cols))),
      dplyr::join_by("enrollment_key")) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) |>
    dplyr::summarise(dplyr::across(tidyselect::ends_with("_days"), sum), .groups = "drop") |>
    dplyr::mutate(
      dplyr::across(
        !tidyselect::all_of(c(group_cols,"patient_days")),
        ~ .x / .data$patient_days * 100,
        .names = "{.col}_rate")) |>
    dplyr::rename_with(~ stringr::str_remove_all(.x, "days_")) |>
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

get_cached <- function(container, key)
{
  if (!is.null(container$.cache) && !is.null(r <- get0(key, envir = container$.cache)))
    return(r)

  return(NULL)
}
