reference_data <- function(x, use_cache = TRUE)
{

}

get_usage_density_rate_table <- function(ref, use_cache = TRUE)
{
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
    dplyr::arrange(factor) |>
    cache(ref, "usage_density_rate_table")
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
