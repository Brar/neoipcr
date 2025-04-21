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
      surgery_rate_table = get_surgery_rate_table(ds, use_cache),
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

#' Get the table with rates of surgical precedues
#'
#' @param ref The reference data set which can be either a neoipcr_ref_ds or a
#'  neoipcr_ds object
#' @param use_cache Use the cache. Ignored if ref is a neoipcr_ref_ds object
#'
#' @returns A table containing the rates of surgical precedues
#' @export
get_surgery_rate_table <- function(ref, use_cache = TRUE)
{
  check_neoipcr_ds_or_ref_ds(ref)

  if(is_neoipcr_ref_ds(ref))
    return(ref$surgery_rate_table)

  if(use_cache && !is.null(r <- get_cached(ref, "surgery_rate_table")))
    return(r)

  tibble::tibble(
    procedure_category = "overall",
    n = get_procedures(ref, use_cache = use_cache) |>
      dplyr::pull()) |>
    dplyr::bind_rows(
      get_procedures(
        ref,
        group_cols = "procedure_category",
        use_cache = use_cache)
    ) |>
    dplyr::bind_cols(
      get_risk_population(ref, use_cache = use_cache) |>
        dplyr::select("n_patients")
    ) |>
    dplyr::mutate(rate = .data$n / .data$n_patients * 100) |>
    dplyr::select(!"n_patients") |>
    dplyr::inner_join(
      get_procedures(
        ref,
        group_cols = c("department_key", "procedure_category"),
        use_cache = use_cache) |>
        dplyr::bind_rows(
          get_procedures(
            ref,
            group_cols = "department_key",
            use_cache = use_cache)) |>
        dplyr::right_join(
          get_risk_population(
            ref,
            group_cols = "department_key",
            use_cache = use_cache) |>
            dplyr::select("department_key", "n_patients"),
          dplyr::join_by("department_key")) |>
        dplyr::mutate(
          n = tidyr::replace_na(.data$n, 0),
          procedure_category = tidyr::replace_na(
            as.character(.data$procedure_category), "overall"),
          rate = .data$n / .data$n_patients * 100) |>
        dplyr::select(!c("n","n_patients")) |>
        tidyr::pivot_wider(
          names_from = "procedure_category",
          values_from = "rate",
          values_fill = 0) |>
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
          names_to = c("procedure_category",".value")),
      dplyr::join_by("procedure_category")) |>
    add_class("neoipcr_tbl_sr_ref") |>
    cache(ref, "surgery_rate_table")
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

get_risk_population <- function(x, group_cols = NULL, use_cache = TRUE)
{
  if(is.null(group_cols))
    cache_key <- "risk_population"
  else
    cache_key <- paste0("risk_population_by.", paste0(group_cols, collapse = "."))

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  x$patients |>
    dplyr::inner_join(x$enrollments, dplyr::join_by("patient_key")) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) |>
    dplyr::summarise(
      n_enrollments = dplyr::n(),
      n_patients = dplyr::n_distinct(.data$patient_key),
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
    dplyr::filter(.data$procedure_category != "not_surgery") |>
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

get_procedure_categories <- function(x, use_cache = TRUE)
{
  if(use_cache && !is.null(r <- get_cached(x, "procedure_categories")))
    return(r)

  tibble::tibble(
    procedure_code = c(
      x$surgeryData$main_procedure_code,
      x$surgeryData$side_procedure_code_1,
      x$surgeryData$side_procedure_code_2) |>
      unique() |>
      sort()) |>
    dplyr::mutate(procedure_category = get_procedure_category(.data$procedure_code)) |>
    cache(x, "procedure_categories")
}

get_procedure_category <- function(x)
{
  target <- stringr::str_extract(x, "^([A-Za-z]{3})\\.", 1)
  action <- stringr::str_extract(x, "^([A-Za-z]{3})\\.([A-Za-z]{2})", 2)
  means <- stringr::str_extract(x, "^([A-Za-z]{3})\\.([A-Za-z]{2})\\.([A-Za-z]{2})", 3)
  dplyr::case_when(
    # Neurosurgery
    ############################################################################
    target %in% c("AAE","MAA") &
      means %in% c("AA","AB") ~ "neurosurgery",

    # Cardiac/large vessel surgery
    ############################################################################
    target == "HIJ" & action == "LA" ~ "cardiac_and_large_vessel_surgery",

    target == "HIK" & means == "AA" ~ "cardiac_and_large_vessel_surgery",

    # Lung/pleural space/thoracic surgery
    ############################################################################
    target == "MCX" & means %in% c("AA","AB") ~ "lung_pleural_space_thoracic_surgery",

    x == "JBF.LL.AA" ~ "lung_pleural_space_thoracic_surgery",

    # Oesophageal surgery
    ############################################################################
    target == "KBA" & means %in% c("AA","AB") ~ "oesophageal_surgery",

    # Abdominal surgery
    ############################################################################
    target %in% c("KBF","KBK","KBP","KBZ","KMA","PAK","PAL") &
      means %in% c("AA","AB") ~ "abdominal_surgery",

    target %in% c("PTA","PTB") &
      action == "LA" &
      means == "AC" ~ "abdominal_surgery",

    x == "KMA.JB.AE" ~ "abdominal_surgery",

    # Inguinal hernia surgery
    ############################################################################
    x == "PAM.MK.AA" ~ "inguinal_hernia_surgery",

    # Other
    ############################################################################
    x %in% c(
      "BCD.DB.AE",
      "IZD.DL.AF",
      "JAM.ML.AD",
      "JBA.LI.AA",
      "JBA.MK.AA",
      "KAB.FB.AC",
      "LAB.JG.AH",
      "LCA.JG.AA",
      "PAW.JB.AA") ~ "other",

    # Not considered as surgery (remove)
    ############################################################################
    x %in% c("KBA.LG.AD","KBK.LD.AH","PTB.SN.AC") ~ "not_surgery",

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
        "not_surgery",
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

add_class <- function(x, class_name)
{
  check_string(class_name, allow_empty = FALSE)
  class(x) <- c(class_name, class(x))
  return(x)
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
    gettext("Procedure category","Number of procedures","Pooled rate","Q1",
            "Q2","Q3"),
    c("procedure_category","n","rate","q1","q2","q3"))

  pairs <- x |>
    dplyr::select("procedure_category") |>
    dplyr::mutate(
      pretty_name = get_procedure_category_pretty(.data$procedure_category))

  row_names <- stats::setNames(pairs$pretty_name, pairs$procedure_category)

  attr(x, "names.pretty") <- col_names
  attr(x, "row.names.pretty") <- row_names

  x |>
    dplyr::inner_join(pairs, dplyr::join_by("procedure_category")) |>
    dplyr::mutate(procedure_category = .data$pretty_name, .keep = "unused") |>
    dplyr::rename_with(
      ~ dplyr::case_match(
        .x,
        "procedure_category"~col_names[["procedure_category"]],
        "n"~col_names[["n"]],
        "rate"~col_names[["rate"]],
        "q1"~col_names[["q1"]],
        "q2"~col_names[["q2"]],
        "q3"~col_names[["q3"]],
        .default = .x))
}

cache <- function(x, container, key)
{
  container$.cache[[key]] = x
  return(x)
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
