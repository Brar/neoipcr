ccs <- get_pathogen_list() |>
  dplyr::filter(.data$is_cc) |> dplyr::pull(id)

get_cc_multiple <- function(x, cc_ids)
{
  max_index <- names(x) |>
    stringr::str_extract("\\d+") |>
    as.integer() |>
    max()

  pair <- tibble::tibble(.rows = nrow(x))
  for (i in 1:max_index) {
    tmp <- x |>
      dplyr::select(
        dplyr::any_of(
          c(
            paste0("pathogen_",i,"_value"),
            paste0("pathogen_",i,"_multiple_value"))))

    pair <- pair |>
      dplyr::bind_cols(
        tmp |>
          dplyr::mutate(
            dplyr::across(
              dplyr::all_of(paste0("pathogen_",i,"_value")),
              \(x) dplyr::if_else(is.na(x), NA, x %in% ccs),
              .names = paste0("pathogen_",i,"_is_cc")),
            !!paste0("pathogen_",i,"_is_cc_multiple") := dplyr::pick(
              dplyr::any_of(c(
                paste0("pathogen_",i,"_is_cc"),
                paste0("pathogen_",i,"_multiple_value")))) |>
              (\(x){
                if(ncol(x) < 2)
                  dplyr::if_else(is.na(x[[1]]), NA, FALSE)
                else
                  dplyr::case_when(
                    is.na(x[[1]]) ~ NA,
                    x[[1]] == TRUE & x[[2]] == TRUE ~ TRUE,
                    .default = FALSE)})(),
            .keep = "unused"
          )
      )
  }
  pair
}

get_cc_info <- function(x){
  x |>
    dplyr::mutate(
      all_cc = dplyr::if_all(
        dplyr::ends_with("is_cc"), .fns = ~ .x == TRUE | is.na(.x)) &
        dplyr::if_any(dplyr::ends_with("is_cc"), .fns = ~ !is.na(.x)),
      any_cc_multiple = all_cc &
        dplyr::if_any(dplyr::ends_with("multiple"), .fns = ~ .x == TRUE),
      all_cc_multiple = all_cc &
        dplyr::if_all(
          dplyr::ends_with("multiple"), .fns = ~ .x == TRUE | is.na(.x)) &
        dplyr::if_any(dplyr::ends_with("multiple"), .fns = ~ !is.na(.x)),
      .keep = "unused"
    )
}
