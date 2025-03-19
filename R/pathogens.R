get_pathogen_list <- function()
{
  pc <- internal_pathogen_concepts |>
    dplyr::rename("name" = "concept") |>
    dplyr::mutate(synonym_for = rlang::na_int)

  not_listed <- pc |>
    dplyr::slice_head()

  rest <- pc |>
    dplyr::filter(.data$id != 0) |>
    dplyr::bind_rows(
      internal_pathogen_synonyms |>
        dplyr::inner_join(
          internal_pathogen_concepts |>
            dplyr::select(!c("concept","concept_source","concept_id")),
          dplyr::join_by("synonym_for" == "id")) |>
        dplyr::relocate("concept_type", .before = "concept_source") |>
        dplyr::relocate("synonym_for", .after = "show_coli_r") |>
        dplyr::rename("name" = "synonym")) |>
    dplyr::arrange(.data$name)

  dplyr::bind_rows(not_listed, rest)
}
