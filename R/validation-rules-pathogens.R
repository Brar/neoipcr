# Find infection events where the unknown pathogen is recorded.
validation_rule_20 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  # TODO: Implement
  r <- dplyr::bind_cols(
    rule_id = c(20L),
    x$enrollments |>
      dplyr::select("enrollment_key") |>
      dplyr::filter(.data$enrollment_key == -1))

  return(r)
}
