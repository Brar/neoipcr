validation_rule_1 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  r <- dplyr::bind_cols(
    rule_id = c(1L),
    x$patients |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key", "department_key")),
        "patient_key") |>
      dplyr::anti_join(
        x$enrollments,
        dplyr::join_by("patient_key")) |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key"))))

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","patient_key"))

  return(r)
}

validation_rule_2 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  if(!"status" %in% names(x$enrollments) || !"status" %in% names(x$events))
  {
    rlang::warn(paste(
      gettextf("Validation rule %i failed to execute.", 1),
      gettext("The dataset must contain the enrolment status and the event status to execute this rule.")))
    return()
  }

  r <- dplyr::bind_cols(
    rule_id = c(2L),
    x$enrollments |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key", "department_key")),
        "patient_key",
        "enrollment_key",
        "enrollment_status" = "status") |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "end") |>
          dplyr::select(
            "enrollment_key",
            "event_status" = "status"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::filter(.data$enrollment_status == "ACTIVE" & .data$event_status == "COMPLETED") |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key",
            "enrollment_key"))))

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","enrollment_key"))

  return(r)
}

validation_rule_3 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  r <- dplyr::bind_cols(
    rule_id = c(3L),
    x$enrollments |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key","department_key","patient_key")),
        "enrollment_key","enrolledAt") |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "adm") |>
          dplyr::select("enrollment_key","occurredAt"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::filter(.data$enrolledAt != .data$occurredAt) |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key","enrolledAt","occurredAt")) |>
    dplyr::group_by(dplyr::across(!c("enrolledAt","occurredAt"))) |>
    dplyr::summarise(
      context = list(
        list(
          enrolledAt = .data$enrolledAt,
          occurredAt = .data$occurredAt)))

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","enrollment_key"))

  return(r)
}

validation_rule_4 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  r <- dplyr::bind_cols(
    rule_id = c(4L),
    x$enrollments |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key","department_key","patient_key")),
        "enrollment_key") |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "adm") |>
          dplyr::select("enrollment_key","admOccurredAt"="occurredAt"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "end") |>
          dplyr::select("enrollment_key","endOccurredAt"="occurredAt"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::filter(.data$endOccurredAt < .data$admOccurredAt) |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key","admOccurredAt","endOccurredAt")) |>
    dplyr::group_by(dplyr::across(!c("admOccurredAt","endOccurredAt"))) |>
    dplyr::summarise(
      context = list(
        list(
          admOccurredAt = .data$admOccurredAt,
          endOccurredAt = .data$endOccurredAt)))

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","enrollment_key"))

  return(r)
}

validation_rule_5 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  if(!"status" %in% names(x$events))
  {
    rlang::warn(paste(
      gettextf("Validation rule %i failed to execute.", 1),
      gettext("The dataset must contain the event status to execute this rule.")))
    return()
  }

  r <- dplyr::bind_cols(
    rule_id = c(5L),
    x$enrollments |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key","department_key","patient_key")),
        "enrollment_key","enrolledAt") |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "adm") |>
          dplyr::select("enrollment_key","status"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::filter(.data$status != "COMPLETED") |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key","status")) |>
    dplyr::group_by(dplyr::across(!"status")) |>
    dplyr::summarise(
      context = list(
        list(
          status = .data$status)))

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","enrollment_key"))

  return(r)
}

validation_rule_6 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  if(!"status" %in% names(x$enrollments) || !"status" %in% names(x$events))
  {
    rlang::warn(paste(
      gettextf("Validation rule %i failed to execute.", 1),
      gettext("The dataset must contain the enrolment status and the event status to execute this rule.")))
    return()
  }

  r <- dplyr::bind_cols(
    rule_id = c(6L),
    x$enrollments |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key","department_key","patient_key")),
        "enrollment_key","enrollment_status" = "status") |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "end") |>
          dplyr::select("enrollment_key","status"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::filter(.data$status != "COMPLETED") |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key","status")) |>
    dplyr::group_by(dplyr::across(!"status")) |>
    dplyr::summarise(
      context = list(
        list(
          status = .data$status)))

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","enrollment_key"))

  return(r)
}

validation_rules <- list(
  list(
    id = 1,
    fun = validation_rule_1,
    formatter = function(x) {
      gettext("The patient record does not have an enrolment.")
    }
  ),
  list(
    id = 2,
    fun = validation_rule_2,
    formatter = function(x) {
      gettext(
        "The patient record has a completed surveillance end form but the enrolment is still active.")
    }
  ),
  list(
    id = 3,
    fun = validation_rule_3,
    formatter = function(x) {
      gettextf(
        "The admission date in the admission form (%s) differs from the admission date in the enrolment (%s).",
        format(x$occurredAt, format = "%x"),
        format(x$enrolledAt, format = "%x"))
    }
  ),
  list(
    id = 4,
    fun = validation_rule_4,
    formatter = function(x) {
      gettextf(
        "The date of the end of the surveillance (%s) is earlier than the date of admission on the admission form (%s).",
        format(x$endOccurredAt, format = "%x"),
        format(x$admOccurredAt, format = "%x"))
    }
  ),
  list(
    id = 5,
    fun = validation_rule_5,
    formatter = function(x) {
      gettextf(
        "The patient record's admission form is not completed (status is '%s').",
        as.character(x$status))
    }
  )
)

validate <- function(x, rules = NULL, exceptions = NULL)
{
  check_neoipcr_ds(x)

  r <- validation_rules |>
    lapply(\(r)if(is.null(rules)||r$id%in%rules)r$fun(x,exceptions)) |>
    dplyr::bind_rows()

  invisible(r)
}
