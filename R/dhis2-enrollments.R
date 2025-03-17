get_enrollments_request <- function(req_base, metadata_options)
{
  req_base |>
    httr2::req_url_path_append("enrollments") |>
    httr2::req_url_query(
      fields = "enrollment,createdAt,createdAtClient,updatedAt,updatedAtClient,trackedEntity,status,orgUnit,enrolledAt,occurredAt,followUp,deleted,completedAt,completedBy,storedBy,createdBy[username],updatedBy[username],notes")
}
