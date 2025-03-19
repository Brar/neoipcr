## code to prepare `sysdata` dataset goes here


# We need to clarify whether this use complies with the ICD-11 License agreement before actually incorporating anything into the package.
# ICHI is an unreleased proposal and is only available via the WHO-FIC Maintenance Platform where the following license is presented:
# https://icd.who.int/dev11/Content/helpfiles.ICD11_en/licenseagreement.html
# The general ICD-11 License at https://icd.who.int/en/docs/ICD11-license.pdf is https://creativecommons.org/licenses/by-nd/3.0/igo/ and
# explicitly allows incorporation of ICD-11 into a software product if certain prohibited actions are avoided.
# https://www.who.int/classifications/international-classification-of-health-interventions looks encouraging.ichiUrl <- "https://icd.who.int/dev11/Downloads/Download?fileName=LinearizationMiniOutput-ICHI-en.zip"

# tmp <- tempfile()
#
# httr2::request(ichiUrl) |>
#   httr2::req_perform() |>
#   httr2::resp_body_raw() |>
#   readr::write_file(tmp)
#
# raw <- unz(tmp, grep("^LinearizationMiniOutput-ICHI-en\\.txt$", unzip(tmp, list = TRUE)$Name, value = TRUE)) |>
#   readr::read_file_raw()
#
# file.remove(tmp)
#
# headers <- raw |>
#   readr::read_tsv(col_names = FALSE, n_max = 1, show_col_types = FALSE) |>
#   unlist()
#
# version <- headers |>
#   tail(1) |>
#   stringr::str_replace("^.+?:", "")
#
# data <- raw |>
#   readr::read_tsv(col_names = head(headers, -1), skip = 1, show_col_types = FALSE)
#
# data <- data |>
#   dplyr::mutate(
#     Id = dplyr::row_number(),
#     Level = tidyr::replace_na(nchar(stringr::str_extract(Title, "^(- )+")) / 2, 0), .before = 1)
#
# create_object <- function(x){
#   list(
#     `Foundation URI` = jsonlite::unbox(x$`Linearization (release) URI`),
#     Title = jsonlite::unbox(stringr::str_replace(x$Title, "^(- )*(.+)$", "\\2")),
#     ClassKind = jsonlite::unbox(x$ClassKind),
#     DepthInKind = jsonlite::unbox(x$DepthInKind),
#     IsResidual = jsonlite::unbox(x$IsResidual),
#     PrimaryLocation = jsonlite::unbox(x$PrimaryLocation)
#   )
# }
#
# get_children <- function(parent, data, i){
#   current_level <- list()
#   browser()
#   while (i < nrow(data)) {
#     obj <- create_object(data[i,])
#     if(length(current_level) == 0 || obj$ClassKind == current_level[[1]]$ClassKind && obj$DepthInKind == current_level[[1]]$DepthInKind){
#       current_level <- append(current_level, list(obj))
#     }
#     else if (obj$DepthInKind > current_level[[1]]$DepthInKind) {
#       o <- current_level[length(current_level)]
#       c <- get_children(o, data, i)
#       o$Children <- c$Children
#       i <- c$i
#     }
#     else {
#       return(list(Children = current_level, i = i))
#     }
#     i <- i + 1
#   }
# }
#
# create_objects <- function(data){
#   ICHI <- list()
#   browser()
#   i <- 1
#   while (i <= nrow(data)) {
#     obj <- create_object(data[i,])
#     c <- get_children(obj, data, i + 1)
#     obj$Children <- c$Children
#     i <- c$i
#     ICHI <- append(ICHI, list(obj))
#     i <- i + 1
#   }
#   ICHI
# }
#
# create_object(data)
#
# get_parent_id <- function(own_id, own_level)
# {
#   purrr::map2(own_id, own_level, \(x, y){ data |> dplyr::filter(Id < x & Level < y) |> dplyr::slice_tail(n = 1) |> dplyr::pull(Id) }) |> unlist()
# }
#
# children <- data |>
#   dplyr::filter(Level > 0) |>
#   dplyr::mutate(
#     Id = Id,
#     Parent = get_parent_id(Id, Level),
#     Title = stringr::str_replace(Title, "^(- )*(.+)$", "\\2"),
#     URL = stringr::str_replace(BrowserLink, "^=hyperlink\\(\"([^\"]+)\".*$", "\\1"),
#     .keep = "unused") |>
#   dplyr::select(Id, Parent, Code, Title, IsResidual, isLeaf, URL)
#
# extension_code_id <- data  |> dplyr::filter(Title == "ICHI Extension Code") |> dplyr::pull(Id)
#
# target_id <- children |> dplyr::filter(Title == "Target") |> dplyr::pull(Id)
# action_id <- children |> dplyr::filter(Title == "Action") |> dplyr::pull(Id)
# means_id <- children |> dplyr::filter(Title == "Means") |> dplyr::pull(Id)
# health_interventions_id <- children |> dplyr::filter(Title == "Health interventions") |> dplyr::pull(Id)
#
# ichi_targets <- children |>
#   dplyr::filter(Id > target_id & Id < action_id)
#
# ichi_actions <- children |>
#   dplyr::filter(Id > action_id & Id < means_id)
#
# ichi_means <- children |>
#   dplyr::filter(Id > means_id & Id < health_interventions_id)
#
# ichi_health_interventions <- children |>
#   dplyr::filter(Id > health_interventions_id & Id < extension_code_id)


internal_pathogen_concepts <- readr::read_csv("https://raw.githubusercontent.com/Brar/Surveillance-Toolkit/refs/heads/rtl_languages/metadata/common/pathogens/NeoIPC-Pathogen-Concepts.csv", col_types = "icffcllllll")
internal_pathogen_synonyms <- readr::read_csv("https://raw.githubusercontent.com/Brar/Surveillance-Toolkit/refs/heads/rtl_languages/metadata/common/pathogens/NeoIPC-Pathogen-Synonyms.csv", col_types = "icfci")

usethis::use_data(internal_pathogen_concepts, internal_pathogen_synonyms, internal = TRUE, overwrite = TRUE, compress = "xz")
