## code to prepare `sysdata` dataset goes here

internal_pathogen_concepts <- readr::read_csv(
  "https://raw.githubusercontent.com/Brar/Surveillance-Toolkit/refs/heads/rtl_languages/metadata/common/pathogens/NeoIPC-Pathogen-Concepts.csv",
  col_types = "icffcllllll")
internal_pathogen_synonyms <- readr::read_csv(
  "https://raw.githubusercontent.com/Brar/Surveillance-Toolkit/refs/heads/rtl_languages/metadata/common/pathogens/NeoIPC-Pathogen-Synonyms.csv",
  col_types = "icfci")

usethis::use_data(internal_pathogen_concepts, internal_pathogen_synonyms, internal = TRUE, overwrite = TRUE, compress = "xz")
