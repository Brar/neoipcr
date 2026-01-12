## code to prepare `sysdata` dataset goes here

internal_pathogen_concepts <- readr::read_csv(
  "https://raw.githubusercontent.com/NeoIPC/Surveillance-Toolkit/refs/heads/main/metadata/common/infectious-agents/NeoIPC-Pathogen-Concepts.csv",
  col_types = "icffclfllllliiiiiiiiii")
internal_pathogen_synonyms <- readr::read_csv(
  "https://raw.githubusercontent.com/NeoIPC/Surveillance-Toolkit/refs/heads/main/metadata/common/infectious-agents/NeoIPC-Pathogen-Synonyms.csv",
  col_types = "icfci")

internal_pathogen_list <- yaml::read_yaml(
  "https://raw.githubusercontent.com/NeoIPC/Surveillance-Toolkit/refs/heads/main/metadata/common/infectious-agents/NeoIPC-Infectious-Agents.yaml",
  handlers = list(
    'bool#yes' = function(x) x,
    'bool#no' = function(x) x))

usethis::use_data(internal_pathogen_concepts, internal_pathogen_synonyms, internal_pathogen_list, internal = TRUE, overwrite = TRUE, compress = "xz")
