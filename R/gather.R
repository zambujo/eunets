files <-
  here::here("cordis-data.yml") %>%
  yaml::read_yaml()

download_file <- function(file_name,
                          download_to = "data-raw",
                          base_url = "https://cordis.europa.eu/data/") {
  if (stringr::str_detect(file_name, "cordisref"))
    base_url <- glue::glue("{base_url}reference/")
  
  file_url <- glue::glue("{base_url}{file_name}")
  usethis::ui_info(glue::glue("Downloading {file_url} to {download_to}..."))
  file_url %>%
    download.file(here::here(download_to, file_name))
}

purrr::walk(files, download_file)
