download_file <- function(file_name,
                          download_to = "data-raw",
                          base_url = "https://cordis.europa.eu/data/") {
  if (stringr::str_detect(file_name, "cordisref"))
    base_url <- glue::glue("{base_url}reference/")
  file_url <- glue::glue("{base_url}{file_name}")
  usethis::ui_info(glue::glue("Downloading {file_url} to {download_to}..."))
  download.file(file_url, here::here(download_to, file_name))
}


# main --------------------------------------------------------------------

files <- yaml::read_yaml(here::here("cordis-data.yml"))
purrr::walk(files, download_file)
