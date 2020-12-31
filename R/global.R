`%>%` <- magrittr::`%>%` # export the pipe operator


list_to_df <- function(x, column_names) {
  .build_tibble <- function(el, el_names) {
    dplyr::tibble(el, el_names)
  }
  res <- purrr::map2_df(x, names(x), .build_tibble)
  if (!missing(column_names)) {
    names(res) = column_names
  }
  return(res)
}


country_selection <- yaml::read_yaml(here::here("data", "countries.yml"))
programme_parts <- yaml::read_yaml(here::here("data", "cordis-programmes.yml"))

info_country <- list_to_df(country_selection, c("country", "group"))
info_programmes <- list_to_df(programme_parts, c("programme_slug", "programme_part"))
