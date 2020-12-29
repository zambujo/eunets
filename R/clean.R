source(here::here("R", "global.R"))

# read --------------------------------------------------------------------

organizations <-
  here::here("data-raw", "cordis-h2020organizations.csv") %>%
  readr::read_csv2() %>%
  janitor::clean_names()

projects <-
  here::here("data-raw", "cordis-h2020projects.csv") %>%
  readr::read_csv2() %>%
  janitor::clean_names()

country_selection <- yaml::read_yaml(here::here("data", "countries.yml"))
programme_parts <- yaml::read_yaml(here::here("data", "cordis-programmes.yml"))

# logic -------------------------------------------------------------------

info_country <- list_to_df(country_selection, c("country", "group"))
info_programmes <- list_to_df(programme_parts, c("programme_slug", "programme_part"))

programme_dimensions <- projects %>%
  dplyr::select(rcn, programme) %>%
  dplyr::mutate(programme = stringr::str_split(programme, ";")) %>%
  tidyr::unnest(programme) %>%
  # mutate slug to be able to join with `info_programmes`
  dplyr::mutate(
    programme_slug = ifelse(
      stringr::str_detect(programme, "H2020-EU\\.[4567]"),
      stringr::str_sub(programme, 1, 11),
      stringr::str_sub(programme, 1, 13)))

programme_dimensions <- programme_dimensions %>%
  dplyr::left_join(info_programmes, by = "programme_slug") %>%
  dplyr::filter(!is.na(programme_part)) %>%
  dplyr::select(rcn, programme_part) %>%
  dplyr::distinct()

## c.a. 104 rcn cannot be matched (probably H2020-EU.2. + H2020-EC)
# dplyr::n_distinct(dplyr::pull(programme_dimensions, rcn))
# programme_dimensions %>% 
#   dplyr::filter(is.na(programme_part)) %>% 
#   dplyr::count(programme_slug, sort = TRUE)

project_dimensions <- projects %>%
  dplyr::select(
    rcn,
    start_date,
    end_date,
    max_contribution = ec_max_contribution,
    total_cost
  ) %>%
  dplyr::mutate(
    start_year = lubridate::year(start_date),
    end_year = lubridate::year(end_date),
    # duration in months
    duration = lubridate::interval(lubridate::ymd(start_date),
                                   lubridate::ymd(end_date)),
    duration = lubridate::as.duration(duration),
    duration = lubridate::time_length(duration, "month"),
    duration = round(duration)
  )

h2020 <- organizations %>%
  dplyr::select(
    rcn = project_rcn,
    role,
    pic = id,
    type = activity_type,
    contribution = ec_contribution,
    country
  )

nunique_countries <- h2020 %>%
  dplyr::group_by(rcn) %>%
  dplyr::summarise(nunique = dplyr::n_distinct(country),
                   .groups = "drop")

h2020 <- h2020 %>%
  dplyr::left_join(project_dimensions, by = "rcn") %>%
  dplyr::left_join(info_country, by = "country") %>%
  dplyr::left_join(nunique_countries, by = "rcn") %>%
  dplyr::mutate(group = forcats::fct_explicit_na(group, na_level = "tc"))

# write -------------------------------------------------------------------


## no observations got lost
assertthat::see_if(identical(nrow(organizations), nrow(dplyr::distinct(h2020))),
                   msg = "nrow(organizations) != nrow(distinct(h2020))")

# nrow(dplyr::distinct(dplyr::filter(h2020, nunique == 1), rcn)) / nrow(dplyr::distinct(h2020, rcn))
# ca. 2/3 of one-country projects in H2020

programme_dimensions %>%
  # readr::write_csv(gzfile(here::here("data", "programme_parts.csv.gz")))
  readr::write_csv(here::here("data", "programme_parts.csv"))

h2020 %>%
  # readr::write_csv(gzfile(here::here("data", "h2020.csv.gz")))
  readr::write_csv(here::here("data", "h2020.csv"))

