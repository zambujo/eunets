`%>%` <- magrittr::`%>%` # export the pipe operator

# read --------------------------------------------------------------------

organizations <-
  here::here("data-raw", "cordis-h2020organizations.csv") %>%
  readr::read_csv2() %>%
  janitor::clean_names()

projects <-
  here::here("data-raw", "cordis-h2020projects.csv") %>%
  readr::read_csv2() %>%
  janitor::clean_names()

country_selection <- yaml::read_yaml(here::here("countries.yml"))

# logic -------------------------------------------------------------------

list_to_df <- function(el, el_names)
  dplyr::tibble(country = el, group = el_names)

info_country <-
  purrr::map2_df(country_selection,
                 names(country_selection),
                 list_to_df)

project_dimensions <- projects %>%
  dplyr::select(
    rcn,
    funding_scheme,
    call,
    programme,
    start_date,
    end_date,
    total_cost,
    max_contribution = ec_max_contribution
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

# write -------------------------------------------------------------------

h2020 <- h2020 %>%
  dplyr::left_join(project_dimensions, by = "rcn") %>%
  dplyr::left_join(info_country, by = "country") %>%
  dplyr::left_join(nunique_countries, by = "rcn") %>%
  dplyr::mutate(
    group = forcats::fct_explicit_na(group, na_level = "tc"),
  )


assertthat::see_if(identical(nrow(organizations), nrow(dplyr::distinct(h2020))),
                   msg = "nrow(organizations) != nrow(distinct(h2020))")

# nrow(dplyr::distinct(dplyr::filter(h2020, nunique == 1), rcn)) / nrow(dplyr::distinct(h2020, rcn))
# ca. 2/3 of one-country projects in H2020

h2020 %>%
  # readr::write_csv(gzfile(here::here("data", "h2020.csv.gz")))
  readr::write_csv(here::here("data", "h2020.csv"))
