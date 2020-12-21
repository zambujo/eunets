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

info_country <- tibble::tibble(country = purrr::flatten_chr(country_selection),
                               group = "other") %>%
  dplyr::mutate(
    group = ifelse(
      country %in% purrr::pluck(country_selection, "eu15"),
      "eu15",
      group
    ),
    group = ifelse(
      country %in% purrr::pluck(country_selection, "eu13"),
      "eu13",
      group
    ),
    group = ifelse(
      country %in% purrr::pluck(country_selection, "ac"),
      "ac",
      group
    )
  )

meta <- projects %>%
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

df <- organizations %>%
  dplyr::select(
    rcn = project_rcn,
    role,
    pic = id,
    type = activity_type,
    contribution = ec_contribution,
    country
  )

nunique_countries <- df %>%
  dplyr::group_by(rcn) %>%
  dplyr::summarise(nunique = dplyr::n_distinct(country),
            .groups = "drop")

# nrow(distinct(filter(df, nunique == 1), rcn)) / nrow(distinct(df, rcn))
# ca. 2/3 of one-country projects in H2020

# write -------------------------------------------------------------------

df <- df %>%
  dplyr::left_join(meta, by = "rcn") %>%
  dplyr::left_join(info_country, by = "country") %>%
  dplyr::left_join(nunique_countries, by = "rcn")

assertthat::see_if(identical(nrow(organizations), nrow(dplyr::distinct(df))),
                   msg = "nrow(organizations) != nrow(distinct(df))")

df %>%
  # readr::write_csv(gzfile(here::here("data", "h2020.csv.gz")))
  readr::write_csv(here::here("data", "h2020.csv"))
