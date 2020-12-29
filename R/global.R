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
