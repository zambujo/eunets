df2graph <- function(two_col_df) {
  two_col_df %>%
    table() %>%
    crossprod() %>%
    igraph::graph_from_adjacency_matrix(
      mode = "undirected",
      weighted = TRUE)
}

scale_by <- function(x, mini = 0, c = 1, maxi) {
  if (missing(maxi)) {
    maxi <- max(x)
  }
  c * (x - mini) / (maxi - mini)
}
