eu15 <- c("AT", "BE", "DK", "FI", "FR",
          "DE", "EL", "IE", "IT", "LU",
          "NL", "PT", "ES", "SE", "UK")
eu13 <- c("BG", "HR", "CY", "CZ",
          "EE", "HU", "LV", "LT",
          "MT", "PL", "RO", "SK",
          "SI")
other <- c("AM", "AL", "CH",
           "GE", "IL", "ME",
           "MK", "NO", "RS",
           "TN", "TR", "UA")

calculate_adjacency_matrix <- function(df) {
  df %>%
    table() %>%
    crossprod()
}

scale_by <- function(x, mini = 0, c = 1, maxi) {
  if (missing(maxi)) {
    maxi <- max(x)
  }
  c * (x - mini) / (maxi - mini)
}
