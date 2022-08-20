get_new_avg_buying_price <- function(current_price, buy_price, n_stocks, n_buy) {

  df <- dplyr::tibble(
    x = c(current_price, buy_price),
    w = c(n_stocks, n_buy)
  ) %>%
    dplyr::filter(!is.na(x))

  weighted.mean(x = df$x, w = df$w)

}
