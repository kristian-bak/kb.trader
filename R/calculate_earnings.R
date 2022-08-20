#' Get all actions
#' @return character vector with possible actions
#' @examples
#' get_all_actions()
get_all_actions <- function() {

  c("Buy", "Hold", "Sell")

}

#' Zero if NA
#' @param x vector of any class
#' @return vector of same class as x
#'
zero_if_na <- function(x) {

  if (length(x) == 0) {
    return(0)
  }

  ifelse(is.na(x), 0, x)

}

#' Get average buy price
#' @param n_stocks_cur number of stocks currently holding (numeric value). Use NA if currently not holding any
#' @param n_stocks_new number of stocks buying (numeric value)
#' @param price_cur current average buy price (numeric value). Use NA if currently not holding any
#' @param price_new new buy price
#' @details get_avg_buy_price updates the average buy price by averaging current
#' buy price and new buy price weighted by the number of stocks having and buying
#' @return numeric value
#' @examples
#' get_avg_buy_price(n_stocks_cur = 1, n_stocks_new = 1, price_cur = 15, price_new = 5)
get_avg_buy_price_loop <- function(n_stocks_cur, n_stocks_new, price_cur, price_new) {

  if (length(n_stocks_cur) == 0) {
    n_stocks_cur <- NA
  }

  if (length(price_cur) == 0) {
    price_cur <- NA
  }

  if (is.na(n_stocks_cur) & is.na(price_cur)) {
    return(price_new)
  }

  weighted.mean(x = c(price_cur, price_new), w = c(n_stocks_cur, n_stocks_new))

}

#' Get average buy price
#' @param StocksHolding tibble columns defined in calculate_earnings
#' @details This is a helper function for calculate_earnings
#'
get_avg_buy_price <- function(StocksHolding, BuyPrice, BuyAmount) {

  n <- length(data)
  AvgBuyPrice <- rep(NA, n)

  for (i in 1:n) {

    if (StocksHolding[i] == 0) {
      AvgBuyPrice[i] <- as.numeric(NA)
    } else if (StocksHolding[i] == 1) {
      AvgBuyPrice[i] <- BuyPrice[i]
    } else {
      AvgBuyPrice[i] <- weighted.mean(x = BuyPrice[1:i], w = BuyAmount[1:i], na.rm = TRUE)
    }
  }

  return(AvgBuyPrice)

}

#' Get relative difference
#' @param x numeric vector (benchmark value)
#' @param y numeric vector (comparison value)
#' @param digits number of digits (default is 2)
#'
get_relative_diff <- function(x, y, digits = 2) {

  round(100 * ((y - x) / x), digits = digits)

}

#' Calculate earnings
#' @param data tibble with stock data (see kb.yahoo::load_data)
#' @param var_price character string with price column (options: "Open", "High", "Low", "Close", "Adjusted" or "Avg")
#' @param verbose logical indicating if loop counter should be on or not
#' @return
calculate_earnings_loop <- function(data, var_price, verbose) {

  n <- nrow(data)

  data$Stocks            <- rep(NA, n)
  data$AvgBuyPrice       <- rep(NA, n)
  data$BuyPrice          <- rep(NA, n)
  data$RealisedEarning   <- rep(NA, n)
  data$UnRealisedEarning <- rep(NA, n)

  data <- data %>%
    dplyr::mutate(Avg = (Open + High + Low + Close) / 4)

  for (i in 1:n) {

    if (data$Action[i] == "Buy") {

      data$Stocks[i]      <- zero_if_na(data$Stocks[i - 1]) + 1
      data$BuyPrice[i]    <- data[[var_price]][i]
      data$AvgBuyPrice[i] <- get_avg_buy_price(
        n_stocks_cur = data$Stocks[i - 1],      ## Current number of stocks holding
        n_stocks_new = 1,                       ## Buying 1 stock
        price_cur    = data$AvgBuyPrice[i - 1], ## Current average buy price
        price_new    = data$BuyPrice[i]         ## New buy price
      )

    }

    last_buy_date <- data %>%
      dplyr::slice(1:i) %>%
      dplyr::filter(!is.na(BuyPrice)) %>%
      dplyr::slice(dplyr::n()) %>%
      dplyr::pull(Date)

    last_buy_id <- which(data$Date == last_buy_date)

    data$UnRealisedEarning[i] <- get_relative_diff(y = data[[var_price]][i], x = data$AvgBuyPrice[last_buy_id])

    if (data$Action[i] == "Sell") {

      data$RealisedEarning[i] <- get_relative_diff(y = data[[var_price]][i], x = data$AvgBuyPrice[last_buy_id])

    }

    if (verbose) {

      kb.utils::loop_counter(i = i, n = n)

    }

  }

  out <- list("data" = data)

  return(out)

}

#' Cumulative sum with reset
#' @param x numeric vector to sum
#' @param reset logical vector used to reset the cumulative sum (for instance is.na(x))
#' @return numeric vector
cumsum_with_reset <- function(x, reset) {

  dplyr::tibble(
    x = x,
    y = cumsum(reset)) %>% ## every new instance of the reset value will be have its own group, y
    dplyr::group_by(y) %>%
    dplyr::mutate(cum_sum = cumsum(x)) %>%
    dplyr::pull(cum_sum)

}

#' Data prep stock holdings earnings
#' @param data tibble with stock data (see kb.yahoo::load_data)
#' @param var_price character string with price column (options: "Open", "High", "Low", "Close", "Adjusted" or "Avg")
#' @return tibble with additional columns, including stockHoldings
data_prep_stock_holdings <- function(data, var_price) {

  data %>%
    dplyr::mutate(
      Avg = (Open + High + Low + Close) / 4,
      BuyAmount = dplyr::if_else(Action == "Buy", 1, 0),
      Sell = dplyr::if_else(Action == "Sell", TRUE, FALSE),
      StocksHolding = cumsum_with_reset(x = BuyAmount, reset = Sell),
      BuyPrice = dplyr::if_else(Action == "Buy", {{var_price}}, as.numeric(NA)),
      SellPrice = dplyr::if_else(Action == "Sell", {{var_price}}, as.numeric(NA))
    )

}

#' Data prep average buy price
#' @param data tibble obtained from data_prep_stock_holdings
#' @return tibble with column AvgBuyPrice (addition to input columns)
data_prep_avg_buy_price <- function(data) {

  data %>%
    dplyr::mutate(AvgBuyPrice = get_avg_buy_price(data = .))

}
