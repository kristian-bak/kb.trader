#' Get price lag diff
#' @param data tibble with stock data
#' @param price_var price variable to use (Close, Low, High or Open)
#' @return tibble with additional columns
#'
get_price_lag_diff <- function(data, price_var) {

  data %>%
    dplyr::mutate(Price_lag = dplyr::lag({{price_var}}),
                  Price_lag2 = dplyr::lag(Price_lag),
                  "{{price_var}}_diff" := {{price_var}} - Price_lag,
                  "{{price_var}}_lag_diff" := Price_lag - Price_lag2) %>%
    dplyr::select(-Price_lag, -Price_lag2)

}

#' Get price optimum
#' @param data tibble obtained from get_price_lag_diff
#' @return tibble with additional columns
#'
get_price_optimum <- function(data) {

  data %>%
    dplyr::mutate(
      event = dplyr::case_when(
        Low_diff > 0 & Low_lag_diff < 0 ~ "Bund",
        High_diff < 0 & High_lag_diff > 0 ~ "Top",
        TRUE ~ "Intet"
      )
    )

}
