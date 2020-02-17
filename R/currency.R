#' calculate EUR values from other currencies
#'
#' @param x a numeric vector to turn to euros.
#' @param time a time (year) vector.
#' @param currency a vector of currencies.
#' @param eur_rate a data.frame of exchange rates
#'
#' @export

EUR <- function(x, time, currency, eur_rate){
  eur_rate$currency <- as.character(eur_rate$currency)
  z <- tibble::tibble(x = x, time= time, currency = as.character(currency))
  z <- dplyr::left_join(z, eur_rate, by = c("time", "currency"))
  y <- mutate(z, eur = x / values)$eur
  y
}
