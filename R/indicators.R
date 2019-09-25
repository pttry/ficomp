#' Rebase (or base) index
#'
#'
#' @param x a numeric vector. An index to rebase
#' @param time a time variable in a Date format.
#' @param baseyear a year or vector of years.
#'
#' @export
rebase <- function(x, time, baseyear) {
  time_year <- if (lubridate::is.Date(time)) lubridate::year(time) else time
  y <- 100 * x / mean(x[time_year %in% baseyear])
  y
}



#' Nominal unit labour costs
#'
#' @param cost Compensation of employees
#' @param output value added or gdp
#' @param input1 a labour input of costs (not neccessary if input = input2)
#' @param input2 a labour input of output (not neccessary if input = input2)
#'
#' @export

ind_ulc <- function(cost, output, input1 = 1, input2 = 1, time, baseyear){
  ind <- (cost / input1) / (output / input2)
  ind <- rebase(ind, time, baseyear)
  ind
}
