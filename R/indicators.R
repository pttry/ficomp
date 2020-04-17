#' Rebase (or base) index
#'
#'
#' @param x a numeric vector. An index to rebase
#' @param time a time variable in a Date format.
#' @param baseyear a year or vector of years.
#' @param basevalue index base values. if NULL value of x at base year.
#'
#' @export
rebase <- function(x, time, baseyear, basevalue = 100) {
  time_year <- if (lubridate::is.Date(time)) lubridate::year(time) else time
  if (is.null(basevalue)) basevalue <- mean(x[time_year %in% baseyear])
  y <- basevalue * x / mean(x[time_year %in% baseyear])
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
#' @example
#' ind_ulc(cost = c(1,2,3), output = c(NA,4,6), time = c(1,2,3), baseyear = 2)

ind_ulc <- function(cost, output, input1 = 1, input2 = 1, time, baseyear){
  ind <- (cost / input1) / (output / input2)
  ind <- rebase(ind, time, baseyear)
  ind
}
