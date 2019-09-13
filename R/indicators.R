#' Nominal unit labour costs
#'
#' @param cost Compensation of employees
#' @param output value added or gdp
#' @param input1 a labour input of costs (not neccessary if input = input2)
#' @param input2 a labour input of output (not neccessary if input = input2)

ind_ulc <- function(cost, output, input1 = 1, input2 = 1, time, baseyear){
  time_year <- if (lubridate::is.Date(time)) lubridate::year(time) else time
  ind <- (cost / input1) / (output / input2)
  ind <- 100 * ind / mean(ind[time_year == baseyear])
  ind
}
