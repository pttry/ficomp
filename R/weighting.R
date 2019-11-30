#' Weighted geometric mean
#'
#' Calculate weighted geometric mean
#'
#' @param x a numeric vector
#' @param w a numeric vector for weights
#'
#' @export
#'

weighted_gmean <- function(x, w) { prod(x^prop.table(w)) }
