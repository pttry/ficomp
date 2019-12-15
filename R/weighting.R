#' Weighted geometric mean
#'
#' Calculate weighted geometric mean
#'
#' @param x a numeric vector
#' @param w a numeric vector for weights
#'
#' @export
#' @examples
#' x <- c(1,2,3)
#' w <- c(0.5,1,0.5)
#' weighted_gmean(x, w)

weighted_gmean <- function(x, w) { prod(x^prop.table(w)) }



#' Calculate weighted index
#'
#' Uses weights from data frame
#'
#' @param x and vector to weight
#' @param a vector to indicate countries
#' @param a year for weights
#' @param weitht_df a weigthing data.frame in long form. Should have time, geo_base and geo columns.
#'
#' @export
weight_index <- function(x, geo, time, weight_df = weights_bis_broad) {
  w <- weight_df[weight_df$time == time & weight_df$geo %in% geo,]
  w <- w[order(match(w$geo, geo)),]
  y <- purrr::imap_dbl(geo, ~ 100 * weighted_gmean(x[.y]/ x[-.y],
                                                   w$weight[w$geo_base == .x & w$geo != .x]))
  y
}
