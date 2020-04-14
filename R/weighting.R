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
#' Uses weights from data frame.
#'
#' @param x and vector to weight
#' @param a vector to indicate countries
#' @param a year (or date which is converted to a year) for weights
#' @param weitht_df a weigthing data.frame in long form. Should have time, geo_base and geo columns.
#' @param nearest a logical whether to use nearest year for weight table
#'
#' @return A weighted vector or vector of NA if any value in x is NA.
#'
#' @export
#' @examples
#'
#' x <- c(1, 2)
#' geo <- c("FI", "DE")
#' weight_index(x, geo, 2015)
weight_index <- function(x, geo, time, weight_df = weights_bis_broad, nearest = TRUE, na_zero = TRUE) {
  if (any(is.na(x))) return(rlang::rep_along(x, NA))
  if (lubridate::is.Date(time)) time <- lubridate::year(time)
  if (nearest) time <- weight_df$time[which.min(abs(weight_df$time-time))]
  w <- weight_df[weight_df$time == time & weight_df$geo %in% geo & weight_df$geo_base %in% geo,]
  w <- w[order(match(w$geo, geo)),]
  if (na_zero) w[is.na(w)] <- 0
  y <- purrr::imap_dbl(geo, ~ 100 * weighted_gmean(x[.y]/ x[-.y],
                                                   w$weight[w$geo_base == .x & w$geo != .x]))
  y
}
