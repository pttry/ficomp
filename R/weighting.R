#' Weighted geometric mean
#'
#' Calculate weighted geometric mean
#'
#' @param x a numeric vector
#' @param w a numeric vector for weights
#' @param na.rm A logical. Should missing x values be removed?
#'
#' @export
#' @examples
#' x <- c(1,2,3, NA)
#' w <- c(0.25,0.5,0.25, 0)
#' weighted_gmean(x, w, na.rm = TRUE)
weighted_gmean <- function(x, w, na.rm = FALSE) {
  if (na.rm) w[is.na(x)] <- 0
  y <- prod(x^prop.table(w))
  y
  }


#' Calculate weighted index
#'
#' Uses weights from data frame.
#'
#' @param x A vector to weight.
#' @param geo A vector to indicate countries.
#' @param time A year (or date which is converted to a year) for weights.
#' @param weitht_df A weigthing data.frame in long form. Should have time, geo_base and geo columns.
#' @param nearest A logical whether to use nearest year for weight table.
#' @param na.rm A logical. Should missing values be removed? With FALSE (default) any NA will
#'        return NA.
#'
#' @return A weighted vector or vector of NA if any value in x is NA.
#'
#' @export
#' @examples
#'
#' x <- c(1, 2, NA)
#' geo <- c("FI", "DE", "SE")
#' w_df <- tibble(geo_base = c("FI", "FI", "FI", "DE", "DE", "DE", "SE", "SE", "SE"),
#'                geo =      c("FI", "DE", "SE", "FI", "DE", "SE", "FI", "DE", "SE"),
#'                time = 2015,
#'                weight =   c(NA, 0.5, 0.25, 1, NA, 1, 1, 1, NA))
#' weight_index(x, geo, 2015, w_df,na.rm = TRUE)
weight_index <- function(x, geo, time, weight_df,
                         nearest = TRUE, na_zero = TRUE, na.rm = FALSE) {
  if (!na.rm) if (any(is.na(x))) return(rlang::rep_along(x, NA))
  if (lubridate::is.Date(time)) time <- lubridate::year(time)
  if (nearest) time <- weight_df$time[which.min(abs(weight_df$time-time))]
  w <- weight_df[weight_df$time == time & weight_df$geo %in% geo & weight_df$geo_base %in% geo,]
  w <- w[order(match(w$geo, geo)),]
  if (na_zero) w$weight[is.na(w$weight)] <- 0

  y <- purrr::map_dbl(geo, ~ 100 * weighted_gmean(x,
                                                   w$weight[w$geo_base == .x],
                                                   na.rm = na.rm))
  y
}


