#' Mass weighting
#'
#' Weight all variables except
#'
#' @param .data A tbl.
#' @param geo,
#' @param weitht_df A weigthing data.frame in long form. Should have time, geo_base and geo columns.
#' @param geo A vector to indicate countries.
#' @param time A year (or date which is converted to a year) for weights.
#' @param weitht_df A weigthing data.frame in long form. Should have time, geo_base and geo columns.
#'
#' @export
#' @import dplyr
#'
weight_all <- function(.data, geo, time, except, weight_df){
  w_name <- deparse1(substitute(weight_df))
  fun_name <- paste0("rel", gsub("weights", "", x = w_name))
  fun_list <- list(rel = ~weight_index(., geo = geo, time = time, weight_df = weight_df))
  names(fun_list) <- fun_name

  y <- group_by(.data, time) %>%
    mutate_at(vars(-matches(except)),
              .funs = fun_list) %>%
    ungroup()

  y
}



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
#' w <- c(0.25,0.5,0.25, NA)
#' weighted_gmean(x, w, na.rm = FALSE)
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
#' x <- c(1, 2, 1)
#' geo <- c("FI", "DE", "SE")
#' w_df <- tibble(geo_base = c("FI", "FI", "FI", "DE", "DE", "DE", "SE", "SE", "SE"),
#'                geo =      c("FI", "DE", "SE", "FI", "DE", "SE", "FI", "DE", "SE"),
#'                time = 2015,
#'                weight =   c(NA, 0.5, 0.25, 1, NA, 1, 1, 1, NA))
#' weight_index(x, geo, 2015, w_df,na.rm = TRUE)
#' weight_index(x, geo, 2015, weight_df = weights_ecb, na.rm = TRUE)
weight_index <- function(x, geo, time, weight_df,
                         nearest = TRUE, na_zero = TRUE, na.rm = FALSE) {

  if (lubridate::is.Date(time)) time <- lubridate::year(time)
  if (!all(time == mean(time))) stop("Time should be unique")
  time <- time[1]

  if (any(is.na(x))){
    if (!na.rm) {
      return(rlang::rep_along(x, NA))
    } else {
        warning("Index missing in ", time, " for: ", paste0(geo[is.na(x)], collapse = ", "))
      }
    }

  if (nearest) time <- weight_df$time[which.min(abs(weight_df$time-time))]

  in_base <- geo %in% weight_df$geo_base
  if (any(!in_base)) stop("Missing geo(s) in weight_df geo_base: ", paste0(geo[!in_base], collapse = ", "))

  in_geo <- geo %in% weight_df$geo
  if (any(!in_geo)) stop("Missing geo(s) in weight_df geo: ", paste0(geo[!in_geo], collapse = ", "))

  w_df <- weight_df[weight_df$time == time, ]

  # w <- weight_df[weight_df$time == time & weight_df$geo %in% geo & weight_df$geo_base %in% geo,]
  # w <- w[order(match(w$geo, geo)),]
  if (na_zero) w_df$weight[is.na(w_df$weight)] <- 0

  weighted_other <- purrr::map_dbl(geo, ~ weight_function(.x, x, geo, w_df))
  y <- 100 * x / weighted_other
}


#' Weight function for weight_index

weight_function <- function(one_geo, x, geo, w_df){

  w_df2 <- w_df[w_df$geo_base == one_geo, ]
  w <- w_df2[match(geo, w_df2$geo),]
  # check for weights
  if (any(is.na(w$weight) & w$geo != one_geo)) stop("Weights missing in ", one_geo, " for: ",
                                                    paste0(geo[is.na(w$weight) & w$geo != one_geo],
                                                           collapse = ", "))
  if (is.na(w$weight[w$geo == one_geo])) x[w$geo == one_geo] <- NA
  weighted_gmean(x, w$weight, na.rm = TRUE)
}
