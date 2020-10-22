#' Imput level series with change
#'
#' @param x A numeric vector to imput. NA values are imputed.
#' @param d A numeric vector of percentage change
#'
#' @export
#' @examples
#' t <- data.frame(a = c(NA, 2, NA, NA), b = c(0.7, 0.3, 0.1, 0.2))
#' dplyr::mutate(t, c = imput_change(a, b))
#'
imput_change <- function(x, d){

  # Only after last no-NA value
  n_last <- which.max(cumsum(!is.na(x)))
  d = dplyr::if_else(dplyr::row_number(d) <= n_last, d, NA_real_)
  # cumulative can
  d = cumprod((1+tidyr::replace_na(d, 0)))
  d = dplyr::if_else(dplyr::row_number(d) > n_last, d, NA_real_)
  y = d * x[n_last]
  y = dplyr::coalesce(x, y)
  y
}
