#' Translate columns from code to names in data.frame
#'
#'
#' @param .tbl A tbl object.
#' @param .var A unquoted column name to translate.
#' @param trans_vec A named tranlation vector. Codes as names.
#' @param simple if TRUE simplyfied by \code{\link{simple_lab}}.
#'        By default only first part of the name is taken, split by ",".
#' @param ... further arguments to \code{\link{simple_lab}}.
#'
#' @import rlang
#' @export
#'
#' @examples
#'   tbl <- tibble::tibble(geo = c("FI", "SE"))
#'   t_geo <- c(FI = "Suomi", SE = "Ruotsi")
#'   translate(tbl, geo, t_geo)
#'   translate(tibble::tibble(vars = c("nulc", "nulc_va", "nulc_hw", "nulc_hw_va", "nulc_aper", "nulc_aper_va")),
#'             vars, var_labels_fi, simple = TRUE, parts = c(3, 4))
#'
translate <- function(.tbl, .var, trans_vec, simple = FALSE, ...){

  if (simple) {
    trans_vec <- simple_lab(trans_vec, ...)
  }

  var <- enquo(.var)
  tbl <- dplyr::mutate(.tbl, !!var := dplyr::recode(!!var, !!!trans_vec))
  tbl
}


#' Simplify labels (, or all vectors)
#'
#' Split based on a pattern and includes selected parts
#'
#' @param x A vector to simplify.
#' @param pattern A  ", " Pattern to look for.
#' @param parts, A position of parts to include. Default 1.
#'
#' @export
#'
#' @examples
#' simple_lab(c(nulc = "unit labour cost, nominal"), parts = 1)
#' simple_lab(x = c(nulc = "unit labour cost, nominal", nulc_va = "unit labour cost, nominal, value added"), parts = c(2,3))
#' simple_lab(x = as.factor(c(nulc = "unit labour cost, nominal", nulc_va = "unit labour cost, nominal, value added")), parts = c(2,3))
#' simple_lab(var_labels_fi[c(36:41)], parts = c(3,4))
#'
simple_lab <- function(x, parts = 1, pattern = ", "){

  if (is.factor(x)) {
    y <- x
    levels(y) <- simple_lab(levels(y), parts = parts, pattern = pattern)
  } else {
    y <- strsplit(x, split = pattern)
    y <- purrr::map_chr(y, ~paste0(na.omit(.x[parts]), collapse = ", "))
  }
  y
}


