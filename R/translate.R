#' Translate columns from code to names in data.frame
#'
#'
#' @param .tbl A tbl object.
#' @param .var A unquoted column name to translate.
#' @param trans_vec A named tranlation vector. Codes as names.
#' @param simple if TRUE only first part of the name is takeny, split by ","
#'
#' @import rlang
#' @export
#'
#' @examples
#'   tbl <- tibble::tibble(geo = c("FI", "SE"))
#'   t_geo <- c(FI = "Suomi", SE = "Ruotsi")
#'   translate(tbl, geo, t_geo)
#'
translate <- function(.tbl, .var, trans_vec, simple = FALSE){

  if (simple) {
    trans_vec <- simple_lab(trans_vec)
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
simple_lab <- function(x, parts = 1, pattern = ", "){
  y <- purrr::map_chr(x, ~paste0(stringr::str_split(.x, pattern = pattern, simplify = TRUE)[, parts], collapse = ", "))
}




