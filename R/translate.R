#' Translate columns from code to names in data.frame
#'
#'
#' @param .tbl A tbl object.
#' @param .var A unquoted column name to translate.
#' @param trans_vec A named tranlation vector. Codes as names.
#'
#' @import rlang
#' @export
#'
#' @examples
#'   tbl <- tibble::tibble(geo = c("FI", "SE"))
#'   t_geo <- c(FI = "Suomi", SE = "Ruotsi")
#'   translate(tbl, geo, t_geo)
#'
translate <- function(.tbl, .var, trans_vec){
  var <- enquo(.var)
  tbl <- dplyr::mutate(.tbl, !!var := dplyr::recode(!!var, !!!trans_vec))
  tbl
}



