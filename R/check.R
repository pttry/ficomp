#' Check misssing values in variable by geo and time
#'
#' @param data A data.frame
#' @param var A name of column to check
#'
#' @export
#'
#' @examples
#'   check_na(data_main_annual, nulc_aper)
#'



check_na <- function(data, var){
  data |>
    dplyr::select(geo, time, {{ var }}) |>
    tidyr::complete(geo, time) |>
    dplyr::filter(is.na({{ var }})) |>
    dplyr::distinct(geo, time)
}


