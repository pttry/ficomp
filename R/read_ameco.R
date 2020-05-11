#' Read Ameco files
#'
#' Gives warning because first line is longer than rest (one empty).
#'
#' @param table_num A link to a zip file.
#'
#' @export
#'
#' @import dplyr
#'
read_ameco <- function(table_num){
  link <- paste0("http://ec.europa.eu/economy_finance/db_indicators/ameco/documents/ameco", table_num, ".zip")
  tempf <- tempfile(fileext = ".zip")

  download.file(link, tempf)
  ameco_file <- unzip(tempf, paste0("AMECO", table_num,".TXT"), junkpaths = TRUE, exdir = tempdir())


  ameco_0 <- readr::read_delim(
    ameco_file, delim = ";",
  col_types = cols(
    CODE = col_character(),
    COUNTRY = col_factor(),
    "SUB-CHAPTER" = col_factor(),
    TITLE = col_factor(),
    UNIT = col_factor(),
    .default = col_number())) %>%
  select(-starts_with("X")) %>%
  statfitools::clean_names(to_lower = TRUE) %>%
  tidyr::gather(time, values, starts_with("x")) %>%
  mutate(time = as.numeric(gsub("x", " ", time)))

  unlink(tempf)

  ameco_0
}

