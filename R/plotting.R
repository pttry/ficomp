#' Color codes
#'
#' Last colour is grey75.
#'
#' @param n number of colours
#'
#' @export
#'
geo_col <- function(n){
  n <- n - 1
  cols <- c("#3b5998", "#FFCD00", "#000000", "#B22234")
  col_seg <- rep(1:length(cols), times = ceiling(n/length(cols)))[1:n]
  c(cols[col_seg], "grey75")
}


#' Save figures for report
#'
#' Uses ggsave.
#'
#' @param filename File name to create on disk.
#' @param plot Plot to save, defaults to last plot displayed.
#' @param width Plot size
#' @param height Plot size
#'
#' @export

save_fig <- function(filename,
                     plot = last_plot(),
                     width = 5.8,
                     height = 3.9){
  ggplot2::ggsave(filename = file.path("~/../Pellervon Taloustutkimus PTT ry/KT172 Kilpailukyvyn arviointi - General/Raportointi/Kuviot",
                                       paste0(filename, ".png")), plot = plot, width = width, height = height)
}
