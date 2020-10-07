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
  cols <- c("#3b5998", "#FFCD00", "#000000", "#6E8B3D", "#6495ED", "#EE7600", "#8B3E2F", "#B22234", "#8968CD", "#7F7F7F")
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
  plot <- plot + theme(legend.text = element_text(size = 9))
  ggplot2::ggsave(filename = file.path("~/../Pellervon Taloustutkimus PTT ry/KT172 Kilpailukyvyn arviointi - General/Raportointi/Kuviot",
                                       paste0(filename, ".png")), plot = plot, width = width, height = height)
}


#'Save plots for twitter
#'
#' @param filename File name to create on disk without extension
#' @param plot Plot to save, defaults to last plot displayed.
#'
#'
#' @export
#'
ggsave_twitter <- function(filename, plot = last_plot()){
  plot2 = plot +
    ggplot2::theme(text = element_text(size = 14)) +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(colour = "grey40"),
      plot.caption = ggplot2::element_text(size = 10, face = "plain", colour = "grey40"),
      text = ggplot2::element_text(face = "plain"),
      plot.margin = ggplot2::margin(1, 1, 2, 1))
  ggplot2::ggsave(file.path(Sys.getenv("USERPROFILE"),"Downloads", paste0(filename, ".png")),
                  plot = plot2,
                  width = 16/2,
                  height = 9/2)
}
