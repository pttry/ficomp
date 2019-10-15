#' Set project theme
#'
#'

set_proj_theme <- function(){
  ggptt::set_ptt(14, "sans")
  ggplot2::theme_update(
    legend.text = ggplot2::element_text(size = 14),
    plot.subtitle = ggplot2::element_text(colour = "grey40"),
    plot.caption = ggplot2::element_text(size = 10, face = "plain", colour = "grey40"))
}
