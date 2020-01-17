#' Set project theme
#'
#'

set_proj_theme <- function(base_size = 14){
  ggptt::set_ptt(base_size, "sans")
  ggplot2::theme_update(
    legend.text = ggplot2::element_text(size = base_size),
    plot.subtitle = ggplot2::element_text(colour = "grey40"),
    plot.caption = ggplot2::element_text(size = base_size-4, face = "plain", colour = "grey40"))
}
