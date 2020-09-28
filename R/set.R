#' Set project theme
#'
#'

set_proj_theme <- function(base_size = 12){
  ggptt::set_ptt(base_size, "sans")
  ggplot2::theme_update(
    legend.text = ggplot2::element_text(size = base_size),
    axis.title = ggplot2::element_text(colour = "grey20", size = base_size - 2),
    plot.subtitle = ggplot2::element_text(colour = "grey40"),
    plot.caption = ggplot2::element_text(size = base_size-4, face = "plain", colour = "grey40"),
    plot.margin = ggplot2::margin(t = 10, r = 2, b = 5, l = 5, unit = "pt"))
}


set_board_theme <- function(base_size = 12){
  ggptt::set_gg(theme_ptt(base_size, "sans"), tableau_color_pal(palette = "Color Blind"))
  ggplot2::theme_update(
    legend.text = ggplot2::element_text(size = base_size),
    axis.title = ggplot2::element_text(colour = "grey20", size = base_size - 2),
    plot.subtitle = ggplot2::element_text(colour = "grey40"),
    plot.caption = ggplot2::element_text(size = base_size-4, face = "plain", colour = "grey40"),
    plot.margin = ggplot2::margin(t = 10, r = 2, b = 5, l = 5, unit = "pt"))
}
