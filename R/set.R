#' Competitiveness board theme
#'
theme_board <- function (base_size = 11, base_family = ""){
  ggptt::theme_ptt(base_size = base_size, base_family = base_family) +
    theme(
      legend.text = ggplot2::element_text(size = ggplot2::rel(1)),
          axis.title = ggplot2::element_text(colour = "grey20", size = ggplot2::rel(0.8)),
          plot.subtitle = ggplot2::element_text(colour = "grey40"),
          plot.caption = ggplot2::element_text(size = ggplot2::rel(0.7), face = "plain", colour = "grey40"),
          plot.margin = ggplot2::margin(t = 10, r = 10, b = 4, l = 5, unit = "pt"))
}


#' Set project theme
#'
#'

set_proj_theme <- function(base_size = 12){
  ggptt::set_ptt(base_size, "sans")
  ggplot2::theme_update(
    legend.text = ggplot2::element_text(size = ggplot2::rel(1.1)),
    axis.title = ggplot2::element_text(colour = "grey20", size = base_size - 2),
    plot.subtitle = ggplot2::element_text(colour = "grey40"),
    plot.caption = ggplot2::element_text(size = base_size-4, face = "plain", colour = "grey40"),
    plot.margin = ggplot2::margin(t = 10, r = 10, b = 4, l = 5, unit = "pt"))
}


set_board_theme <- function(base_size = 11){
  ggptt::set_gg(theme_board(base_size, "sans"), ggthemes::tableau_color_pal(palette = "Color Blind"))
}

