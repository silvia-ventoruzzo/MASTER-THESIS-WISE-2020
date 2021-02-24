#' @title Seasonal diagnostics plots for STL decomposition
#' 
#' @description The function `gg_seasonal_diagnostic_plot` implements a ggplot2 version of the function `plot_seasonal` from the package `stlplus`.
#' 
#' @param x stlplus object
#' @param point_col character. Colors to give to points (default: grey).
#' @param line_col character. Colors to give to lines (default: blue).
#' @param xlab character. Title of x-axis (default: time).
#' @param ylab character. Title of y-axis (default: centered seasonal + remainder).
#' @param title character. Plot title (default: Seasonal diagnostic plot).
#' @param ggtheme ggplot2 theme (default: theme_bw()).

# Needed packages
needed_packages <- c("dplyr", "ggplot2", "stlplus")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE)) {install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages", "package")

gg_seasonal_diagnostic_plot <- function(x, point_col = "grey", line_col = "blue",
                                        xlab = "time", ylab = "centered seasonal + remainder",
                                        title = "Seasonal diagnostic plot",
                                        ggtheme = ggplot2::theme_bw()) {
  
  data <- x$data %>%
    dplyr::group_by(sub.labels) %>%
    dplyr::mutate(seasonal_remainder = seasonal + remainder,
                  sub_mean = mean(seasonal_remainder),
                  a        = seasonal_remainder - sub_mean,
                  b        = seasonal - sub_mean) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(t = x$time)
  
  p <- data %>%
    ggplot2::ggplot(aes(x = t)) +
    ggplot2::geom_point(aes(y = a), shape = 1, col = point_col) +
    ggplot2::geom_line(aes(y = b), col = line_col) +
    # ggplot2::geom_smooth(method = "loess", line_col) +
    ggplot2::facet_wrap(~sub.labels) +
    ggplot2::labs(x      = xlab,
                  y      = ylab,
                  title = title) +
    ggtheme +
    ggplot2::theme(plot.title = element_text(hjust = 0.5))

  return(p)
}