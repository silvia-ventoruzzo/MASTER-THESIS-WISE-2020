#' @title Plots to perform remainder diagnostics
#' 
#' @description The function `gg_remainder_diagnostics` produces various plots to analyze a prediction remainder.
#' 
#' @param remainder numeric. Vector of remainders.
#' @param lag_max numeric. Maximum lag for correlation plots (default: 30).
#' @param ggtheme ggplot2 theme (default: theme_bw()).
#' @param main character. Title for final plot (default: Remainder diagnostic plots).
#' @param plot logical. Whether to display plot or not (default: TRUE).

# Needed packages
needed_packages <- c("dplyr", "ggplot2", "tsibble", "feasts", "ggpubr")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE)) {install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages", "package")

gg_remainder_diagnostics <- function(remainder, lag_max = 30, 
                                     ggtheme = ggplot2::theme_bw(), 
                                     main = "Remainder diagnostic plots",
                                     plot = TRUE, ...) {
  
  # Creation of data frame for plotting
  data <- data.frame(time = 1:length(remainder),
                     remainder = remainder,
                     stringsAsFactors = FALSE) %>%
    tsibble::as_tsibble(index = time)
  
  # Time series
  p1 <- data %>%
    ggplot2::ggplot(aes(x = time, y = remainder)) +
    ggplot2::geom_line(size = 0.3) +
    ggplot2::geom_point(size = 0.3) +
    ggtheme +
    ggplot2::theme(plot.title = element_text(hjust = 0.5))
  
  # Autocorrelation
  p2a <- data %>%
    feasts::ACF(remainder, lag_max = lag_max) %>% 
    autoplot() +
    scale_x_continuous(breaks = seq(0, lag_max, 5)) +
    labs(x = "lag",
         y = "ACF") +
    ggtheme +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Partial autocorrelation
  p2b <- data %>%
    feasts::PACF(remainder, lag_max = lag_max) %>% 
    autoplot() +
    scale_x_continuous(breaks = seq(0, lag_max, 5)) +
    labs(x = "lag",
         y = "PACF") +
    ggtheme +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Histogram
  remainder_mean <- mean(data$remainder, na.rm = TRUE)
  remainder_sd <- sd(data$remainder, na.rm = TRUE)
  remainder_min <- min(min(data$remainder, na.rm = TRUE), remainder_mean - 3*remainder_sd)
  remainder_max <- max(max(data$remainder, na.rm = TRUE), remainder_mean + 3*remainder_sd)
  bins <- min(500, grDevices::nclass.FD(na.exclude(data$remainder)))
  binwidth <- (remainder_max - remainder_min)/bins
  data_dnorm <- data.frame(x = seq(remainder_min, remainder_max, length.out = bins)) %>%
    dplyr::mutate(y = length(data$remainder) * binwidth * stats::dnorm(x    = x, 
                                                                       mean = remainder_mean, 
                                                                       sd   = remainder_sd))
  p3a <- data %>%
    ggplot2::ggplot(aes(x = remainder)) +
    ggplot2::geom_histogram(bins = bins, alpha = 0.8) +
    ggplot2::geom_rug() +
    ggplot2::geom_line(data = data_dnorm, aes(x = x, y = y), color = "darkorange") +
    ggtheme +
    ggplot2::theme(plot.title = element_text(hjust = 0.5))
  
  # QQ plot
  p3b <- data %>%
    ggplot2::ggplot(aes(sample = remainder)) +
    ggplot2::geom_qq_line(color = "red") +
    ggplot2::geom_qq(size = 0.8) +
    ggplot2::labs(x = "theoretical",
                  y = "remainder") +
    ggtheme +
    ggplot2::theme(plot.title = element_text(hjust = 0.5))
  
  # Arrange plots
  p <- ggpubr::ggarrange(p1,
                         ggpubr::ggarrange(p2a, p2b, ncol = 2, nrow = 1),
                         ggpubr::ggarrange(p3a, p3b, ncol = 2, nrow = 1),
                         ncol = 1, nrow = 3)
  p <- ggpubr::annotate_figure(p = p,
                               top = text_grob(main, size = 14))
  
  # Either print plot of return list of plots
  if (plot) {
    return(p)
  } else {
    return(list(ts = p1,
                acf = p2a,
                pacf = p2b,
                hist = p3a,
                qqplot = p3b))
  }
  
}
