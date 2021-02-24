#' @title Time Series correlation plot
#' 
#' @description The function `ts_corr_plot` displays Autocorrelation and Partial Autocorrelation of a univariate time series.
#' 
#' @inheritParams acf
#' @param ci numeric. Confidence interval (defaul: 0.95).
#' @param horizontal logical. Whether to display plots horizontally or vertically (default: TRUE).
#' @param ggtheme ggplot2 theme (default: theme_bw())

if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)

ts_corr_plot <- function(x, lag.max = NULL, ci = 0.95, horizontal = TRUE, ggtheme = ggplot2::theme_bw()) {
  
  if (horizontal == TRUE) {n_row <- 1} else {n_row <- 2}
  
  ############################
  # Autocorrelation ##########
  ############################
  acf_result <- acf(x, lag.max = lag.max, type = "correlation", plot = FALSE)

  ############################
  # Partial autocorrelation ##
  ############################
  pacf_result <- acf(x, lag.max = lag.max, type = "partial", plot = FALSE)

  ############################
  # Calculations #############
  ############################
  # Number of time points
  N <- as.numeric(acf_result$n.used)
  # Create dataframe
  if (identical(acf_result$lag, pacf_result$lag)) {
    df <- data.frame(lag  = acf_result$lag, 
                     acf  = acf_result$acf,
                     pacf = pacf_result$acf,
                     stringsAsFactors = FALSE)
  } else {
    acf_df <- data.frame(lag = acf_result$lag, ACF = acf_result$acf,
                         stringsAsFactors = FALSE)
    pacf_df <- data.frame(lag = pacf_result$lag, PACF = pacf_result$acf,
                          stringsAsFactors = FALSE)
    df <- full_join(acf_df, pacf_df, by = "lag")
  }

  ############################
  # Plot #####################
  ############################
  p <- df %>%
    pivot_longer(cols = c("ACF", "PACF")) %>%
    ggplot(aes(x = lag, y = value)) +
    geom_col(alpha = 0.7, width = 0.7) +
    geom_hline(yintercept = qnorm((1+ci)/2)/sqrt(N), 
               colour = "blue",
               linetype = "dashed") + 
    geom_hline(yintercept = - qnorm((1+ci)/2)/sqrt(N), 
               colour = "blue",
               linetype = "dashed") +
    scale_x_continuous(breaks = seq(0, max(df$lag), 5)) +
    scale_y_continuous(limits = c(min(df$ACF, df$PACF), 1)) +
    facet_wrap(~name, nrow = n_row) +
    labs(title = "Autocorrelation and Partial Autocorrelation") +
    ggtheme +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank())
  
  return(p)
}