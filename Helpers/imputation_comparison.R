#' @title Comparison of different missing data imputation runs.
#' 
#' @description 
#' The function `imputation_comparison` produces statistics and plots to compare different missing data imputation runs.
#' 
#' @param stats1 list. List of errors from missing data imputation.
#' @param stats2 list. List of errors from missing data imputation.
#' @param title character. Title for plot.
#' @param subtitle character. Subtitile for plot.

if(!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)

imputation_comparison <- function(stats1, stats2,
                                  title = "Comparison of error distribution for Amelia with and without transformations",
                                  subtitle = "Bootstraps") {
  
  # Dataframe with all the values
  error_values <- c(stats1, stats2) %>%
    bind_rows()
  # Dataframe with confidence intervals
  source("helpers/confidence_interval.R")
  error_ci <- error_values %>%
    group_by(stat, type) %>%
    summarize(ci = list(confidence_interval(x = Amelia, level = 0.95, type = "se")),
              mean = mean(Amelia, na.rm = TRUE)) %>%
    ungroup() %>%
    unnest(cols = ci)
  
  # Histogram
  plot <- error_values %>%
    ggplot() +
    geom_histogram(aes(x = Amelia, fill = type, color = type), 
                   bins = 50, alpha = 0.8, position = "dodge") +
    geom_vline(data = error_ci,
               aes(xintercept = mean, color = type)) +
    geom_vline(data = error_ci,
               aes(xintercept = ci_lower_2.5, color = type), linetype="dashed") +
    geom_vline(data = error_ci,
               aes(xintercept = ci_upper_97.5, color = type), linetype="dashed") +
    facet_wrap(~stat, scales = "free_x") +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    labs(title = title,
         subtitle = subtitle,
         x = "value")
  
  # t-test
  stats <- list()
  for (statistic in unique(error_values$stat)) {
    for (i in unique(error_values$type)) {
      errors1 = error_values %>% 
        filter(stat == statistic,
               type == i) %>%
        pull(Amelia)
      errors2 = error_values %>% 
        filter(stat == statistic,
               type == i) %>%
        pull(Amelia)
    }
    stats[[statistic]] <- t.test(x = errors1, y = errors2, alternative = "two.sided")
  }
  return(list(confidence_intervals = error_ci,
              comparison_plot = plot,
              tstat = stats))
}