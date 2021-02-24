#' @title Time series plot of most likely predictors.
#' 
#' @description The function `gg_predictors` implements a ggplot2 version of the function `PlotBstsPredictors` from the package `bsts`.
#' 
#' @param bsts_model bsts object
#' @param burn integer. The number of initial MCMC iterations to remove from each model as burn-in (default: NULL).
#' @param inclusion_lower_bound numeric. Plot predictors with marginal inclusion probabilities above this threshold (default: 0.10).
#' @param max_variables_nr integer. Maximal number of variables to display. If NULL all variables will be displayed (default: NULL).

if(!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if(!require(ggpubr)) install.packages("ggpubr"); library(ggpubr)
if(!require(bsts)) install.packages("bsts"); library(bsts)

gg_predictors <- function(bsts_model,
                          burn = bsts::SuggestBurn(proportion = 0.1, bsts.object = bsts_model),
                          inclusion_lower_bound = 0.10,
                          max_variables_nr = NULL) {
  
  # If no time provided, use timestamps from BSTS object
  if (is.null(time)) {
    time <- bsts_model$timestamp.info$regular.timestamps
  }

  # The number of MCMC iterations to discard as burn-in.
  if (is.null(burn) | burn == 0) {
    burn <- 0
    coefficients <- bsts_model$coefficients
  } else {
    coefficients <- bsts_model$coefficients[-(1:burn), ,]
  }
  
  # If no maximum number of variables is indicated, display all variables
  if (is.null(max_variables_nr)) {
    max_variables_nr <- dim(bsts_model$coefficients)[2]
  }

  # Calculate inclusion probability (mean of in how many iterations the coefficient is not zero)
  inclusion_prob <- colMeans(coefficients != 0)
  
  # Keep only regressors with inclusion probability over lower bound
  regressors_names <- names(inclusion_prob[inclusion_prob > inclusion_lower_bound])
  
  # Extract values of regressors and scale them to fit in the same plot
  predictors <- as.data.frame(bsts_model$predictors) %>%
    dplyr::select(!!regressors_names)
  original_series <- data.frame(original_series = bsts_model$original.series,
                                stringsAsFactors = FALSE)
  
  bind_cols(original_series, predictors) %>%
    mutate_all(scale) %>%
    mutate(time = time) %>%
    pivot_longer(cols = -starts_with("time"), names_to = "variable", values_to = "series") %>%
    ggplot(aes(x = time, y = series, color = variable)) +
    geom_line()
  
  # Calculate probability that each variable has a positive coefficient
  positive_prob <- apply(coefficients, 2, 
                         function(x) {
                           x <- x[x != 0]
                           if (length(x) == 0) {
                             return(0)
                           }
                           return(mean(x > 0))
                         })
  
  # Plot of variables ordered by descending inclusion probability
  bind_rows(inclusion_prob, positive_prob) %>%
    mutate(type = c("inclusion probability", "positive probability")) %>%
    pivot_longer(cols = -ends_with("type"), names_to = "variable", values_to = "prob") %>%
    pivot_wider(id_cols = "variable", names_from = "type", values_from = "prob") %>%
    # 
    # inclusion_prob %>%
    #   as.data.frame() %>%
    #   set_names(nm = "prob") %>%
    #   rownames_to_column(var = "variable") %>%
    mutate(variable = factor(variable, levels = names(inclusion_prob[order(inclusion_prob)]))) %>%
    arrange(desc( `inclusion probability`)) %>%
    filter( `inclusion probability` >= inclusion_lower_bound) %>%
    slice(1:max_variables_nr) %>%
    ggplot(aes(x = variable, y =  `inclusion probability`)) +
    geom_col(aes(fill = as.factor(`positive probability`)), color = "black") +
    coord_flip() +
    # geom_ribbon(aes(ymin = .lower, ymax = .upper, 
    #                 fill = fct_rev(as.factor(.width)))) +
    scale_fill_grey(start = 0, end = 1) +
    scale_y_continuous(breaks = seq(0, 1, 0.2),
                       minor_breaks = seq(0, 1, 0.1)) +
    labs(title = "Regressors' inclusion probabilities") +
    theme(legend.position = "none")
}