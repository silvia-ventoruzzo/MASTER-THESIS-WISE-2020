#' @title Plots for comparing BSTS models
#' 
#' @description The function `gg_comparebstsmodels` implements a ggplot2 version of the function `CompareBstsModels` from the package `bsts`.
#' 
#' @param model_list list. List of bsts objects.
#' @param burn integer. The number of initial MCMC iterations to remove from each model as burn-in (default: NULL).
#' @param time date. Vector of time points. If not provided a vector of integers will be given (default: NULL).
#' @param cutpoint integer. Observation number used to define a holdout sample (default: NULL).
#' @param plot logical. Whether to show the plot or not (default: TRUE).
 
if(!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if(!require(ggpubr)) install.packages("ggpubr"); library(ggpubr)
if(!require(bsts)) install.packages("bsts"); library(bsts)

gg_comparebstsmodels <- function(model_list,
                                 burn = NULL,
                                 time = NULL,
                                 cutpoint = NULL,
                                 plot = TRUE) {
  
  # Number of time points, it needs to be equal in all models (and to time parameter if provided)
  time_dimension <- sapply(model_list, function(m) {dim(m$state.contributions)[3]})
  if (any(time_dimension != time_dimension[1])) {
    stop("All models need to have the same time dimension.")
  }
  
  # If no time provided, use timestamps from BSTS object
  if (is.null(time)) {
    time <- model_list[[1]]$timestamp.info$regular.timestamps
  } else if (length(time) != time_dimension[1]) {
    stop("The provided time needs to have the same dimension as the models.")
  }
  
  # If no burn-in provided, calculate suggested burn-in
  if (is.null(burn)) {
    burn <- SuggestBurn(.1, model_list[[1]])
  }
  
  # If no model names provided in the model list, assign them an increasing number
  if (is.null(names(model_list))) {
    model_names <- paste("Model", 1:length(model_list))
  } else {
    model_names <- names(model_list)
  }
  
  # Calculate cumulative absolute prediction errors
  if (is.null(cutpoint)) {
    cumulative_errors <- lapply(model_list,
                                function(x) {
                                  errors <- bsts.prediction.errors(bsts.object = x, 
                                                                   burn = burn)$in.sample
                                  return(cumsum(abs(colMeans(errors))))
                                })
  } else {
    if (cutpoint > time_dimension[1]) stop("Cutpoint is bigger than time dimension. Please adjust value accordingly.")
    cumulative_errors <- lapply(model_list,
                                function(x) {
                                  errors <- bsts.prediction.errors(bsts.object = x, 
                                                                   burn = burn, 
                                                                   cutpoints = cutpoint)[[1]]
                                  return(cumsum(abs(colMeans(errors))))
                                })
  }
  
  # Plot of cumulative errors
  p_errors <- cumulative_errors %>%
    bind_cols() %>%
    mutate(time = time) %>%
    pivot_longer(cols = -time, names_to = "model") %>%
    ggplot(aes(x = time, y = value, color = model)) +
    geom_line() +
    labs(y = "cumulative absolute error") +
    theme(legend.position = c(0.1, 0.8),
          legend.title = element_blank())
  
  # Plot of original values
  p_series <- as.data.frame(model_list[[1]]$original.series) %>%
    set_names(nm = "original") %>%
    mutate(time = time) %>%
    ggplot(aes(x = time, y = original)) +
    geom_line() +
    labs(y = "original values")
  
  # Print the plots or return a list of plots
  if (plot) {
    p_errors <- p_errors +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank())
    p <- ggpubr::ggarrange(p_errors, p_series, ncol = 1)
    p <- ggpubr::annotate_figure(p = p,
                                 top = "Comparison of bsts models")
    p
    
  } else {
    return(list(cum_errors = p_errors,
                original_series = p_series))
  }
}