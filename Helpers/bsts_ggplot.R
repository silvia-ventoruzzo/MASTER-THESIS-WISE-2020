#' @title Various functions for ggplot plots for BSTS models
#' 
#' @description 
#' The function `gg_components` implements a ggplot2 version of the function `PlotBstsComponents` from the package `bsts`.
#' The function `gg_state` implements a ggplot2 version of the function `PlotBstsState` from the package `bsts`.
#' The function `gg_coefficients` implements a ggplot2 version of the function `PlotBstsCoefficients` from the package `bsts`.
#' The function `gg_forecast` implements a ggplot2 version of the function `plot.bsts.prediction` from the package `bsts`.
#' 
#' @param bsts_model bsts object
#' @param burn integer. The number of initial MCMC iterations to remove from each model as burn-in (default: NULL).
#' @param time date. Vector of time points. If not provided a vector of integers will be given (default: NULL).
#' @param sum_same logical. In the function `gg_components`, whether components with same name (e.g. all regressors) should be summed or not (default: FALSE).
#' @param same_scale logical. In the function `gg_components`, whether the plots should have the same y-axis scale (default: FALSE).
#' @param point_estimate character. Point estimate to display, one of mean, median or mode (default: median).
#' @param quantile_step numeric. For how far of a step the quantiles should be displayed (default: 0.1).
#' @param inclusion_lower_bound numeric. In the function `gg_coefficients`, plot regressors with marginal inclusion probabilities above this threshold (default: 0).
#' @param max_variables_nr integer. Maximal number of variables to display. If NULL all variables will be displayed (default: NULL).
#' @param interval logical. In the function `gg_forecast`, whether to display confidence interval (default: TRUE).

if(!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if(!require(bsts)) install.packages("bsts"); library(bsts)
if(!require(tidybayes)) install.packages("tidybayes"); library(tidybayes)

gg_components <- function(bsts_model,
                          burn = NULL,
                          time = NULL,
                          sum_same = FALSE,
                          same_scale = FALSE,
                          point_estimate = "median",
                          quantile_step = 0.1) {
  
  # If no time provided, use timestamps from BSTS object
  if (is.null(time)) {
    time <- bsts_model$timestamp.info$regular.timestamps
  }
  
  # If no burn set, take suggested burn calculated with function SuggestBurn
  if (is.null(burn)) {
    burn <- bsts::SuggestBurn(proportion = 0.1, bsts.object = bsts_model)
  }
  
  # Since the mode function has a upper-case initial letter, this value is renamed in that case
  # Morevoer, this parameter can only take one of three values: mode, mean or median
  if (point_estimate == "mode") {
    point_estimate <- "Mode"
  } else if (!(point_estimate) %in% c("median", "mean")) {
    stop("Point estimate can either be median, mean or mode.")
  }
  
  # The number of MCMC iterations to discard as burn-in.
  if (burn == 0) {
    state_contributions <- bsts_model$state.contributions[, , ]
  } else {
    state_contributions <- bsts_model$state.contributions[-(1:burn), ,]
  }

  # Names of state components
  state_components_names <- dimnames(state_contributions)$component
  
  # Number of state components
  nr_components <- length(state_components_names)
  
  # Check that state components names are unite
  if (nr_components > length(unique(state_components_names))) {
    # Extract unique names of states which appear more than once
    double_names <- unique(Filter(function(elem) length(which(state_components_names == elem)) > 1, state_components_names))
    # Add to each an identifier number
    double_names_new <- lapply(double_names,
                               function(x) {
                                 nr <- table(state_components_names)[[x]]
                                 new_names <- paste0(x, 1:nr)
                                 }) %>% unlist()
    # Combine with the unique names of states that appear only once
    state_components_names <- c(setdiff(state_components_names, double_names), double_names_new)
  }
  
  
  # Quantile probabilities
  quantile_probs <- seq(0, 1, quantile_step)
  quantile_probs <- quantile_probs[quantile_probs > 0]
  
  # Transform state contributions MCMC into data frame
  state_contributions <- lapply(1:dim(state_contributions)[1],
                                function(i) {
                                  state <- state_contributions[i,,] %>%
                                    t() %>%
                                    as.data.frame() %>%
                                    set_names(nm = state_components_names) %>%
                                    mutate(time = time,
                                           mcmc = burn+i)
                                  # If the sum of the components with the same name should be displayed, sum by state with same name
                                  if (sum_same & exists("double_names")) {
                                    for (name in double_names) {
                                      state <- state %>%
                                        mutate(!!sym(name) := rowSums(dplyr::select(., starts_with(name)))) %>%
                                        dplyr::select(-one_of(double_names_new))
                                    }
                                  }
                                  return(state)
                                }) %>%
    bind_rows()
  
  # If the sum of the components with the same name should be displayed, adapt vector of state names
  if (sum_same & exists("double_names")) {
    state_components_names <- c(setdiff(state_components_names, double_names_new), double_names)
  }

  # Calculate point estimate and quantiles
  state_contributions_quantiles <- state_contributions %>%
    pivot_longer(cols = all_of(state_components_names)) %>%
    group_by(name, time) %>%
    point_interval(value,
                   .width = quantile_probs,
                   .point = match.fun(point_estimate),
                   .interval = qi) %>%
    mutate(.point = tolower(point_estimate)) %>%
    ungroup()
  
  # Plot
  state_contributions_quantiles %>%
    mutate(name = factor(name, levels = state_components_names)) %>%
    ggplot(aes(x = time, y = value)) +
    geom_ribbon(aes(ymin = .lower, ymax = .upper, 
                    fill = fct_rev(as.factor(.width)))) +
    geom_line(color = "black") +
    scale_fill_grey(start = 0.9, end = 0) +
    facet_wrap(~name, scales = ifelse(same_scale, "fixed", "free_y"), ncol = 1, strip.position = "right") +
    labs(title = "Time series of state components") +
    theme(legend.position = "none")
}

gg_state <- function(bsts_model,
                     burn = NULL,
                     time = NULL,
                     point_estimate = "median") {
  
  # If no time provided, use timestamps from BSTS object
  if (is.null(time)) {
    time <- bsts_model$timestamp.info$regular.timestamps
  }
  
  # If no burn set, take suggested burn calculated with function SuggestBurn
  if (is.null(burn)) {
    burn <- bsts::SuggestBurn(proportion = 0.1, bsts.object = bsts_model)
  }
  
  # Since the mode function has a upper-case initial letter, this value is renamed in that case
  # Morevoer, this parameter can only take one of three values: mode, mean or median
  if (point_estimate == "mode") {
    point_estimate <- "Mode"
  } else if (!(point_estimate) %in% c("median", "meand")) {
    stop("Point estimate can either be median, mean or mode.")
  }
  
  # The number of MCMC iterations to discard as burn-in.
  if (burn == 0) {
    state_contributions <- bsts_model$state.contributions
  } else {
    state_contributions <- bsts_model$state.contributions[-(1:burn), ,]
  }
  
  # Names of state components
  state_components_names <- dimnames(state_contributions)$component
  
  # If some names are duplicates, rename them
  if (length(duplicated(state_components_names)) >= 1) {
    for (x in state_components_names[duplicated(state_components_names)])  {
      state_components_names <- replace(x = state_components_names, 
                                        list = which(state_components_names == x),
                                        values = paste0(x, 1:sum(state_components_names == x)))
    }
  }
  
  # Transform state contributions MCMC into data frame
  state_contributions <- lapply(1:dim(state_contributions)[1],
                                function(i) {
                                  state_contributions[i,,] %>%
                                    t() %>%
                                    as.data.frame() %>%
                                    set_names(nm = state_components_names) %>%
                                    mutate(time = time,
                                           mcmc = burn+i)
                                }) %>%
    bind_rows()
  
  # Sum over all state components and plot
  state_contributions %>%
    mutate(state = rowSums(.[which(colnames(state_contributions) %in% state_components_names)])) %>%
    group_by(time) %>%
    summarize(state = mean(state)) %>%
    ungroup() %>%
    mutate(original = bsts_model$original.series) %>%
    ggplot(aes(x = time)) +
    geom_line(aes(y = state, color = "black")) +
    geom_point(aes(y = original, color = "blue"), shape = 1) +
    scale_color_manual(name = "series", values = c("black" = "black", "blue" = "blue"), labels = c("state", "original")) +
    labs(y = "value",
         title = "Time series of state") +
    theme(legend.position = "bottom")
}

gg_coefficients <- function(bsts_model,
                            burn = NULL,
                            inclusion_lower_bound = 0,
                            max_variables_nr = NULL) {
  
  # If no time provided, use timestamps from BSTS object
  if (is.null(time)) {
    time <- bsts_model$timestamp.info$regular.timestamps
  }

  # If no burn set, take suggested burn calculated with function SuggestBurn
  if (is.null(burn)) {
    burn <- bsts::SuggestBurn(proportion = 0.1, bsts.object = bsts_model)
  }
  
  # The number of MCMC iterations to discard as burn-in.
  if (burn == 0) {
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
    mutate(variable = factor(variable, levels = names(inclusion_prob[order(inclusion_prob)]))) %>%
    arrange(desc( `inclusion probability`)) %>%
    filter( `inclusion probability` >= inclusion_lower_bound) %>%
    slice(1:max_variables_nr) %>%
    ggplot(aes(x = variable, y =  `inclusion probability`)) +
    geom_col(aes(fill = `positive probability`), color = "black") +
    coord_flip(ylim = c(0, 1)) +
    scale_fill_gradientn(colours = grey.colors(n = 10, start = 0.9, end = 0),
                         limits = c(0, 1)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2),
                       minor_breaks = seq(0, 1, 0.1)) +
    labs(fill = "Probability\nof positive\ncoefficient",
         title = "Regressors' inclusion probabilities")
}
  
gg_forecast <- function(bsts_prediction,
                        time = NULL,
                        point_estimate = "median",
                        quantile_step = 0.1,
                        interval = TRUE) {
  
  # Original data used to produce the model
  original_data <- as.data.frame(bsts_prediction$original.series) %>%
    set_names(nm = "y") %>%
    rownames_to_column(var = "time") %>%
    mutate(time = ifelse(!is.na(as.Date(as.character(time), tz = "UTC", format = "%Y-%m-%d")), 
                         as.Date(time), 
                         as.numeric(time)),
           type = "original")
  
  if (!is.null(time)) {
    original_data$time <- time
  }
  
  # Construct time interval of forecast
  if (is.numeric(original_data$time)) {
    forecast_interval <- seq(from = max(original_data$time) + 1,
                             to = max(original_data$time) + ncol(bsts_prediction$interval),
                             by = 1)
  } else if (is.Date(original_data$time)) {
    forecast_interval <- seq.Date(from = max(original_data$time) + 1,
                                  # to = max(original_data$time) + nrow(.),
                                  by = paste(as.numeric(diff(tail(original_data$time, 2))),
                                             "days"),
                                  length.out = ncol(bsts_prediction$interval))
  } else {
    stop("Check why time is neither a number nor can it be transformed to a Date.")
  }
  
  prediction_data <- as.data.frame(t(bsts_prediction$interval)) %>%
    set_names(nm = c("int_lower", "int_upper")) %>%
    mutate(y = bsts_prediction[[point_estimate]],
           time = forecast_interval,
           type = "prediction")
  
  # Quantile probabilities
  quantile_probs <- seq(0, 1, quantile_step)
  quantile_probs <- quantile_probs[quantile_probs > 0]
  
  prediction_quantiles <- as.data.frame(t(bsts_prediction$distribution)) %>%
    mutate(time = prediction_data$time) %>%
    pivot_longer(cols = starts_with("V")) %>%
    group_by(time) %>%
    median_qi(x = value,
              .width = quantile_probs) %>%
    ungroup() %>%
    dplyr::select(time, .lower, .upper, .width)

  # Plot
  prediction_plot <- bind_rows(original_data, prediction_data) %>%
    left_join(prediction_quantiles, by = "time") %>%
    ggplot(aes(x = time, y = y)) +
    theme(legend.position = "none") +
    labs(y = "value",
         title = "Time series forecast")
  
  if (interval) {
    prediction_plot <- prediction_plot +
      geom_ribbon(aes(ymin = .lower, ymax = .upper,
                      fill = fct_rev(as.factor(.width)))) +
      geom_line(aes(color = type)) +
      geom_line(aes(y = int_lower), linetype = "dashed", color = "green") +
      geom_line(aes(y = int_upper), linetype = "dashed", color = "green") +
      scale_color_manual(values = c("original" = "black", "prediction" = "blue")) +
      scale_fill_grey(start = 0.9, end = 0)
  } else {
    prediction_plot <- prediction_plot +
      geom_line(aes(color = type)) +
      scale_color_manual(values = c("original" = "black", "prediction" = "blue"))
  }
  
  # Suppress warnings when printing plot
  return(suppressWarnings(prediction_plot))
}
