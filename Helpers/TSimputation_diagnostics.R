#' @title Visualize Imputed Values
#'
#' @description Visualize the imputed values in a time series.
#'
#' @param df_missing data frame. Data frame with missing values.
#' @param df_imputed data frame. Data frame where missing values have been imputed.
#' @param df_complete data frame. Data frame without missing values.
#' @param ts_variable character. Name of variable with time points.
#' @param cs_variable character. Name of cross-sectional variable, if present.
#' @param relevant_variables character. Name(s) of variable(s) with missing values.
#' @param xlab character. Name to give to plot x-axis.
#' @param col_real character. Color for actual non-missing values (default: green3).
#' @param col_imputed character. Color for imputed values (default: indianred2)
#' @param col_known character. Color for known values at time points that are imputed (default: steelblue2).
#' @param shape integer. Shape of points (default: 20).
#' @param size integer. Size of points (default: 1).
#' @param ggplot_theme ggplot2 theme. Default: theme_bw().

if(!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
if(!require(Metrics)) install.packages("Metrics"); require(Metrics)

TSimputation_diagnostics <- function(df_missing, df_imputed, df_complete = NULL,
                                     ts_variable, cs_variable, relevant_variables = NULL,
                                     xlab = "time",
                                     col_real = "green3", col_imputed = "indianred2", col_known = "steelblue2",
                                     shape = 20, size = 1, ggplot_theme = ggplot2::theme_bw()) {

  ###########################
  ## Checks #################
  ###########################
  # Check that dimensions are identical
  if (!identical(dim(df_missing), dim(df_imputed)))
    stop("The dimensions of the original and imputed dataframes are not equal.")
  
  # Check that there is at least one numeric variable
  if (sum(sapply(df_imputed, is.numeric)) < 1)
    stop("No numeric variables present in the dataframes.")
  
  ############################
  ## Dataframe for plotting ##
  ############################
  # Extracting all numeric variables
  numeric_variables <- colnames(df_imputed)[sapply(df_imputed, is.numeric)]
  if (is.null(relevant_variables)) {
    variables <- colnames(df_missing)
    missing_variables <- sapply(df_missing,
                                function(x) {is.numeric(x) & (sum(is.na(x)) > 0)})
    relevant_variables <- variables[missing_variables]
  }
  # Extract missing values for all variables separately
  data_all_var <- list()
  for (i in relevant_variables) {
    index_missing <- df_missing %>% 
      mutate(row_n = row_number()) %>%
      filter_at(vars(i), is.na) %>%
      pull(row_n)
    data_known <- df_missing %>%
      slice(-index_missing) %>%
      mutate(type = "known values") %>%
      pivot_longer(cols = i, names_to = "variable", values_to = "value")
    data_imputed <- df_imputed %>%
      slice(index_missing) %>%
      mutate(type = "imputed values") %>%
      pivot_longer(cols = i, names_to = "variable", values_to = "value")
    # If a grouping variable is present, one variable more to keep in dataframe
    if (is.null(cs_variable)) {
      data_known <- data_known %>% select(!!ts_variable, type, variable, value)
      data_imputed <- data_imputed %>% select(!!ts_variable, type, variable, value)  
    } else {
      data_known <- data_known %>% select(!!cs_variable, !!ts_variable, type, variable, value)
      data_imputed <- data_imputed %>% select(!!cs_variable, !!ts_variable, type, variable, value)
    }
    data_all_var[[i]] <- bind_rows(data_known, data_imputed)
    
    # If also dataframe with complete information is present
    if (!is.null(df_complete)) {
      data_real <- df_complete %>%
        slice(index_missing) %>%
        mutate(type = "real values") %>%
        pivot_longer(cols = i, names_to = "variable", values_to = "value")
      
      #####################################
      ## Error metrics ####################
      #####################################
      actual    <- df_complete %>% slice(index_missing) %>% pull(!!i)
      predicted <- df_imputed %>% slice(index_missing) %>% pull(!!i)
      mae <- Metrics::mae(actual    = actual,
                          predicted = predicted)
      mse <- Metrics::mse(actual    = actual,
                          predicted = predicted)
      mape <- Metrics::mae(actual    = actual,
                           predicted = predicted)
      rmse <- Metrics::rmse(actual    = actual,
                            predicted = predicted)
      if (is.null(cs_variable)) {
        data_real <- data_real %>% select(!!ts_variable, type, variable, value)
      } else {
        data_real <- data_real %>% select(!!cs_variable, !!ts_variable, type, variable, value)
      }
      data_all_var[[i]] <- bind_rows(data_all_var[[i]], data_real)
    }
  }
  data_all <- bind_rows(data_all_var)
  
  
  #####################################
  ## Time series plot #################
  #####################################
  ts_plot <- function(df, relevant_variables, ts_variable,
                      shape, size, xlab, main = "Time series plot",
                      col_real, col_imputed, col_known) {
    
    p <- df %>%
      ggplot(aes(x = !!sym(ts_variable), y = value, col = type)) +
      geom_point(shape = shape, size = size) +
      labs(title = main,
           x     = xlab,
           y     = relevant_variables) +
      scale_color_manual(values = c("real values" = col_real, "imputed values" = col_imputed, "known values" = col_known)) +
      ggplot_theme +
      theme(plot.title      = element_text(hjust = 0.5),
            legend.title    = element_blank(),
            legend.position = "bottom",
            axis.text.x     = element_text(angle = 30, hjust = 1))
    
    if (length(relevant_variables) > 1) {
      p <- p +
        theme(axis.title.y = element_blank()) +
        facet_wrap(~variable, nrow = length(relevant_variables), scales = "free_y")
    }
    
    return(p)
  }
  
  #####################################
  ## Density comparison plot ##########
  #####################################
  dens_plot <- function(df, df_missing, relevant_variables, ts_variable,
                           shape, size, xlab, main = "Comparison of densities",
                           col_real, col_imputed, col_known) {
  # Percentage missing values
  missing_percentage <- df_missing %>% 
    summarize_if(is.numeric, function(x) paste0(round(sum(is.na(x))/length(x)*100, 2), "%")) %>%
    pivot_longer(cols = one_of(relevant_variables), names_to = "variable", values_to = "missing")
  # Plot
  p <- df %>%
    group_by(type, variable) %>%
    filter(n() >= 2) %>%
    mutate_at(vars(value), list(density.x = function(x) density(x, n = length(x))$x,
                                density.y = function(x) density(x, n = length(x))$y)) %>%
    ggplot(aes(x = density.x, y = density.y)) +
    geom_line(aes(col = type), alpha = 0.5) +
    geom_text(data    = missing_percentage,
              mapping = aes(x = +Inf, y = +Inf, label = paste0("Fraction missing: ", missing)),
              hjust   = +1.1,
              vjust   = +2) +
    labs(title = main,
         x     = relevant_variables,
         y     = "relative density") +
    scale_color_manual(values = c("real values" = col_real, "imputed values" = col_imputed, "known values" = col_known)) +
    ggplot_theme +
    theme(plot.title      = element_text(hjust = 0.5),
          legend.title    = element_blank(),
          legend.position = "bottom",
          axis.text.x     = element_text(angle = 30, hjust = 1))
  
    if (length(relevant_variables) > 1) {
      p <- p +
        theme(axis.title.x = element_blank()) +
        facet_wrap(~variable, nrow = length(relevant_variables), scales = "free")
    }
    
    return(p)
  }
  
  #############################################
  ## Creation of plots ########################
  #############################################
  if (is.null(cs_variable)) {
    timeseries_plot <- ts_plot(df = data_all,
                               relevant_variables = relevant_variables, ts_variable = ts_variable,
                               shape = shape, size = size, xlab = xlab,
                               col_real = col_real, col_imputed = col_imputed, col_known = col_known)
    density_plot <- dens_plot(df = data_all, df_missing = df_missing,
                            relevant_variables = relevant_variables, ts_variable = ts_variable,
                            shape = shape, size = size, xlab = xlab,
                            col_real = col_real, col_imputed = col_imputed, col_known = col_known)
  } else {
    # Initialize list of plots
    timeseries_plot <- list()
    density_plot <- list()
    for (id in unique(data_all[[cs_variable]])) {
      filtered_data <- data_all %>%
        filter(!!sym(cs_variable) == id)
      filtered_missing <- df_missing %>%
        filter(!!sym(cs_variable) == id)
      # Time series plot
      timeseries_plot[[id]] <- ts_plot(df = filtered_data,
                                       relevant_variables = relevant_variables, ts_variable = ts_variable,
                                       shape = shape, size = size, xlab = xlab,
                                       col_real = col_real, col_imputed = col_imputed, col_known = col_known) +
        labs(subtitle = paste(cs_variable, "=", id)) +
        theme(plot.subtitle = element_text(hjust = 0.5))
      density_plot[[id]] <- dens_plot(df = filtered_data, df_missing = filtered_missing,
                                         relevant_variables = relevant_variables, ts_variable = ts_variable,
                                         shape = shape, size = size, main = "Time series plot", xlab = xlab,
                                         col_real = col_real, col_imputed = col_imputed, col_known = col_known) +
        labs(subtitle = paste(cs_variable, "=", id)) +
        theme(plot.subtitle = element_text(hjust = 0.5))
    }
  }
  
  # Final list
  plot_list <- list(timeseries_plot  = timeseries_plot,
                    density_plot     = density_plot,
                    error_statistics = ifelse(exists("rmse"), list(mae = mae, mse = mse, mape = mape, rmse = rmse), 
                                              "Provide dataframe with complete data to calculate error statistics."))
  return(plot_list)
}