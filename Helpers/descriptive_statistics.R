#' @title Descriptive statistics for numerical or categorical data.
#' 
#' @description
#' The functions `descr_numerical` and `descr_categorical` calculate descriptive statistics for numerical and categorical data, respectively.
#' @param df data.frame. 

if(!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)

descr_numerical = function(df) {
  
  # Extract all variable names
  all_variables <- colnames(df)
  # Select only numeric variables and extract their names
  df <- df %>%
    select_if(is.numeric)
  numeric_variables <- colnames(df)
  
  cat("The variables", paste(setdiff(all_variables, numeric_variables), collapse = ", "),
      "have been removed since they are not numerical.")
  
  # Calculate summary statistics
  summary = data.frame(
    variable = names(df),
    min      = apply(df, 2, min, na.rm = TRUE),
    `1Q`     = apply(df, 2, quantile, probs = 0.25, na.rm = TRUE),
    median   = apply(df, 2, median, na.rm= TRUE),
    `3Q`     = apply(df, 2, quantile, probs = 0.75, na.rm = TRUE),
    max      = apply(df, 2, max, na.rm = TRUE),
    IQR      = apply(df, 2, IQR, na.rm = TRUE),
    mean     = apply(df, 2, mean, na.rm = TRUE),
    sd       = apply(df, 2, sd, na.rm = TRUE),
    check.names = FALSE) %>%
    dplyr::mutate_if(is.numeric, function(x) round(x, 4))
  
  return(summary)  
}
    
descr_categorical <- function(df) {
  # Extract all variable names
  all_variables <- colnames(df)
  # Select only categorical variables and extract their names
  df <- df %>%
    select_if(function(x) is.character(x) | is.factor(x) | is.logical(x))
  categorical_variables <- colnames(df)
  
  if (length(setdiff(all_variables, categorical_variables)) > 0) {
    cat("The variables", paste(setdiff(all_variables, categorical_variables), collapse = ", "),
        "have been removed since they are not categorical.")
  }
  
  
  # Frequency
  frequency_list = apply(df, 2, function(x) table(x, useNA = "ifany"))
  
  if (length(categorical_variables) == 1) {
    summary <- as.data.frame(frequency_list) %>%
      tibble::rownames_to_column(var = "factor") %>%
      rename(frequency = !!categorical_variables) %>%
      mutate(variable = categorical_variables,
             sum = sum(frequency),
             proportion = round(frequency/sum, 4)*100,
             proportion = as.character(proportion) %>% paste("%")) %>%
      dplyr::select(variable, factor, frequency, proportion) %>%
      arrange(variable, desc(frequency))
      
  } else {
    summary <- data.frame(frequency = unlist(frequency_list)) %>%
      tibble::rownames_to_column(var = "var_fact") %>%
      separate(col = "var_fact", into = c("variable", "factor"), sep = "\\.", remove = TRUE) %>%
      group_by(variable) %>%
      mutate(sum = sum(frequency)) %>%
      ungroup() %>%
      # Proportion
      mutate(proportion = round(frequency/sum, 4)*100,
             proportion = as.character(proportion) %>% paste("%")) %>%
      dplyr::select(-sum) %>%
      arrange(variable, desc(frequency))
  }
  
  return(summary)
}
