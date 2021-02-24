#' @title Bootstrap imputation of missing data for Multivariate Time Series
#' 
#' @description 
#' The function `MTSbootstrapping` implements a bootstrapping version of various methods of Multivariate Time Series missing data imputation.
#' Currently the following functions are implemented: Amelia, mtsdi, missRanger.
#' For each bootstrap a different block of time will be set to missing and data will be imputed.
#' 
#' @param df_complete data frame. Data frame with no missing values.
#' @param bootstrap_nr integer. Number of bootstrap repetitions (default: 100).
#' @param missing_fact numeric. Percentage of missingness to set at each repetition (default: 0.25).
#' @inheritParams MTSimputation

if(!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
source(file = "../Helpers/MTSimputation.R", chdir = TRUE)

MTSbootstrapping <- function(df_complete, ts_variable, cs_variable, relevant_variables = NULL,
                             bootstrap_nr = 100, missing_fact = 0.25,
                             funs = c("Amelia", "mtsdi", "missRanger"),
                             imputation_nr = 5,
                             idvars = NULL, 
                             lags = NULL, 
                             leads = NULL, 
                             logs = NULL, 
                             sqrts = NULL,
                             noms = NULL,
                             ords = NULL,
                             bounds = NULL,
                             polytime = NULL,
                             splinetime = NULL,
                             ci_imputations = FALSE,
                             ci_level = ci_level,
                             ci_type = ci_type) {
  
  # If variables in `noms` are not factors (e.g. characters), transform into factor
  if (!is.null(noms)) {
    for (i in noms) {
      if (!is.factor(df_complete[[i]])) {
        message("The variable ", i, " will be transformed into a factor variable.")
        df_complete[[i]] <- as.factor(df_complete[[i]])
      }
    }
  }
  
  # Variables for which we want to analyze imputation
  # If not indicated specific variables, analyze all numeric variables
  if (is.null(relevant_variables)) {
    variables <- colnames(df_complete)
    missing_variables <- sapply(df_complete,
                                function(x) {is.numeric(x) & (sum(is.na(x)) == 0)})
    relevant_variables <- variables[missing_variables]
    }
  
  function_error_list <- list()
  
  for (bstr in 1:bootstrap_nr) {
    
    cat("Bootstrapping run:", paste0(bstr, "/", bootstrap_nr), "\n")
    
    # Assign missing values at random
    df_missing <- df_complete %>%
      mutate(row_n = row_number()) %>%
      mutate_at(vars(one_of(relevant_variables)), 
                ~ifelse(row_n %in% sample(x = n(), size = round(missing_fact*n())), NA, .)) %>%
      ungroup() %>%
      select(-row_n)
    
    # Run imputation
    imputed_list <- MTSimputation(df_missing = df_missing,
                                  funs = funs,
                                  ts_variable = ts_variable,
                                  cs_variable = cs_variable,
                                  relevant_variables = relevant_variables,
                                  imputation_nr = imputation_nr,
                                  idvars = idvars,
                                  lags = lags,
                                  leads = leads,
                                  logs = logs,
                                  sqrts = sqrts,
                                  noms = noms,
                                  bounds = bounds,
                                  polytime = polytime,
                                  splinetime = splinetime,
                                  ci_imputations = ci_imputations,
                                  ci_level = ci_level,
                                  ci_type = ci_type)
    
    # Extract error statistics
    function_error_list[[bstr]] <- lapply(names(imputed_list),
                                          function(f) {
                                            df_imputed <- imputed_list[[f]]
                                            error_stats <- sapply(relevant_variables,
                                                                  function(i) {
                                                                    index_missing <- df_missing %>% 
                                                                      mutate(row_n = row_number()) %>%
                                                                      filter_at(vars(i), is.na) %>%
                                                                      pull(row_n)
                                                                    actual    <- df_complete %>% slice(index_missing) %>% pull(!!i)
                                                                    predicted <- df_imputed %>% slice(index_missing) %>% pull(!!i)
                                                                    mae <- Metrics::mae(actual = actual, predicted = predicted)
                                                                    mse <- Metrics::mse(actual = actual, predicted = predicted)
                                                                    mape <- Metrics::mae(actual = actual, predicted = predicted)
                                                                    rmse <- Metrics::rmse(actual = actual, predicted = predicted)
                                                                    error_list <- c(MAE = mae, MSE = mse,
                                                                                    MAPE = mape, RMSE = rmse)
                                                                  }) %>%
                                              apply(., 1, mean) %>%
                                              as.data.frame() %>%
                                              setNames(nm = "value") %>%
                                              rownames_to_column(var = "stat") %>%
                                              mutate(imp_fun = f)
                                          }) %>%
      bind_rows() %>%
      pivot_wider(names_from = "imp_fun", values_from = "value")
    
    
  }
  
  # Calculate confidence intervals for error statistics
  return(function_error_list)
}