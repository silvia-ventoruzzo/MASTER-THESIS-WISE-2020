#' @title Imputation of missing data for Multivariate Time Series
#' 
#' @description 
#' The function `MTSimputation` implements various methods of Multivariate Time Series missing data imputation.
#' Currently the following functions are implemented: Amelia, mtsdi, missRanger.
#' 
#' @param df_missing data frame. Data frame of data containing misssing values.
#' @param funs Function to use to impute missing data (one of Amelia, mtsdi, missRanger).
#' @param ts_variable character. Name of variable with time points
#' @param cs_variable character. Name of cross-section variable, if present.
#' @param relevant_variables character. Name(s) of variables where to impute missing data.
#' @inheritParams Amelia
#' @inheritParams mtsdi
#' @inheritParams missRanger

source("../Helpers/confidence_interval.R")
if(!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
if(!require(Amelia)) install.packages("Amelia", repos = "http://cran.r-project.org"); require(Amelia)
if(!require(mtsdi)) install.packages("mtsdi"); require(mtsdi)
if(!require(missRanger)) install.packages("missRanger"); require(missRanger)

################
## ci_df #######
################
ci_df <- function(df_imputed_allimps, ts_variable, cs_variable, relevant_variables,
                  ci_level, ci_type, point_estimate = NULL, point_estimate_calculation = "mean") {
  
  df_ci <- df_imputed_allimps %>%
    mutate_all(function(i) ifelse(is.nan(i), NA, i)) %>%
    group_by_at(vars(cs_variable, ts_variable)) %>%
    summarize_at(vars(relevant_variables), 
                 list(ci = function(i) list(confidence_interval(x = i,
                                                                level  = ci_level,
                                                                type = ci_type,
                                                                point_estimate = point_estimate,
                                                                point_estimate_calculation = point_estimate_calculation)),
                      mean = function(i) mean(i, na.rm = TRUE))) %>%
    ungroup() %>%
    mutate_at(vars(ts_variable), function(i) as.Date(as.numeric(as.character(i)),
                                                     origin = "1970-01-01")) %>%
    rename_at(vars(contains("ci")), function(i) gsub("_ci", "", i)) %>%
    unnest(cols = relevant_variables, names_sep = "-")
  
  return(df_ci)
}

###################
## imputed_final ##
###################
imputed_final <- function(df_missing, df_ci, 
                          ts_variable, cs_variable, relevant_variables) {
  df_imputed <- df_missing %>%
    left_join(df_ci, by = c(cs_variable, ts_variable)) %>%
    pivot_longer(cols = c(relevant_variables, paste0(relevant_variables, "_mean")),
                 names_to = "variable", values_to = "value") %>%
    mutate(variable = gsub("_mean", "", variable)) %>%
    drop_na(value) %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    select(!!cs_variable, !!ts_variable, one_of(relevant_variables))
  return(df_imputed)
}



################
## Amelia ######
################
amelia_imputation <- function(df_missing, ts_variable, cs_variable, relevant_variables = NULL, 
                              imputation_nr = 5, missingness_pattern = NULL,
                              idvars = NULL, lags = NULL, leads = NULL, 
                              logs = NULL, sqrts = NULL, lgstc = NULL, noms = NULL, ords = NULL,
                              p2s = 0, bounds = NULL, polytime = NULL, splinetime = NULL,
                              return_ci = FALSE, ci_imputations = TRUE, ci_level, ci_type, ...) {
  
  # If variables in `noms` are not factors (e.g. characters), transform into factor
  if (!is.null(noms)) {
    for (i in noms) {
      if (!is.factor(df_missing[[i]])) {
        message("The variable ", i, " will be transformed into a factor variable.")
        df_missing[[i]] <- as.factor(df_missing[[i]])
      }
    }
  }
  
  # If variables in `ords` are not ordered, display error message
  if (!is.null(ords)) {
    for (i in ords) {
      if (!is.ordered(df_missing[[i]])) {
        stop("The variable", i, "is not ordered. Either assign it an order or use it in `noms`.")
      }
    }
  }
  
  # Extracting all numeric variables
  numeric_variables <- df_missing %>%
    select_if(is.numeric) %>%
    colnames()
  if (is.null(relevant_variables)) {
    variables <- colnames(df_missing)
    missing_variables <- sapply(df_missing,
                                function(x) {is.numeric(x) & (sum(is.na(x)) > 0)})
    relevant_variables <- variables[missing_variables]
  }
  
  if (is.null(missingness_pattern)) {
    # Define missingness pattern: missing values only important for relevant variables
    missingness_pattern <- df_missing %>%
      mutate_at(vars(relevant_variables), 
                function(x) ifelse(is.na(x), TRUE, FALSE)) %>%
      mutate_at(vars(-relevant_variables), 
                function(x) x = TRUE)
  }
  
  # Run all imputations for a specific pattern of missingness
  cat("Imputing values using `Amelia`.\n")
  amelia_results <- amelia(x = df_missing, m = imputation_nr,
                           ts = ts_variable, cs = cs_variable, 
                           idvars = idvars, lags = lags, leads = leads, logs = logs, sqrts = sqrts,
                           noms = noms, ords = ords,
                           p2s = p2s, bounds = bounds, polytime = polytime, splinetime = splinetime)
  
  # Bind all imputed dataframes
  df_imputed <- lapply(names(amelia_results$imputations), 
                       function(i) {
                         df <- amelia_results$imputations[[i]] %>%
                           ungroup()
                         df <- sapply(colnames(df), 
                                      function(j) {
                                        ifelse(missingness_pattern[[j]], df[[j]], NA)
                                      }) %>%
                           as.data.frame() %>%
                           mutate_at(vars(relevant_variables),
                                     function(x) as.numeric(as.character(x))) %>%
                           mutate_at(vars(ts_variable), 
                                     function(x) as.Date(as.numeric(as.character(x)),
                                                         origin = "1970-01-01")) %>%
                           mutate_at(vars(cs_variable), 
                                     function(x) as.character(x)) %>%
                           mutate(replicate = as.integer(gsub("imp", "", i)))
                       }) %>%
    bind_rows()
  
  # Confidence intervals and mean
  if (ci_imputations == TRUE) {
    cat("Calculating confidence intervals for `Amelia` imputed values.\n")
    df_ci <- ci_df(df_imputed_allimps = df_imputed, 
                   ts_variable = ts_variable,
                   cs_variable = cs_variable,
                   relevant_variables = relevant_variables,
                   ci_level = ci_level, 
                   ci_type = ci_type)
  } else {
    cat("Calculating means for `Amelia` imputed values.\n")
    df_ci <- df_imputed %>%
      mutate_all(function(i) ifelse(is.nan(i), NA, i)) %>%
      group_by_at(vars(cs_variable, ts_variable)) %>%
      summarize_at(vars(relevant_variables), 
                   list(mean = function(i) mean(i, na.rm = TRUE))) %>%
      ungroup() %>%
      mutate_at(vars(ts_variable), function(i) as.Date(as.numeric(as.character(i)),
                                                       origin = "1970-01-01"))
  }
  
  # Join with initial dataframe to fill missing values with the mean of the imputations
  df_imputed <- imputed_final(df_missing         = df_missing, 
                              df_ci              = df_ci,
                              ts_variable        = ts_variable, 
                              cs_variable        = cs_variable, 
                              relevant_variables = relevant_variables)
  
  if (return_ci == FALSE) {return(df_imputed)}
  else if (return_ci == TRUE) {
    results <- list(df_imputed = df_imputed,
                    df_ci      = df_ci)
    return(results)
  }
  
}

################
## mtsdi #######
################
mtsdi_imputation <- function(df_missing, ts_variable, cs_variable, relevant_variables, 
                             imputation_nr, missingness_pattern, ci_imputations = FALSE, 
                             ci_level, ci_type) {
  
  if(!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
  if(!require(mtsdi)) install.packages("mtsdi"); require(mtsdi)
  
  # Extracting all numeric variables
  numeric_variables <- df_missing %>%
    select_if(is.numeric) %>%
    colnames()
  
  if (is.null(relevant_variables)) {
    variables <- colnames(df_missing)
    missing_variables <- sapply(df_missing,
                                function(x) {is.numeric(x) & (sum(is.na(x)) > 0)})
    relevant_variables <- variables[missing_variables]
  }
  
  mtsdi_1imputation <- function(df_missing, ts_variable, cs_variable, relevant_variables = NULL) {
    # First try imputation method "gam" using all numeric variables except the one being imputed. 
    # If this leads to an error (possibly because of multicollinearity), use method "spline".
    mtsdi_results <- tryCatch({
      ga_formulas <- sapply(relevant_variables, 
                            function(i) {
                              other_variables <- numeric_variables[numeric_variables != i]
                              formula(paste(i, "~", paste(other_variables, collapse = " + ")))
                            })
      mtsdi_results <- mnimput(formula = formula(paste("~", paste(relevant_variables, collapse = " + "))), 
                               dataset = df_missing, 
                               by = df_missing[[cs_variable]],
                               ts = TRUE,
                               method = "gam",
                               ga.control = list(formula = ga_formulas))
    }, error = function(e) {
      message("Imputation with method `gam` not possible because of ", e,
              ". The method `spline` will be used instead.")
      mtsdi_results <- mnimput(formula = formula(paste("~", paste(relevant_variables, collapse = " + "))), 
                               dataset = df_missing, 
                               by = df_missing[[cs_variable]],
                               ts = TRUE)
    })
    
    # Extract dataframe with imputed values and add ts_variable and cs_variable
    df_imputed <- predict.mtsdi(mtsdi_results) %>%
      mutate(!!cs_variable := missing_data[[cs_variable]],
             !!ts_variable := missing_data[[ts_variable]]) %>%
      select(!!cs_variable, !!ts_variable, one_of(relevant_variables))
    
    return(df_imputed)
  }
  
  # Calculate all imputations and merge them
  cat("Imputing values using `mtsdi`.\n")
  df_imputed <- lapply(1:imputation_nr, function(i) {
    df <- mtsdi_1imputation(df_missing         = df_missing,
                            ts_variable        = ts_variable,
                            cs_variable        = cs_variable,
                            relevant_variables = relevant_variables)
    df <- sapply(colnames(df),
                 function(j) {
                   ifelse(missingness_pattern[[j]], df[[j]], NA)
                 }) %>%
      as.data.frame() %>%
      mutate_at(vars(relevant_variables),
                function(x) as.numeric(as.character(x))) %>%
      mutate_at(vars(ts_variable), 
                function(x) as.Date(as.numeric(as.character(x)),
                                    origin = "1970-01-01")) %>%
      mutate_at(vars(cs_variable), 
                function(x) as.character(x)) %>%
      mutate(row_n = row_number(),
             replicate = i)
  }) %>%
    bind_rows()
  
  if (ci_imputations == TRUE) {
    cat("Calculating confidence intervals for `mtsdi` imputed values.\n")
    df_ci <- ci_df(df_imputed_allimps = df_imputed, 
                   ts_variable = ts_variable,
                   cs_variable = cs_variable,
                   relevant_variables = relevant_variables,
                   ci_level = ci_level, 
                   ci_type = ci_type)
  } else {
    cat("Calculating means for `mtsdi` imputed values.\n")
    df_ci <- df_imputed %>%
      mutate_all(function(i) ifelse(is.nan(i), NA, i)) %>%
      group_by_at(vars(cs_variable, ts_variable)) %>%
      summarize_at(vars(relevant_variables), 
                   list(mean = function(i) mean(i, na.rm = TRUE))) %>%
      ungroup() %>%
      mutate_at(vars(ts_variable), function(i) as.Date(as.numeric(as.character(i)),
                                                       origin = "1970-01-01"))
  }
  
  
  
  df_imputed <- imputed_final(df_missing         = df_missing, 
                              df_ci              = df_ci,
                              ts_variable        = ts_variable, 
                              cs_variable        = cs_variable, 
                              relevant_variables = relevant_variables)
  
  return(df_imputed)
}


################
## missRanger ##
################
missRanger_imputation <- function(df_missing, ts_variable, cs_variable, relevant_variables, 
                                  imputation_nr, missingness_pattern,
                                  ci_imputations,
                                  ci_level, ci_type) {

  # Extracting all numeric variables
  numeric_variables <- df_missing %>%
    select_if(is.numeric) %>%
    colnames()
  if (is.null(relevant_variables)) {
    variables <- colnames(df_missing)
    missing_variables <- sapply(df_missing,
                                function(x) {is.numeric(x) & (sum(is.na(x)) > 0)})
    relevant_variables <- variables[missing_variables]
  }
  
  if (is.null(missingness_pattern)) {
    # Define missingness pattern: missing values only important for relevant variables
    missingness_pattern <- df_missing %>%
      mutate_at(vars(relevant_variables), 
                function(x) ifelse(is.na(x), TRUE, FALSE)) %>%
      mutate_at(vars(-relevant_variables), 
                function(x) x = TRUE)
  }
  
  # Create formula
  ranger_formula = as.formula(paste(paste(names(which(apply(df_missing, 2, function(x) sum(is.na(x))) > 0)), 
                                          collapse = " + "),
                                    "~ . -", ts_variable))
  
  # Calculate all imputations and merge them
  cat("Imputing values using `missRanger`.\n")
  df_imputed <- lapply(1:imputation_nr, function(i) {
    df <- missRanger(data = df_missing,
                     formula = ranger_formula,
                     verbose = 0)
    df <- sapply(colnames(df),
                 function(j) {
                   ifelse(missingness_pattern[[j]], df[[j]], NA)
                 }) %>%
      as.data.frame() %>%
      mutate_at(vars(relevant_variables),
                function(x) as.numeric(as.character(x))) %>%
      mutate_at(vars(ts_variable), 
                function(x) as.Date(as.numeric(as.character(x)),
                                    origin = "1970-01-01")) %>%
      mutate_at(vars(cs_variable), 
                function(x) as.character(x)) %>%
      mutate(row_n = row_number(),
             replicate = i)
  }) %>%
    bind_rows()
  
  if (ci_imputations == TRUE) {
    cat("Calculating confidence intervals for `missRanger` imputed values.\n")
    df_ci <- ci_df(df_imputed_allimps = df_imputed, 
                   ts_variable = ts_variable,
                   cs_variable = cs_variable,
                   relevant_variables = relevant_variables,
                   ci_level = ci_level, 
                   ci_type = ci_type)
  } else {
    cat("Calculating means for `missRanger` imputed values.\n")
    df_ci <- df_imputed %>%
      mutate_all(function(i) ifelse(is.nan(i), NA, i)) %>%
      group_by_at(vars(cs_variable, ts_variable)) %>%
      summarize_at(vars(relevant_variables), 
                   list(mean = function(i) mean(i, na.rm = TRUE))) %>%
      ungroup() %>%
      mutate_at(vars(ts_variable), function(i) as.Date(as.numeric(as.character(i)),
                                                       origin = "1970-01-01"))
  }

  
  df_imputed <- imputed_final(df_missing         = df_missing, 
                              df_ci              = df_ci,
                              ts_variable        = ts_variable, 
                              cs_variable        = cs_variable, 
                              relevant_variables = relevant_variables)
  
  return(df_imputed)
}

#############################################################################################
MTSimputation <- function(df_missing, funs = c("Amelia", "mtsdi", "missRanger"),
                          ts_variable, cs_variable, relevant_variables = NULL, 
                          missingness_pattern = NULL,
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
                          ci_imputations = TRUE,
                          ci_level = ci_level,
                          ci_type = ci_type) {
  
  # Variables for which we want to analyze imputation
  # If not indicated specific variables, analyze all numeric variables that had missing values
  if (is.null(relevant_variables)) {
    variables <- colnames(df_missing)
    missing_variables <- sapply(df_missing,
                                function(x) {is.numeric(x) & (sum(is.na(x)) > 0)})
    relevant_variables <- variables[missing_variables]
    }
  
  # Define missingness pattern: missing values only important for relevant variables
  missingness_pattern <- df_missing %>%
    mutate_at(vars(relevant_variables), 
              function(x) ifelse(is.na(x), TRUE, FALSE)) %>%
    mutate_at(vars(-relevant_variables), 
              function(x) x = TRUE)
  
  # Imputation process
  imputed_list <- c()
  if ("Amelia" %in% funs) {
    imputed_list[["Amelia"]] <- amelia_imputation(df_missing         = df_missing,
                                           ts_variable        = ts_variable,
                                           cs_variable        = cs_variable,
                                           relevant_variables = relevant_variables,
                                           imputation_nr      = imputation_nr,
                                           missingness_pattern = missingness_pattern,
                                           idvars = idvars, 
                                           lags = lags, 
                                           leads = leads, 
                                           logs = logs, 
                                           sqrts = sqrts,
                                           noms = noms,
                                           ords = ords,
                                           bounds = bounds,
                                           polytime = polytime,
                                           splinetime = splinetime,
                                           ci_imputations = ci_imputations,
                                           ci_level = ci_level,
                                           ci_type = ci_type)
  }
  if ("mtsdi" %in% funs) {
    imputed_list[["mtsdi"]] <- mtsdi_imputation(df_missing         = df_missing,
                                         ts_variable        = ts_variable,
                                         cs_variable        = cs_variable,
                                         relevant_variables = relevant_variables,
                                         imputation_nr      = imputation_nr,
                                         missingness_pattern = missingness_pattern,
                                         ci_imputations = ci_imputations,
                                         ci_level = ci_level,
                                         ci_type = ci_type)
    
  }
  if ("missRanger" %in% funs) {
    imputed_list[["missRanger"]] <- missRanger_imputation(df_missing         = df_missing,
                                                          ts_variable        = ts_variable,
                                                          cs_variable        = cs_variable,
                                                          relevant_variables = relevant_variables,
                                                          imputation_nr      = imputation_nr,
                                                          missingness_pattern = missingness_pattern,
                                                          ci_imputations = ci_imputations,
                                                          ci_level = ci_level,
                                                          ci_type = ci_type)
    
  }

  return(imputed_list)
}