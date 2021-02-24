if(!require(purrr)) install.packages("purrr"); library(purrr)

#' The function `group_quantiles` calculates the quantiles from a grouped dataframe
grouped_quantiles <- function(p) {
  p_names <- paste0(p*100, "%")
  p_funs <- purrr:::map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% set_names(nm = p_names)
  return(p_funs)
}

#' The function `reference_years_calculation` extracts those years that are relevant for the analysis
reference_years_calculation <- function(y, n = 5) {
  
  check_years <- function(x, y) {
    yesno <- ifelse(x >= y-n & x <= y-1, TRUE, FALSE) 
    return(yesno)}
  
  y_names <- paste0("year_", y)
  y_funs <- purrr::map(y, ~partial(check_years, y = .x)) %>% set_names(nm = y_names)
  return(y_funs)
}