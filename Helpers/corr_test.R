#' @title Correlation test
#' 
#' @description
#' The function `corr_test` performs the correlation test using the function `cor.test` for all pairs of variables in the dataframe.
#' It returns a dataframe with the results as rows.
#' 
#' @param df data.frame. This object contains the data.frame with the values as columns
#' The rest of the parameters are the sames as for the function `cor.test`

if(!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
if(!require(vcd)) install.packages("vcd"); require(vcd)

corr_test <- function(df, alternative = "two.sided", method = "pearson", conf.level = 0.95) {
  # Extract variables
  variables <- colnames(df)
  # Initialize dataframe
  correlation <- data.frame()
  # Run correlation test for all variable pairs
  for (i in 1:(length(variables)-1)) {
    for (j in (i+1):length(variables)) {
      corr.test_df <- data.frame(variable_1 = variables[i], variable_2 = variables[j])
      corr.test <- cor.test(x = df[[variables[i]]], y = df[[variables[j]]], 
                            alternative = alternative, method = method, conf.level = conf.level) %>%
        tidy()
      corr.test_df <- corr.test_df %>%
        cbind(corr.test)
      correlation <- correlation %>%
        rbind(corr.test_df)
    }
  }
  # Rename variable `estimate` according to correlation method chosen
  correlation <- correlation %>%
    rename_at(vars("estimate"), ~case_when(method == "pearson" ~ "cor",
                                           method == "kendall" ~ "tau",
                                           method == "spearman" ~ "rho")) %>%
    mutate(stat_significant = (p.value < (1-conf.level)))

  return(correlation)
}