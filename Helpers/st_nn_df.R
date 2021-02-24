#' @title
#' Dataframe of nearest neighbor(s)
#' 
#' @description
#' The function `st_nn_df` uses the function `st_nn` from the package `nngeo` to extract the nearest neighbor(s) and then creates a dataframe out of it.
#' The parameters are greatly those for function `st_nn`, view `help(st_nn)` for information on parameters `x`, `y` and `k`
#' @param x_names Character. Names of elements of `x`
#' @param y_names Character. Names of elements of `y`

if(!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if(!require(sf)) install.packages("sf"); library(sf)
if(!require(nngeo)) install.packages("nngeo"); library(nngeo)

st_nn_df <- function(x, y, k = 1, x_names, y_names) {
  # Create list with nearest neighbors
  nn_list <- nngeo::st_nn(x, y, k = k, returnDist = TRUE)
  cat("Transformation into dataframe...")
  # Set names of elements according to points in x
  names(nn_list$nn) <- as.character(x_names)
  names(nn_list$dist) <- as.character(x_names)
  # Create dataframes out of results
  nn_df <- bind_rows(nn_list$nn) %>%
    rownames_to_column(var = "nn") %>%
    mutate(nn = paste0("nn_", nn)) %>%
    pivot_longer(cols = all_of(as.character(x_names)), names_to = "x", values_to = "y_nn") %>%
    pivot_wider(id_cols = "x", names_from = "nn", values_from = "y_nn")
  dist_df <- bind_rows(nn_list$dist) %>%
    rownames_to_column(var = "nn") %>%
    mutate(nn = paste0("distance_m_", nn)) %>%
    pivot_longer(cols = all_of(as.character(x_names)), names_to = "x", values_to = "distance") %>%
    pivot_wider(id_cols = "x", names_from = "nn", values_from = "distance")
  
  # Create named list for y_names (names of nearest neighbors)
  nn_index_name <- setNames(y_names, 1:length(y_names))
  # Join distance and update nn values with y_names
  final_df <- nn_df %>%
    inner_join(dist_df, by = "x") %>%
    mutate_at(vars(contains("nn")), function(x) recode(as.character(x), !!!nn_index_name))
  cat(" done!\n")
  return(final_df)
}