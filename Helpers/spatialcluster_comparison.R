#' @title
#' Validation statistics and silhuoette plot for clustering method, specifically for regionalization
#' 
#' @description
#' The function `cluster_validation` calculates the statistics and the silhouette plot for one clustering method.
#' The function `cluster_comparison` calculates the information above for multiple clustering methods.
#' The function `spatialcluster_comparison` calculates the information above for multiple clustering methods with respect to the feature and spatial distances.
#' @param labels numeric. Vector of cluster labels.
#' @param distance_matrix matrix. Matrix of distances among points.
#' @param stats character. Vector of statistics, has to be one or multiple from function `cluster.stats` from package `fpc` and/or connectivity from package `clValid`.
#' @param labels_list named list. Named list containing cluster labels vectors.
#' @param feature_distance_matrix matrix. Matrix of feature distances among points.
#' @param spatial_distance_matrix matrix. Matrix of spatial distances among points

if(!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if(!require(cluster)) install.packages("cluster"); library(cluster)
if(!require(fpc)) install.packages("fpc"); library(fpc)
if(!require(clValid)) install.packages("clValid"); library(clValid)
if(!require(factoextra)) install.packages("factoextra"); library(factoextra)

# Calculation of validation statistics and silhouette plot
cluster_validation <- function(labels, distance_matrix, stats = c("dunn", "average.within"),
                               plot_title = "Silhouette plot") {
  # Validation statistics
  cluster_statistics <- fpc::cluster.stats(d = distance_matrix, clustering = labels)
  if ("connectivity" %in% stats) {
    connectivity <- clValid::connectivity(distance = distance_matrix,
                                          clusters = labels)
    stats <- setdiff(stats, "connectivity")
    val_stats <- sapply(stats, function(i) cluster_statistics[[i]]) %>%
      t() %>%
      as.data.frame() %>%
      mutate(connectivity = connectivity)
  } else {
    val_stats <- sapply(stats, function(i) cluster_statistics[[i]]) %>%
      t() %>%
      as.data.frame()
  }

  # Add number of clusters
  val_stats <- val_stats %>%
    mutate(k = max(labels))
  
  # Silhouette plot
  sil_plot <- fviz_silhouette(sil.obj = silhouette(x = labels, dmatrix = distance_matrix),
                              print.summary = FALSE,
                              ggtheme = theme_bw())
  plot <- sil_plot +
    labs(title = plot_title,
         subtitle = paste0("Avg silhouette width: ", round(mean(sil_plot$data$sil_width), 2))) +
    theme(plot.title = element_text(hjust = 0.5, size = 11),
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          legend.position = "bottom")
  
  return(list(validation_statistics = val_stats,
              silhouette_plot = plot))
}

# Calculation of validation statistics and silhouette plots for multiple clustering method
cluster_comparison <- function(labels_list, distance_matrix, stats = c("dunn", "average.within")) {
  
  val_all <- sapply(names(labels_list),
                    function(i) {cluster_validation(labels = labels_list[[i]],
                                                    distance_matrix = distance_matrix,
                                                    stats = stats,
                                                    plot_title = paste(i, "silhouette plot"))},
                    simplify = FALSE, USE.NAMES = TRUE)
  stats_all <- sapply(names(val_all),
                      function(i) {
                        val_all[[i]]$validation_statistics %>% 
                          mutate(cluster_method = i) %>%
                          dplyr::select(cluster_method, everything())},
                      simplify = FALSE, USE.NAMES = TRUE) %>%
    bind_rows()
                     
  plots_all <- sapply(names(val_all),
                      function(i) {val_all[[i]]$silhouette_plot},
                      simplify = FALSE, USE.NAMES = TRUE)                      
  
  return(list(validation_statistics = stats_all,
              silhouette_plot = plots_all))
}

# Calculation of validation statistics and silhouette plots
# for multiple clustering methods and for both feature and spatial distance
spatialcluster_comparison <- function(labels_list, feature_distance_matrix, spatial_distance_matrix,
                                      stats = c("dunn", "average.within")) {
  
  # Create list of distances matrices
  distance_matrix_list <- list(feature_distance = feature_distance_matrix,
                               spatial_distance = spatial_distance_matrix)
  
  # Calculation of all statistics and plots with both distance matrices
  val_all <- sapply(names(distance_matrix_list),
                    function(i) {
                      cluster_comparison(labels_list = labels_list, 
                                         distance_matrix = distance_matrix_list[[i]], 
                                         stats = stats)
                    }, simplify = FALSE, USE.NAMES = TRUE)
  
  stats_all <- sapply(names(val_all),
                      function(i) {
                        val_all[[i]]$validation_statistics %>% 
                          mutate(distance = i) %>%
                          dplyr::select(distance, everything())},
                      simplify = FALSE, USE.NAMES = TRUE) %>%
    bind_rows()
    
  plots_all <- sapply(names(val_all),
                      function(i) {
                        lapply(names(val_all[[i]]$silhouette_plot),
                               function(j) {
                                 p <- val_all[[i]]$silhouette_plot[[j]]
                                 p +
                                   labs(title = paste0(p$labels$title, 
                                                       " (", gsub("_", " ", i), ")"))
                                 })
                      },
                      simplify = FALSE, USE.NAMES = TRUE)    
  
  return(list(validation_statistics = stats_all,
              silhouette_plot = plots_all))
}