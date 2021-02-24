#' @title Plots to chose number of clusters in Hierarchical Clustering
#' 
#' @description The function `hclust_nclust` uses the results from Hierarchical Clustering to plot various statistics in order to choose the number of clusters.  
#' 
#' @param hclust_result hclust object. Results from Hierarchical Clustering.
#' @param feature_distance_matrix dist object or matrix. If matrix, it will be transformed into dist object.
#' @param bootstrap_nr integer. Number of bootstrap repetitions.
#' @param dist_measure character. One of "euclidean", "dtw" or "correlation". Default: "euclidean"
#' @param range_clusters numeric vector. Minimum and maximum number of clusters to sample from. If NULL (default), it takes the values  c(2, sqrt(N))
#' @inheritParams ClustGeo::hclustgeo

if(!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if(!require(cluster)) install.packages("cluster"); library(cluster)
if(!require(inflection)) install.packages("inflection"); library(inflection)
if(!require(clValid)) install.packages("clValid"); library(clValid)
if(!require(dynamicTreeCut)) install.packages("dynamicTreeCut"); library(dynamicTreeCut)
if(!require(factoextra)) install.packages("factoextra"); library(factoextra)

hclust_nclust <- function(hclust_result,
                           feature_distance_matrix,
                           spatial_distance_matrix,
                           k = c(1, 10),
                          colors = c("steelblue", "indianred"),
                          delta = FALSE,
                          adaptive_tree_cut = FALSE) {
  
  # Average silhouette value
  silhouette_avg <- function(labels, distance_matrix) {
    if (max(labels) == 1) {
      avg_sil <- 0
    } else {
      avg_sil <- silhouette(x = labels, dmatrix = distance_matrix) %>%
        summary() %>%
        .$avg.width
    }
    return(avg_sil)
  }
  # Total within Sum of Squares
  wss <- function(labels, distance_matrix) {
    sapply(1:max(labels),
           function(i) {
             cluster_size <- sum(labels == i)
             dmatrix <- as.dist(distance_matrix[labels == i, cluster_labels == i])
             wss <- sum(dmatrix^2)/cluster_size
             return(wss)
           }) %>%
      sum()
  }
  
  # Delta
  # Certain aggl. clustering functions have a different distance matrix than hclust, in that case it will be transformed
  if (delta) {
    feature_distance_matrix = (feature_distance_matrix^2)/(2*nrow(feature_distance_matrix))
    spatial_distance_matrix = (spatial_distance_matrix^2)/(2*nrow(spatial_distance_matrix))
  }

  # Calculation
  ncluster_var <- data.frame(n_cluster = min(k):max(k),
                             silhouette = rep(NA, max(k)),
                             wss = rep(NA, max(k)),
                             distance = "feature distances",
                             stringsAsFactors  = FALSE)
  ncluster_geo <- data.frame(n_cluster = min(k):max(k),
                             silhouette = rep(NA, max(k)),
                             wss = rep(NA, max(k)),
                             distance = "spatial distances",
                             stringsAsFactors = FALSE)
  for (i in min(k):max(k)) {
    cluster_labels <- cutree(hclust_result, k = i)
    # feature_distance_matrix
    ncluster_var[i, "silhouette"] <- silhouette_avg(cluster_labels, feature_distance_matrix)
    ncluster_var[i, "wss"] <- wss(cluster_labels, feature_distance_matrix)
    ncluster_var[i, "dunn"] <- clValid::dunn(distance = feature_distance_matrix, clusters = cluster_labels)
    # spatial_distance_matrix
    ncluster_geo[i, "silhouette"] <- silhouette_avg(cluster_labels, spatial_distance_matrix)
    ncluster_geo[i, "wss"] <- wss(cluster_labels, spatial_distance_matrix)
    ncluster_geo[i, "dunn"] <- clValid::dunn(distance = spatial_distance_matrix, clusters = cluster_labels)
  }

  ncluster <- bind_rows(ncluster_var, ncluster_geo) %>%
    group_by(distance) %>%
    mutate(sil_k = min(n_cluster[which.max(silhouette)]),
           wss_k = inflection::uik(x = n_cluster, y = wss),
           dunn_k = min(n_cluster[which.max(dunn[is.finite(dunn)])])) %>%
    ungroup()

  # Plots
  plot_list <- list()
  # Silhouette method
  plot_list[["silhouette"]] <- ncluster %>%
    ggplot(aes(x = n_cluster, y = silhouette, color = distance)) +
    geom_line() +
    geom_point() +
    geom_vline(aes(xintercept = sil_k, color = distance), linetype = "dashed") +
    # scale_x_continuous(breaks = min(k):max(k)) +
    scale_x_continuous(breaks = min(k):max(k)) +
    scale_color_manual(values = colors) +
    # facet_wrap(~distance, ncol = 1, nrow = 2, scales = "free_y") +
    theme_bw() +
    labs(title = "Silhouette method",
         x = "Number of clusters k",
         y = "Average silhouette width") +
    theme(plot.title = element_text(hjust = 0.5, size = 11),
          axis.title = element_text(size = 9),
          legend.title = element_blank())
  # Elbow method
  plot_list[["wss"]] <- ncluster %>%
    group_by(distance) %>%
    mutate(wss = scale(wss)) %>%
    ungroup() %>%
    ggplot(aes(x = n_cluster, y = wss, color = distance)) +
    geom_line() +
    geom_point() +
    geom_vline(aes(xintercept = wss_k, color = distance), linetype = "dashed") +
    scale_x_continuous(breaks = min(k):max(k)) +
    scale_color_manual(values = colors) +
    # facet_wrap(~distance, ncol = 1, nrow = 2, scales = "free_y") +
    theme_bw() +
    labs(title = "Elbow method",
         x = "Number of clusters k",
         y = "Scaled total WSS") +
    theme(plot.title = element_text(hjust = 0.5, size = 11),
          axis.title = element_text(size = 9),
          legend.title = element_blank())
  # plot <- grid.arrange(grobs = plot_list,
  #                      ncol = 2, nrow = 1,
  #                      top = "Optimal number of clusters")
  # Dunn Index
  plot_list[["dunn"]] <- ncluster %>%
    ggplot(aes(x = n_cluster, y = dunn, color = distance)) +
    geom_line() +
    geom_point() +
    geom_vline(aes(xintercept = dunn_k, color = distance), linetype = "dashed") +
    scale_x_continuous(breaks = min(k):max(k)) +
    scale_color_manual(values = colors) +
    theme_bw() +
    labs(title = "Dunn Index method",
         x = "Number of clusters k",
         y = "Dunn index") +
    theme(plot.title = element_text(hjust = 0.5, size = 11),
          axis.title = element_text(size = 9),
          legend.title = element_blank())
  # Dendogramm
  k_dendo <- NULL
  if (adaptive_tree_cut) {
    dendo_cut <- cutreeHybrid(dendro = hclust_result,
                              distM = feature_distance_matrix,
                              minClusterSize = 1,
                              verbose = 0)
    k_dendo <- max(c(max(dendo_cut$labels), 1))
  }

  plot_list[["dendogramm"]] <- fviz_dend(x = hclust_result,
                                         show_labels = FALSE,
                                         k = k_dendo,
                                         # rect = TRUE,
                                         ggtheme = theme_bw(),
                                         rect_border = "black",
                                         lwd = 0.5,
                                         main = "Dendogramm",
                                         xlab = "Points") +
    theme(plot.title = element_text(hjust = 0.5, size = 11),
          axis.title = element_text(size = 9))
  arranged_plot <- ggarrange(plotlist = plot_list, ncol = 2, nrow = 2, 
                             common.legend = TRUE, legend = "bottom")
  
  plot <- annotate_figure(arranged_plot,
                          top = text_grob("Optimal number of clusters", size = 12))
  return(plot)
}
