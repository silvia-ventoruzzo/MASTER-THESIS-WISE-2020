#' @title
#' CorClustST: Clustering for spatio-temporal data
#' 
#' @description
#' Implementation of the clustering method CorClustST from the paper “CorClustST — correlation-based clustering of big spatio-temporal datasets”
#' @param data matrix or data.frame. Numeric matrix or dataframe (columns: time, rows: points).
#' @param points data.frame, sf object. If matrix rows are points and columns are coordinates (long, lat).
#' @param ids character. Vector of point ids. 
#' @param cor character. Type of correlation to calculate ("method" in function stats::cor). Default: "pearson"
#' @param epsilon number. Upper bound of distance for neighbors.
#' @param rho number. Lower bound of correlation for neighbors. Value needs to be between 0 and 1. Default: 0.8

# Needed packages
needed_packages <- c("tidyverse", "sf", "raster")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE)) {install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages", "package")

CorClustST <- function(data, points, ids,
                       cor = "pearson", epsilon, rho = 0.8) {
  
  ############
  ## Checks ##
  ############
  # Check that data is of correct type
  if (is.data.frame(data)) {
    data <- as.matrix(data)
  } else if(!is.matrix(data)) {
    stop("Data must be either a numeric matrix or a dataframe.")
  }
  
  # Check that points if of correct type
  if (is.matrix(points)) {
    if (ncol(points) != 2) stop("The matrix points need to have only 2 columns (long, lat).")
  } else if (!st_is_longlat(station_points)) {
    stop("The object is not of correct type or the simple feature coordinates are not longlat degrees.")
  }
    
  # Check that number of ids is the same has the number of columns of data and points
  if ((length(ids) != ncol(data) | (length(ids) != nrow(points)))) {
    stop("Number of ids does not correspond to the number of points of either data or points.")
  }
  
  # Check if matrix (or matrices in list) have ids as colnames, otherwise set them
  if (is.null(colnames(data))) {
    colnames(data) <- ids
  }
  
  ###############
  ## Algorithm ##
  ###############
  
  ## Step 0: Find all neighbors for each point
  ############################################
  # Calculate distance
  if (is.matrix(points)) {
    sp_dist <- raster::pointDistance(p1 = points, p2 = points, lonlat = TRUE, allpairs = TRUE)
  } else {
    sp_dist <- st_distance(x = points)
    class(sp_dist) <- "matrix"
  }
  rownames(sp_dist) <- ids
  colnames(sp_dist) <- ids
  # Select neighbors as those points that have distance lower than epsilon (higher than 0 to avoid same point)
  sp_neighbors <- apply(sp_dist, 2, function(x) rownames(sp_dist)[(x <= epsilon) & (x > 0)])
  
  ## Step 1: Calculate correlation for neighbors and arrange them by descending amount of neighbors
  #################################################################################################
  # Calculate correlation with neighbors
  
  ft_corr <- sapply(names(sp_neighbors),
                    function(i) {
                      lapply(sp_neighbors[[i]],
                             function(j) {
                               s <- data[,colnames(data) == i]
                               q <- data[,colnames(data) == j]
                               corr <- cor(x = s, y = q, method = cor, use = "pairwise.complete.obs")
                               # corr <- cor(x = data, method = cor, "pairwise.complete.obs")
                               return(data.frame(id = j,
                                                 cor = corr,
                                                 stringsAsFactors = FALSE))
                             }) %>%
                        bind_rows() %>%
                        filter(cor > rho) %>%
                        arrange(desc(cor))
                    },
                    simplify = FALSE, USE.NAMES = TRUE)
  # Rearrange list according to number of spatio-temporal neighbors
  order_points <- sapply(names(ft_corr), function(i) nrow(ft_corr[[i]]))
  order_points <- order_points[order_points > 0] %>%
    sort(decreasing = TRUE) %>%
    names()
  ft_corr_ordered <- ft_corr[order_points]
  
  # Step 2-4: Assign point as center and neighbors to the cluster if they satisfy certain conditions
  ##################################################################################################
  # Step 2 is together with steps 3 and 4 because at the beginning no point has been assign a cluster label
  # Create empty dataframe to fill with cluster labels
  clusters <- data.frame(id = ids,
                         center = rep(NA, length(ids)),
                         label = rep(NA, length(ids)),
                         stringsAsFactors = FALSE)
  # First cluster label
  c = 1
  # names(ft_corr_ordered) <- paste0("D", names(ft_corr_ordered))
  
  # Go through points which were ordered according to the amount of neighbors
  for (i in names(ft_corr_ordered)) {
    neighbors <- pull(ft_corr_ordered[[i]], id)
    # Point should not belong to a cluster
    not_in_cluster <- is.na(clusters$label[clusters$id == i])
    # More than 50% of its neighbors should not belong to a cluster
    half_not_in_cluster <- sum(is.na(clusters$label[clusters$id %in% neighbors]))/length(neighbors) > 0.5
    if (not_in_cluster & half_not_in_cluster) {
    # if (not_in_cluster & half_not_in_cluster) {
      # Define point as new center
      clusters <- clusters %>%
        mutate(center = ifelse(id == i, c, center),
               label = ifelse(id == i, c, label))
      neighbor_label <- clusters %>%
        mutate(neighbor = (id %in% neighbors),
               na_label = (is.na(label)))
      # Assign neighbors to new cluster if they weren't in any cluster
      no_label_neighbors <- neighbor_label %>%
        filter(neighbor, na_label) %>%
        pull(id)
      clusters <- clusters %>%
        mutate(label = ifelse(id %in% no_label_neighbors, c, label))
      # Or if they have an higher correlation
      label_neighbors <- neighbor_label %>%
        filter(neighbor, !na_label) %>%
        pull(id)
      for (j in label_neighbors) {
        # Extract center point from present cluster
        cluster_label <- clusters$label[clusters$id == j]
        cluster_center <- clusters$id[(clusters$center == cluster_label) & !is.na(clusters$center)]
        # Compare correlations and extract cluster center with highest correlation
        center_high_cor <- sapply(c(i, cluster_center),
                                  function(k) {
                                    ft_corr_ordered[[k]] %>%
                                      filter(id == j) %>%
                                      pull(abs(cor))
                                  }) %>%
          which.max() %>%
          names()
        # Assign same cluster 
        clusters <- clusters %>%
          mutate(label = ifelse(id == j, clusters$label[clusters$id == center_high_cor], label))
      }
      # Increase cluster number only if created new cluster
      c <- c + 1
    } 
  }
  
  # Step 5: If points not assigned and have neighbors, assign them to the cluster of the neighbors with highest correlation
  #########################################################################################################################
  # Go through points wich have neighbors but no label
  for (i in names(ft_corr_ordered)[names(ft_corr_ordered) %in% clusters$id[is.na(clusters$label)]]) {
    # Extract neighbor with highest correlation
    point_high_corr <- ft_corr_ordered[[i]] %>%
      slice(1) %>% # neighbors already ordered by descending correlation
      pull(id)
    # Assign cluster label from this neighbor
    clusters <- clusters %>%
      mutate(label = ifelse(id == i, clusters$label[clusters$id == point_high_corr], label))
  }
  
  # Step 6: if still not assigned, define as "noise" (label = 0)
  ##############################################################
  clusters <- clusters %>%
    mutate(label = ifelse(is.na(label), 0, label))
  
  # Create final list for export
  ##############################
  result_list <- list(clusters = clusters,
                      cor = cor,
                      epsilon = epsilon,
                      rho = rho)
  
  return(result_list)
}