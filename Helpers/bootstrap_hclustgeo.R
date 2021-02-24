#' @title Bootstrap ClustGeo
#' 
#' @description The function `bootstrap_hclustgeo` implements the bootstrapped version of ClustGeo from the paper "Bootstrap ClustGeo with spatial constraints"
#' For the moment, only the Euclidean distance is implemented.  
#' 
#' @param data matrix or list. Matrix for UTS (rows: points, columns: time). List for MTS (elements: points, rows: features, columns: time).
#' @param D1 dist object or matrix. If matrix, it will be transformed into dist object.
#' @param bootstrap_nr integer. Number of bootstrap repetitions.
#' @param dist_measure character. One of "euclidean", "dtw" or "correlation". Default: "euclidean"
#' @param range_clusters numeric vector. Minimum and maximum number of clusters to sample from. If NULL (default), it takes the values  c(2, sqrt(N))
#' @inheritParams ClustGeo::hclustgeo
#' 
#' @details The number of clusters and the value of the parameter `alpha` can be chosen following the instructions from the package `ClustGeo` and using only the the feature distance matrix `D0`.

if(!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if(!require(ClustGeo)) install.packages("ClustGeo"); library(ClustGeo)
if(!require(EnsCat)) install.packages("EnsCat"); library(EnsCat)
if(!require(dendextend)) install.packages("dendextend"); library(dendextend)
if(!require(parallelDist)) install.packages("parallelDist"); library(parallelDist)

bootstrap_hclustgeo <- function(data, D1,
                                bootstrap_nr, dist_measure = "euclidean",
                                range_clusters = NULL,
                                alpha = 0, scale = TRUE, wt = NULL) {
  pb <- txtProgressBar(min = 0, max = 1, style = 3)
  
  # Identify type of data (matrix or list)
  # Extract N (number of points) and M (number of features)
  if (is.matrix(data)) {
    N = nrow(data)
    M = 1
  } else if (is.list(data)) {
    if (all(sapply(data, is.matrix))) {
      if (any(sapply(data, dim) != dim(data[[1]]))) stop("Not all matrices have the same size.")
      N <- length(data)
      M <- nrow(data[[1]])
    } 
  } else {
    stop("Not all elements are of the same or correct type.")
  }
  
  # Check if D1 is dist or matrix and transform matrix to dist
  if (dendextend::is.dist(D1)) {
    D1 <- base::as.matrix(D1)
  } else if (!(is.matrix(D1))) {
    stop("D1 needs to be either a dist object or a matrix.")
  }
  
  # If range_clusters = NULL (default), it takes the values  c(2, sqrt(N))
  if (is.null(range_clusters)) {
    range_clusters <- c(2, floor(sqrt(N)))
  }
  
  # Initialize object for cluster vectors
  # clusters = NULL
  clusters <- data.frame(n = 1:N)
  
  # Bootstrapping
  for (i in 1:bootstrap_nr) {

    # pb$tick()
    # Sys.sleep(1/bootstrap_nr)
    Sys.sleep(0.02)
    setTxtProgressBar(pb, value = (i/bootstrap_nr))
    
    # Sampling of number of clusters from range
    sample_k <- base::sample(range_clusters[1]:range_clusters[2], 1) 
    sample_n <- unique(base::sample(N, replace = TRUE))
    sample_data <- data[sample_n]
    
    # Distance matrix
    if (dist_measure == "euclidean") {
      D0_sample <- parDist(x = sample_data, method = "euclidean")
    } else if (dist_measure == "dtw") {
      D0_sample <- parDist(x = sample_data,
                           method = "dtw", step.pattern = "symmetric2", norm.method = "path.length")
    } else if (dist_measure == "correlation") {
      point_grid <- combinations(n = length(sample_n), r = 2, 
                                 v = sample_n,
                                 repeats.allowed = TRUE)
      corr_matrix <- matrix(nrow = length(sample_n), ncol = length(sample_n))
      rownames(corr_matrix) <- sample_n
      colnames(corr_matrix) <- sample_n
      if (is.list(data)){
        data_for_corr <- sapply(data,
                                function(x) as.vector(t(x)),
                                simplify = FALSE, USE.NAMES = TRUE)
        x <- data_for_corr[[sample_n[i, 1]]]
        y <- data_for_corr[[sample_n[i, 2]]]
      } else {
        x <- data[sample_n[i, 1],]
        y <- data[sample_n[i, 2],]
      }
      for (i in 1:nrow(point_grid)) {
        corr_matrix[point_grid[i, 1], point_grid[i, 2]] <- cor(x = x,
                                                               y = y)
      }
      corr_matrix[lower.tri(corr_matrix)]  <- t(corr_matrix)[lower.tri(corr_matrix)]
      D0_sample <- as.dist(corr_matrix)
    }
    
    D1_sample <- stats::as.dist(D1[sample_n, sample_n])
    hc <- ClustGeo::hclustgeo(D0 = D0_sample, D1 = D1_sample, 
                              alpha = alpha, scale = scale, wt = wt)
    clusters_bootstrap <- data.frame(n = sample_n,
                                     cluster = cutree(hc, k = sample_k)) %>%
      stats::setNames(nm = c("n", paste0("cluster_", i)))
    
    # clusters <- cbind(clusters, clusters_bootstrap)
    clusters <- full_join(clusters, clusters_bootstrap, by = "n")
  }
  # Hamming distance as new dissimilarity matrix
  DB = as.dist(EnsCat::hammingD(clusters)) 
  setTxtProgressBar(pb, value = bootstrap_nr)
  hcB <- ClustGeo::hclustgeo(D0 = DB, scale = scale, wt = wt)
  
  return(hcB)
}