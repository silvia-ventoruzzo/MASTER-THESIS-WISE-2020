#' @title Multi Horizon SPA test
#' 
#' @description 
#' The function `multi_horizon_forecast_comparison` implements the the tests developed in the paper Multi Horizon Forecast Comparison (Quaedvlieg, 2019).
#' Developed from the Matlab code provided by Quaedvlieg in his website: https://sites.google.com/site/rogierquaedvlieg/research
#' 
#' @param loss_diff matrix. Matrix of the loss difference between model 1 and model 2.
#' @param weights numeric. Numeric vectors for weights of the different horizons in the aSPA test.
#' @param B integer. Number of bootstrap repetitions.
#' @param l integer. Length of the block of the moving block bootstrap samples.
#' @param alpha numeric. Significance level (defaul: 0.05).
#' @param seed integer. Seed for reproducibility.

if(!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if(!require(tseries)) install.packages("tseries"); library(tseries)

# Weights for HAC variance
QS_weights <- function(x) {
  argQS <- 6*pi*x/5
  # w <- 3 * (1/argQS)^2 * (sin(argQS)/argQS - cos(argQS))
  w1 <- 3/(argQS^2)
  w2 <- (sin(argQS)/argQS)-cos(argQS)
  wQS <- w1*w2
  wQS[x == 0] <- 1
  return(wQS)
}

# HAC variance
QS_var <- function(x) {
  x <- as.matrix(x)
  T <- nrow(x)
  N <- ncol(x)
  bw <- 1.3221*T^(1/5)
  weight <- QS_weights((1:T-1)/bw)
  omega <- rep(0, N)
  for (i in 1:N) {
    workdata <- x[, i] - mean(x[, i])
    omega[i] <- t(workdata)%*%workdata/T
    for (j in 1:(T-1)) {
      omega[i] <- omega[i] + 2*weight[j]*t(workdata[1:(T-j)])%*%workdata[(j+1):T]/T
    }
  }
  return(omega)
}

# Bootstrap variance
b_variance <- function(x, l) {
  x <- as.matrix(x)
  T <- nrow(x)
  N <- ncol(x)
  omega <- rep(0, N)
  xmean <- apply(x, 2, mean)
  xdem <- x -  do.call(rbind, replicate(T, xmean, simplify=FALSE))
  K <- floor(T/l)
  for (n in 1:N) {
    temp <- matrix(xdem[1:(K*l), n], nrow = K, ncol = l)
    omega[n] <- mean(apply(temp, 2, sum)^2)/l
  }
  return(omega)
}

# uSPA test
uSPA_test <- function(loss_diff, B, l, alpha = 0.05, seed = NULL) {
  # Progress bar
  pb <- txtProgressBar(min = 0, max = 1, style = 3)
  loss_diff <- as.matrix(loss_diff)
  T <- nrow(loss_diff)
  # Mean loss diff for each h = {1, ..., H}
  d_ij <- apply(loss_diff, 2, mean)
  # Test statistic
  t_uSPA <- min(sqrt(T)*d_ij/sqrt(QS_var(loss_diff)))
  # Bootstrap statistic
  demeaned_loss_diff <- sweep(x = loss_diff, MARGIN = 2, STATS = d_ij, FUN = "-")
  t_uSPA_b <- rep(0, B)
  if (!is.null(seed)) set.seed(seed)
  b_IDs <- tseries::tsbootstrap(x = 1:T, nb = B, statistic = NULL, b = l, type = "block")
  for (b in 1:B) {
    Sys.sleep(0.02)
    setTxtProgressBar(pb, value = (b/B))
    id <- b_IDs[,b]
    b_lossdiff <- demeaned_loss_diff[id,]
    omega_b <- b_variance(b_lossdiff, l)
    t_uSPA_b[b] <- min(sqrt(T)*apply(b_lossdiff, 2, mean)/sqrt(omega_b))
  }
  # Critical value
  c_uSPA_alpha <- quantile(t_uSPA_b, 1-alpha)
  alpha_percent <- names(c_uSPA_alpha)
  c_uSPA_alpha <- unname(c_uSPA_alpha)
  # p-value
  uSPA_pvalue <-  mean(t_uSPA < t_uSPA_b)
  # List of values
  result <- list(t           = t_uSPA,
                 t_bootstrap = t_uSPA_b,
                 c_alpha     = c_uSPA_alpha,
                 p_value     = uSPA_pvalue)
  names(result)[3] <- paste0("critical_value_", alpha_percent)
  setTxtProgressBar(pb, value = B)
  return(result)
}

# aSPA test
aSPA_test <- function(loss_diff, weights = NULL, B, l, alpha = 0.05, seed = NULL) {
  # Progress bar
  pb <- txtProgressBar(min = 0, max = 1, style = 3)
  loss_diff <- as.matrix(loss_diff)
  T <- nrow(loss_diff)
  H <- ncol(loss_diff)
  # Weights: if not provided, use equal weights
  if (is.null(weights)) weights <- rep(1/H, H)
  # Weighted loss difference
  weighted_loss_diff <- apply(sweep(x = loss_diff, MARGIN = 2, STATS = weights, FUN = "*"), 1, sum)
  # Weighted mean loss diff for each h = {1, ..., H}
  d_ij <- mean(weighted_loss_diff)
  # Test statistic
  t_aSPA <- sqrt(T)*d_ij/sqrt(QS_var(weighted_loss_diff))
  # Bootstrap statistic
  demeaned_loss_diff <- weighted_loss_diff - d_ij
  t_aSPA_b <- rep(0, B)
  if (!is.null(seed)) set.seed(seed)
  b_IDs <- tseries::tsbootstrap(x = 1:T, nb = B, statistic = NULL, b = l, type = "block")
  for (b in 1:B) {
    Sys.sleep(0.02)
    setTxtProgressBar(pb, value = (b/B))
    id <- b_IDs[,b]
    b_lossdiff <- demeaned_loss_diff[id]
    zeta_b <- b_variance(b_lossdiff, l)
    t_aSPA_b[b] <- sqrt(T)*mean(b_lossdiff)/sqrt(zeta_b)
  }
  # Critical value
  c_aSPA_alpha <- quantile(t_aSPA_b, 1-alpha)
  alpha_percent <- names(c_aSPA_alpha)
  c_aSPA_alpha <- unname(c_aSPA_alpha)
  # p-value
  aSPA_pvalue <-  mean(t_aSPA < t_aSPA_b)
  # List of values
  result <- list(t           = t_aSPA,
                 t_bootstrap = t_aSPA_b,
                 c_alpha     = c_aSPA_alpha,
                 p_value     = aSPA_pvalue)
  names(result)[3] <- paste0("critical_value_", alpha_percent)
  setTxtProgressBar(pb, value = B)
  return(result)
}