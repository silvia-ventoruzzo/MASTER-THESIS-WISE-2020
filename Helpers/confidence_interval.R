#' @title Calculation of confidence interval.
#' 
#' @description 
#' The function `confidence_interval` calculates the confidence interval of a value.
#' 
#' @param x numeric. Vector of values to calculate confidence interval of.
#' @param level numeric. Confidence level (defaul: 0.95).
#' @param type character. Type of confidence interval, one of "percentile" (default) and "se".
#' @param point_estimate numeric. Value of the point estimate. Can be omitted by setting to NULL (default).
#' @param point_estimate_calculation character. How to calculate point estimate if not provided. One of "mean" (default), "median" and "mode".

confidence_interval <- function(x, level = 0.95, type = "percentile", 
                                point_estimate = NULL, point_estimate_calculation = "mean") {
  
  ## Checks
  if (!is.numeric(x)) stop("The vector x is not numeric.")
  if (level > 1 | level < 0) stop("The value in level needs to be between 0 and 1.")
  if (!(type %in% c("percentile", "se"))) stop("The type", type, "is not accepted. Please choose between `percentile` and `se`.")
  if (!is.numeric(point_estimate) & !is.null(point_estimate)) stop("The point estimate is not numeric.")
  
  # Calculate of confidence interval for type `percentile`
  if (type == "percentile") {
    ci_values <- quantile(x = x, probs = c((1 - level) / 2, level + (1 - level) / 2),
                          na.rm = TRUE)
    ci <- data.frame(ci_values[1], ci_values[2]) %>%
      setNames(nm = c(paste0("ci_lower_", gsub("%", "", names(ci_values)[1])),
                      paste0("ci_upper_", gsub("%", "", names(ci_values)[2])))) %>%
      remove_rownames()
  # Calculate of confidence interval for type `se`
  } else if (type == "se") {
    # Checks
    if (is.null(point_estimate)) {
      if (is.null(point_estimate_calculation)) {
        warning("Define either point_estimate or point_estimate_calculation (point_estimate takes precedence).")
      } else if (point_estimate_calculation == "mode") {
        x_unique <- unique(x = x)
        point_estimate <- x_unique[which.max(tabulate(match(x, x_unique)))]
      } else if (point_estimate_calculation %in% c("mean", "median")) {
        point_estimate <- summary(object = x)[[str_to_title(point_estimate_calculation)]]
      } else {
        stop("The parameter `point_estimate_calculation` can only be `mean`, `median` or `mode`.")
      }
    }
    score <- qnorm(1 - (1 - level) / 2)
    ci <- data.frame(point_estimate - score * sd(x = x, na.rm = TRUE), 
                     point_estimate + score * sd(x = x, na.rm = TRUE)) %>%
      setNames(nm = c(paste0("ci_lower_", round(((1-level)/2)*100, 2)),
                      paste0("ci_upper_", round((level+(1-level)/2)*100, 2))))
  }
  
  return(ci)
}
