#' Simplified Exponential Smoothing
#'
#' This function computes an exponentially smoothed time series with a 
#' given alpha by simplifying the process.
#' @param x a vector to be exponentially smoothed
#' @param alpha Holt-Winter smoothing alpha
#' @keywords eSmooth
#' @export
#' @examples
#' eSmooth()

eSmooth <- function(x, alpha = 0.2){
  hw.x <- HoltWinters(x, alpha = alpha, beta = FALSE, gamma = FALSE)
  es.x <- rbind(hw.x$fitted, hw.x$coefficients)
  es.x <- es.x[,2]
  return(es.x)
}