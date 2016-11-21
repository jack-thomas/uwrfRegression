#' Simplified Exponential Smoothing
#'
#' This function computes an exponentially smoothed time series with a given alpha by
#' simplifying the process.
#' @param x a vector to be exponentially smoothed
#' @keywords eSmooth
#' @export
#' @examples
#' eSmooth()

eSmooth <- function(x){
  hw.x <- HoltWinters(x, alpha = 0.2, beta = FALSE, gamma = FALSE)
  es.x <- rbind(hw.x$fitted, hw.x$coefficients)
  es.x <- es.x[,2]
  return(es.x)
}