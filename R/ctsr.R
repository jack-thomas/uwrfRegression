#' Time Series Regression
#'
#' This function performs time series regression with multiple methods, then it 
#' selects the best method based on mean absolute deviation. Once the best model
#' is selected, it returns some information about it.
#' @param data a vector of time series data.
#' @param frequency the number of observations per unit of time.
#' @keywords ctsr
#' @export
#' @examples
#' ctsr()

ctsr <- function(data, frequency = 1){
  mad <- c(1,2,3,4,5,6,7)
  t <- c(1:length(data))
# Methods to include
#  1. Exponential smoothing (eSmooth)
  mad[1] <- mean(abs(data - fitted(lm(eSmooth(data, alpha=0.2)~t))))
  mad[2] <- mean(abs(data - fitted(lm(eSmooth(data, alpha=0.4)~t))))
  mad[3] <- mean(abs(data - fitted(lm(eSmooth(data, alpha=0.6)~t))))
  mad[4] <- mean(abs(data - fitted(lm(eSmooth(data, alpha=0.8)~t))))
#  2. Basic linear model (lm)
  mad[5] <- mean(abs(data - fitted(lm(data~t))))
#  3. Polynomial model (ts.poly)
  mad[6] <- mean(abs(data - fitted(ts.poly(data))))
#  4. Trend analysis using seasonal indices (csts)
  mad[7] <- csts(data, frequency = frequency, mad = TRUE, df.print = FALSE)
  
  cat("MAD Values:")
  cat("\n  Exponential Smooth (alpha = 0.2)   :", mad[1])
  cat("\n  Exponential Smooth (alpha = 0.4)   :", mad[2])
  cat("\n  Exponential Smooth (alpha = 0.6)   :", mad[3])
  cat("\n  Exponential Smooth (alpha = 0.8)   :", mad[4])
  cat("\n  Basic Linear Model (lm())          :", mad[5])
  cat("\n  Polynomial Model (ts.poly())       :", mad[6])
  cat("\n  Trend Analysis Using SI's (csts()) :", mad[7])
  
}