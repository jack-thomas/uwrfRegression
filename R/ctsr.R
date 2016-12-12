#' Chatterjee Time Series Regression
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
#  5. Basic linear model (lm)
  mad[5] <- mean(abs(data - fitted(lm(data ~ t))))
#  6. x ~ t^2
  mad[6] <- mean(abs(data - fitted(lm(data ~ I(t^2)))))
#  7. x ~ t^3  
  mad[7] <- mean(abs(data - fitted(lm(data ~ I(t^3)))))
#  8. x ~ t + t^2
  mad[8] <- mean(abs(data - fitted(lm(data ~ t + I(t^2)))))
#  9. x ~ t + t^3
  mad[9] <- mean(abs(data - fitted(lm(data ~ t + I(t^3)))))
# 10. x ~ t^2 + t^3
  mad[10] <- mean(abs(data - fitted(lm(data ~ I(t^2) + I(t^3)))))
# 11. x ~ t + t^2 + t^3
  mad[11] <- mean(abs(data - fitted(lm(data ~ t + I(t^2) + I(t^3)))))
# 12. Trend analysis using seasonal indices (csts)
  mad[12] <- csts(data, frequency = frequency, mad = TRUE, df.print = FALSE)
  
  cat("MAD Values:")
  cat(sprintf("\n  %-34s : %5.2f", "Exponential Smoothing (w = 0.2)", mad[1]))
  cat(sprintf("\n  %-34s : %5.2f", "Exponential Smoothing (w = 0.4)", mad[2]))
  cat(sprintf("\n  %-34s : %5.2f", "Exponential Smoothing (w = 0.6)", mad[3]))
  cat(sprintf("\n  %-34s : %5.2f", "Exponential Smoothing (w = 0.8)", mad[4]))
  cat(sprintf("\n  %-34s : %5.2f", "lm(x ~ t)",mad[5]))
  cat(sprintf("\n  %-34s : %5.2f", "lm(x ~ t^2)",mad[6]))
  cat(sprintf("\n  %-34s : %5.2f", "lm(x ~ t^3)",mad[7]))
  cat(sprintf("\n  %-34s : %5.2f", "lm(x ~ t + t^2)",mad[8]))
  cat(sprintf("\n  %-34s : %5.2f", "lm(x ~ t + t^3)",mad[9]))
  cat(sprintf("\n  %-34s : %5.2f", "lm(x ~ t^2 + t^3)",mad[10]))
  cat(sprintf("\n  %-34s : %5.2f", "lm(x ~ t + t^2 + t^3)",mad[11]))
  cat(sprintf("\n  %-34s : %5.2f", paste0("csts(data, frequency = ", frequency, ")"),mad[12]))
}