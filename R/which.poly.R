#' Polynomial Regression
#'
#' This function performs time series polynomial regression. It returns the
#' polynomial regression with the best adjusted r-squared value.
#' @param x a vector of time series data upon which to perform polynomial regression
#' @param t a vector of time series sequence information
#' @param degree.max the maximum degree to consider for polynomial regression
#' @keywords which.poly
#' @export
#' @examples
#' which.polt()

which.poly <- function(x,t=NULL,degree.max=10){
  if(is.null(t)){
    t <- c(1:length(x))
  }
  poly.r.squareds=c()
  for (i in c(1:degree.max)){
    poly.r.squareds[i] = summary(lm(x ~ poly(t,i, raw=TRUE)))$adj.r.squared
  }
  poly.best=which.max(poly.r.squareds)
  cat("Polynomial Regression Results:")
  cat("\n  - Best Polynomial Degree :",which.max(poly.r.squareds))
  cat("\n  - Best Adjusted R-Squared:",
      summary(lm(x ~ poly(t, poly.best, raw=TRUE)))$adj.r.squared)
  invisible(lm(x ~ poly(t, poly.best, raw=TRUE)))
}