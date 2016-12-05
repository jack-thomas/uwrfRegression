#' Polynomial Time Series Regression
#'
#' This function performs polynomial time series regression. It returns the
#' polynomial regression with the best adjusted r-squared value.
#' @param x a vector of time series data upon which to perform polynomial regression.
#' @param t a vector of time series sequence information.
#' @keywords ts.poly
#' @export
#' @examples
#' ts.poly()

ts.poly <- function(x,t=NULL){
  if(is.null(t)){
    t <- c(1:length(x))
  }
  #t
  lm.best <- lm(x ~ t)
  r.best <- summary(lm(x ~ t))$adj.r.squared
  for (i in c(2,3)){
    #t^2; t^3
    if (summary(lm(x ~ I(t^i)))$adj.r.squared > r.best){
      lm.best <- lm(x ~ I(t^i))
      r.best <- summary(lm.best)$adj.r.squared
    }
    #t + t^2; t + t^3
    if (summary(lm(x ~ t + I(t^i)))$adj.r.squared > r.best){
      lm.best <- lm(x ~ t + I(t^i))
      r.best <- summary(lm.best)$adj.r.squared
    }
  }
  #t + t^2 + t^3
  if (summary(lm(x ~ t + I(t^2) + I(t^3)))$adj.r.squared > r.best){
    lm.best <- lm(x ~ t + I(t^2) + I(t^3))
    r.best <- summary(lm.best)$adj.r.squared
  }
  #t^2 + t^3
  if (summary(lm(x ~ I(t^2) + I(t^3)))$adj.r.squared > r.best){
    lm.best <- lm(x ~ I(t^2) + I(t^3))
    r.best <- summary(lm.best)$adj.r.squared
  }
  return(lm.best)
}