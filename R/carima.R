#' Chatterjee Auto-Regressive Modeling
#'
#' This function models autoregressive processes as described by Dr. 
#' Arunendu Chatterjee (2016.12.09).
#' @param data a vector of time series data.
#' @param silent whether or not to silence output.
#' @param no.predict number of predictions to make.
#' @keywords carima
#' @export
#' @examples
#' carima()

carima <- function(data, silent = FALSE, no.predict = 0){
  boxp <- Box.test(data, type = "Ljung-Box")$p.value
  if(boxp > 0.05){
    cat(paste0("Not an autoregressive process. ",
               "Ljung-Box test yielded p-value of ",
               round(boxp,2),".\n"))
  } else {
    acf(data)
    max <- readline("Based on the ACF, what is the max AR(k) to consider? ")
    while(is.na(max) | max <= 0){
      max <- readline("Please try again: ")
      max <- ifelse(grepl("[^0-9]",max),-1,as.numeric(max))
    }
    loglikinfo <- c()
    aicinfo <- c()
    suggests <- 1
    ar <- arima(data, order = c(1,0,0))
    for (i in c(1:max)){
      ar <- arima(data, order = c(i, 0, 0))
      loglikinfo[i] <- ar$loglik
      aicinfo[i] <- ar$aic
      if (i > 1){
        if (loglikinfo[i] > loglikinfo[i-1] && aicinfo[i] < aicinfo[i-1]){
          suggests <- i
        }
      }
    }
    out <- data.frame(loglikinfo, aicinfo)
    names(out) <- c("loglik","aic")
    if(!silent){
      print(out)
      cat(paste0("\nSuggested: AR(",suggests,")"))
    }
    ar.relevant <- arima(data, order = c(suggests, 0, 0))
    if(no.predict != 0){
      predictions <- c(predict(ar.relevant, n.ahead = 5)$pred)
      if(!silent) cat("\n\n")
      print(cbind(predictions))
    }
    invisible(ar.relevant)
  }
}