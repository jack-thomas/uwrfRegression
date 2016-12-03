#' Seasonal Time Series Regression
#'
#' This function performs time series regression with seasonal considerations by computing the
#' following (as per Dr. Chatterjee's instruction in Fall 2016): (centered) moving average,
#' seasonl ratio, raw seasonal indices, normalized seasonal indices, de-seasonalized raw data,
#' de-seasonalized predictions, and re-seasonalized predictions. Optionally, it can carry this
#' forecasting method out past the end of the data set.
#' . It either returns a data frame containing
#' an abundance of information (including the final prediction) or it returns the model.
#' @param ts.data time series data
#' @param year.start the (optional) year in which the data starts
#' @param year.end the (optional) year in which the data ends
#' @param ts.frequency the number of observations per unit of time
#' @keywords csts
#' @export
#' @examples
#' csts()

csts <- function(ts.data = NA, ts.start = NULL, ts.end = NULL, ts.frequency = 1,
                 plot.initial = FALSE, out = c("data.frame", "model"), df.print = FALSE,
                 no.predict = 0){
  #How many elements are in the data?
  no.elts <- length(ts.data)
  #Assign periods if years not assigned
  periods <- c()
  if (is.null(ts.start) && is.null(ts.end)){
    #Both are null
    for (i in c(1:ceiling(no.elts/ts.frequency))){
      periods <- c(periods,rep(i,ts.frequency))
    }
  } else if (is.null(ts.start) || is.null(ts.end)) {
    #WHAT TO DO IF ONLY ONE IS NULL?
  } else {
    #Neither are null
    for (i in c(ts.start:ts.end)){
      periods <- c(periods,rep(i,ts.frequency))
    }
  }
  periods <- periods[c(1:no.elts)]
  
#  ts.relevant <- ts(ts.data, start = c(year.start,1), end = c(year.end, ts.frequency),
#                    frequency = ts.frequency)
  df.relevant <- data.frame(periods,c(1:no.elts),ts.data)
  for (i in c(1:no.elts)){
    df.relevant[i,2] <- i %% ts.frequency
    if (df.relevant[i,2] == 0) df.relevant[i,2] <- ts.frequency
  }
  names(df.relevant) <- c("Period","SubPer","Data")
  if (plot.initial){
    plot(ts.relevant, ylab = "Value", main = "Initial Time Series Plot")
  }
  
  #Moving Average(s)
  if (ts.frequency %% 2 == 0){
    #Even frequencies
    mv.freq <- rollmean(df.relevant[,3],ts.frequency)
    mv.freq <- c(rep(NA,ts.frequency/2),mv.freq)
    mv.freq <- c(mv.freq,rep(NA,length(df.relevant[,3])-length(mv.freq)))
    df.relevant[,4] <- mv.freq
    mv.freq <- rollmean(na.omit(mv.freq),2)
    mv.freq <- c(rep(NA,ts.frequency/2),mv.freq,rep(NA,ts.frequency/2))
    df.relevant[,5] <- mv.freq
    names(df.relevant)[c(4,5)] <- c(paste0(ts.frequency,"MA"),paste0(ts.frequency,"CMA"))
  } else {
    #Odd frequencies
    mv.freq <- rollmean(df.relevant[,3],ts.frequency)
    mv.freq <- c(rep(NA,floor(ts.frequency/2)),mv.freq,rep(NA,floor(ts.frequency/2)))
    df.relevant[,4] <- mv.freq
    names(df.relevant)[4] <- c(paste0(ts.frequency,"MA"))
  }
  
  #Ratio
  df.relevant[,(ncol(df.relevant)+1)] <- df.relevant[,3] / df.relevant[,ncol(df.relevant)]
  names(df.relevant)[ncol(df.relevant)] <- "Ratio"
  
  #Average ratio (each period)
  avgrat <- c()
  avgrat.intermediary <- c()
  for (i in c(1:ts.frequency)){
    avgrat.intermediary <- c()
    for (j in c(1:no.elts)){
      if (df.relevant[j,2] == i){
        avgrat.intermediary <- c(avgrat.intermediary, df.relevant[j,ncol(df.relevant)])
      }
    }
    avgrat.intermediary <- na.omit(avgrat.intermediary)
    avgrat <- c(avgrat,sum(avgrat.intermediary)/length(avgrat.intermediary))
  }
  df.relevant[,ncol(df.relevant)+1] <- c(rep(NA,ts.frequency),avgrat,rep(NA,no.elts-2*ts.frequency))
  names(df.relevant)[ncol(df.relevant)] <- "AvgRatio"
  
  #Normalized Seasonal Indices
  seasonalindices <- (ts.frequency*na.omit(df.relevant[,ncol(df.relevant)]))/sum(na.omit(df.relevant[,ncol(df.relevant)]))
  df.relevant[,ncol(df.relevant)+1] <- rep(seasonalindices,no.elts/ts.frequency)
  names(df.relevant)[ncol(df.relevant)] <- "SI"
  
  #Deseasonalize
  df.relevant[,ncol(df.relevant)+1] <- df.relevant[,3]/df.relevant[,ncol(df.relevant)]
  names(df.relevant)[ncol(df.relevant)] <- "DeS"
  
  #Model
  lm.relevant <- lm(df.relevant[,ncol(df.relevant)]~c(1:no.elts))
  df.relevant[,ncol(df.relevant)+1] <- fitted(lm.relevant)
  names(df.relevant)[ncol(df.relevant)] <- "Forecast"
  
  #Reseasonalize
  df.relevant[,ncol(df.relevant)+1] <- df.relevant[,ncol(df.relevant)-2] * df.relevant[,ncol(df.relevant)]
  names(df.relevant)[ncol(df.relevant)] <- "ReS"
  
  #Predictions (if requested)
  if (no.predict != 0){
    predictions <- c()
    for (i in c(1:no.predict)){
      df.relevant[(no.elts+i),] <- c(rep(NA, ncol(df.relevant)))
      df.relevant[(no.elts+i),2] <- ifelse(i %% ts.frequency == 0, ts.frequency, i %% ts.frequency)
      df.relevant[(no.elts+i),(ncol(df.relevant)-3)] <- seasonalindices[df.relevant[(no.elts+i),2]]
      df.relevant[(no.elts+i),(ncol(df.relevant)-1)] <- as.numeric(lm.relevant$coefficients[1]+lm.relevant$coefficients[2]*(no.elts+i))
      df.relevant[(no.elts+i),ncol(df.relevant)] <- df.relevant[(no.elts+i),(ncol(df.relevant)-3)] * df.relevant[(no.elts+i),(ncol(df.relevant)-1)]
    }
    #Add periods
    newperiods <- c()
    for (i in c(1:(ceiling(no.elts + no.predict)/ts.frequency))){
      newperiods <- c(newperiods,rep(df.relevant[no.elts,1]+i, ts.frequency))
    }
    df.relevant[c((no.elts+1):(no.elts+no.predict)),1] <- newperiods[c(1:no.predict)]
  }
  
  
  #Print Data Frame (if requested)
  if (df.print) print(df.relevant)
  
  #Return Requested
  if (out[1] == "model"){
    invisible(lm.relevant)
  } else {
    if (out[1] != "data.frame"){
      cat("Invalid out specified. Assuming data frame.")
    }
    invisible(df.relevant)
  }
}