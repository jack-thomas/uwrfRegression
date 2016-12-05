#' Seasonal Time Series Regression
#'
#' This function performs time series regression with seasonal considerations by computing the
#' following (as per Dr. Chatterjee's instruction in Fall 2016): (centered) moving average,
#' seasonl ratio, raw seasonal indices, normalized seasonal indices, de-seasonalized raw data,
#' de-seasonalized predictions, and re-seasonalized predictions. Optionally, it can carry this
#' forecasting method out past the end of the data set.
#' . It either returns a data frame containing
#' an abundance of information (including the final prediction) or it returns the model.
#' @param data time series data.
#' @param start	the time of the first observation. Either a single number or a vector of two
#' integers, which specify a natural time unit and a (1-based) number of samples into the time
#' unit. See the examples for the use of the second form.
#' @param end	the time of the last observation, specified in the same way as start.
#' @param frequency the number of observations per unit of time.
#' @param plot.initial a boolean indicating whether you want a plot of the initial time series.
#' @param out whether you want a data frame (out = "data.frame") or the deseasonalized linear
#' model (out = "model") to be provided as output.
#' @param results.print a boolean value indicating whether you want the data frame to be printed.
#' @param no.predict the number of predictions to perform.
#' @keywords csts
#' @export
#' @examples
#' csts()

csts <- function(data, start = NULL, end = NULL, frequency = 1,
                 plot.initial = FALSE, out = c("data.frame", "model"),
                 results.print = FALSE, no.predict = 0){
  #How many elements are in the data?
  no.elts <- length(data)
  
  #Create a time series
  if (is.null(start)) start <- c(1, 1)
  if (is.null(end)) end <- c(ceiling(no.elts / frequency), no.elts %% frequency)
  if (end[2] == 0) end[2] <- frequency
  ts.relevant <- ts(data, start = start, end = end, frequency = frequency)
  
  #Plot the initial time series, if requested
  if (plot.initial) plot(ts.relevant)
  
  #Create a data frame.
  df.relevant <- data.frame(start[1], start[2], data[1])
  for (i in c(2:no.elts)){
    df.relevant[i,] <- c(df.relevant[i-1,1], df.relevant[i-1,2]+1, data[i])
    if (df.relevant[i, 2] > frequency){
      df.relevant[i, 2] <- 1
      df.relevant[i, 1] <- df.relevant[i-1, 1] + 1
    }
  }
  names(df.relevant) <- c("Period","SubPer","Data")
  
  #Moving Average(s)
  if (frequency %% 2 == 0){
    #Even frequencies
    mv.freq <- rollmean(df.relevant[,3],frequency)
    mv.freq <- c(rep(NA,frequency/2),mv.freq)
    mv.freq <- c(mv.freq,rep(NA,length(df.relevant[,3])-length(mv.freq)))
    df.relevant[,4] <- mv.freq
    mv.freq <- rollmean(na.omit(mv.freq),2)
    mv.freq <- c(rep(NA,frequency/2),mv.freq,rep(NA,frequency/2))
    df.relevant[,5] <- mv.freq
    names(df.relevant)[c(4,5)] <- c(paste0(frequency,"MA"),paste0(frequency,"CMA"))
  } else {
    #Odd frequencies
    mv.freq <- rollmean(df.relevant[,3],frequency)
    mv.freq <- c(rep(NA,floor(frequency/2)),mv.freq,rep(NA,floor(frequency/2)))
    df.relevant[,4] <- mv.freq
    names(df.relevant)[4] <- c(paste0(frequency,"MA"))
  }
  
  #Ratio
  df.relevant[,(ncol(df.relevant)+1)] <- df.relevant[,3] / df.relevant[,ncol(df.relevant)]
  names(df.relevant)[ncol(df.relevant)] <- "Ratio"
  
  #Average ratio (each period)
  avgrat <- c()
  avgrat.intermediary <- c()
  for (i in c(1:frequency)){
    avgrat.intermediary <- c()
    for (j in c(1:no.elts)){
      if (df.relevant[j,2] == i){
        avgrat.intermediary <- c(avgrat.intermediary, df.relevant[j,ncol(df.relevant)])
      }
    }
    avgrat.intermediary <- na.omit(avgrat.intermediary)
    avgrat <- c(avgrat,sum(avgrat.intermediary)/length(avgrat.intermediary))
  }
  df.relevant[,ncol(df.relevant)+1] <- NA
  firstrow <- min(which(df.relevant[,2]==1)) + frequency
  lastrow <- min(which(df.relevant[c(firstrow:no.elts),2]==frequency)) + firstrow - 1
  df.relevant[c(firstrow:lastrow),ncol(df.relevant)] <- avgrat
  names(df.relevant)[ncol(df.relevant)] <- "AvgRatio"

  #Normalized Seasonal Indices
  seasonalindices <- (frequency*avgrat)/sum(avgrat)
  df.relevant[,ncol(df.relevant)+1] <- seasonalindices[df.relevant[,2]]
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
  
  #Square Error
  df.relevant[,ncol(df.relevant)+1] <- (df.relevant[,3] - df.relevant[,ncol(df.relevant)])^2
  names(df.relevant)[ncol(df.relevant)] <- "SqError"
  
  #Predictions (if requested)
  if (no.predict != 0){
    predictions <- c()
    for (i in c(1:no.predict)){
      df.relevant[(no.elts+i),] <- c(rep(NA, ncol(df.relevant)))
      df.relevant[(no.elts+i),1] <- df.relevant[(no.elts+i-1),1]
      df.relevant[(no.elts+i),2] <- df.relevant[(no.elts+i-1),2] + 1
      if (df.relevant[(no.elts+i),2] > frequency){
        df.relevant[(no.elts+i),1] <- df.relevant[(no.elts+i-1),1] +1
        df.relevant[(no.elts+i),2] <- 1
      }
      df.relevant[(no.elts+i),(ncol(df.relevant)-3)] <- seasonalindices[df.relevant[(no.elts+i),2]]
      df.relevant[(no.elts+i),(ncol(df.relevant)-1)] <- as.numeric(lm.relevant$coefficients[1]+lm.relevant$coefficients[2]*(no.elts+i))
      df.relevant[(no.elts+i),ncol(df.relevant)] <- df.relevant[(no.elts+i),(ncol(df.relevant)-3)] * df.relevant[(no.elts+i),(ncol(df.relevant)-1)]
    }
    #Add periods
    newperiods <- c()
    for (i in c(1:(ceiling(no.elts + no.predict)/frequency))){
      newperiods <- c(newperiods,rep(df.relevant[no.elts,1]+i, frequency))
    }
    df.relevant[c((no.elts+1):(no.elts+no.predict)),1] <- newperiods[c(1:no.predict)]
  }
  
  #Print Data Frame (if requested)
  if (results.print){
    print(df.relevant)
    cat("\nRoot Mean Square Error:",sqrt(mean(df.revenue[,ncol(df.revenue)])))
  }
  
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