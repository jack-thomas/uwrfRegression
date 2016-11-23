#' Chatterjee Multiple Linear Regression
#'
#' Performs multiple linear regression (with transformations) using
#' the methodology taught by Dr. Arunendu Chatterjee for Fall 2016.
#' @param x a data frame for regression, where the first column is the dependent variable.
#' @param vif.lim the maximum vif allowed when removing variables due to multicollinearity.
#' @param plot.out a boolen value indicating whether to provide diagnostic plots.
#' @param trace a nonnegative integer [0, 2], indicating how much diagnostic and processing
#' information to print as the function runs. Specifically, 0 will print no information,
#' 1 will print the initial and final models, and 2 will print everything.
#' @param dir the direction in which you want to do stepwise model selection.
#' @keywords cmlr
#' @export
#' @examples
#' cmlr()

cmlr<-function(x, vif.lim = 10, plot.out = FALSE, trace = 2, dir = "both"){
  model <- "x[,1] ~ "
  for (i in c(2:ncol(x))){
    if (i == ncol(x)){
      model <- paste0(model,"x[,",i,"]")
    } else {
      model <- paste0(model,"x[,",i,"] + ")
    }
  }
  model <- as.formula(model)
  lm.model <- lm(model)

  if (trace > 0){
    cat("**************** Original Full Model:\n")
    print(lm.model$coefficients)
  }
  
  p.bp <- bptest(lm.model)$p.value
  p.sw <- shapiro.test(residuals(lm.model))$p.value
  
  if (trace > 1){
    cat("\n**************** Results of Assumption Checks on Original Full Model: \n")
    cat("1.",ifelse(p.bp>0.05,"Passed","Failed"),
        "Breusch-Pagan Test -- p-value :",p.bp,"\n",
        ifelse(p.bp>0.05,
               "    Fail to Reject ",
               "            Reject "
        ),"H_0 : Error variance is constant \n"
        ,"                    H_a : Error variance is not constant \n"
    )
    cat("2.",ifelse(p.sw>0.05,"Passed","Failed"),
        "Shapiro-Wilk Test  -- p-value :",p.sw,"\n",
        ifelse(p.sw>0.05,
               "    Fail to Reject ",
               "            Reject "
        ),"H_0 : The residuals are normally distributed \n"
        ,"                    H_a : The residuals are not normally distributed \n"
    )
  }
  
  if (trace > 1){
    cat("\n**************** Correlation Matrix on Original Full Model: \n")
    print(cor(x))
  }
  
  #PROVIDE QQ-PLOT OF ORIGINAL FULL MODEL (if plot.out = TRUE)
  if (plot.out){
    plot(x)
    qqnorm(residuals(lm.model), main = "Normal Q-Q Plot for Original Full Model")
    qqline(residuals(lm.model))
  }
  
  #TRANSFORMATIONS ON X
  #WHEN DO WE TRANSFORM X? WHEN IT'S NON LINEAR...
  #OR JUST TRY ALL COMBINATIONS OF TRANSFORMATIONS?
  
  #TRANSFORMATIONS ON Y (WHERE APPLICABLE)
  if (trace > 1){
    cat("\n**************** Transformations:\n")
  }
  if (p.bp < 0.05 || p.sw < 0.05){
    for (i in c(1:3)){
      #RECREATE THE MODEL WITH TRANSFORMATIONS
      if (i == 1) {tmodel <- "I(log(x[,1])) ~ "}
      if (i == 2) {tmodel <- "I(sqrt(x[,1])) ~ "}
      if (i == 3) {tmodel <- "I(1/(x[,1])) ~ "}
      for (j in c(2:ncol(x))){
        if (j == ncol(x)){
          tmodel <- paste0(tmodel,"x[,",j,"]")
        } else {
          tmodel <- paste0(tmodel,"x[,",j,"] + ")
        }
      }
      tmodel <- as.formula(tmodel)
      lm.tmodel <- lm(tmodel)
      p.bp <- bptest(lm.tmodel)$p.value
      p.sw <- shapiro.test(residuals(lm.tmodel))$p.value
      if(p.bp > 0.05 && p.sw > 0.05){
        lm.model <- lm(tmodel)
        if (trace > 0) {cat("ACCEPTED:",gsub(" ","",deparse(tmodel)),"\n")}
        break
      } else {
        if (trace > 0) {cat("REJECTED:",gsub(" ","",deparse(tmodel)),"\n")}
      }
      if (trace > 0) {cat("  Shapiro-Wilk Test p-value  :",p.sw,"\n")}
      if (trace > 0) {cat("  Breusch-Pagan Test p-value :",p.bp,"\n")}
    }
  }
  
  #MODEL SELECTION
  step <- stepAIC(lm.model, direction = dir, trace = 0)
  lm.model <- lm(step$model)
  if (trace > 1){
    cat("\n**************** Final Model After Model Selection:\n")
    cat("Direction :",dir,"\n\n")
    print(lm.model$coefficients)
  }
  
  #MULTICOLLINEARITY
  if (trace > 1){
    cat("\n**************** Multicollinearity:")
    cat("\nVIF Before Removals :",VIF(lm.model))
  }
  xprime <- step$model
  while (vif(xprime)[1,2] > vif.lim){
    xprime <- xprime[,c(-which.max(vif(xprime)[c(2:ncol(xprime)),2])-1)]
  }
  lm.model <- lm(xprime)
  if (trace > 1){
    cat("\nVIF After Removals  :",VIF(lm.model))
  }
  
  #FINAL MODEL
  if (trace > 0){
    cat("\n\n**************** Final Model:\n")
    print(lm.model$coefficients)
    if (trace > 1){
      cat("\n**************** Summary of Final Model:\n")
      print(summary(lm.model)) 
    }
  }
  
  #PREDICTION INTERVAL FOR FINAL MODEL
  invisible(lm.model)
}


