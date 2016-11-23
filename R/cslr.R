#' Chatterjee Simple Linear Regression
#'
#' Performs simple linear regression (with transformations) using the methodology
#' taught by Dr. Arunendu Chatterjee for Fall 2016.
#' @param x a vector of x values (representing the independent variable).
#' @param y a vector of y values (representing the dependent variable).
#' @param matrix.out whether to provide a matrix of adjusted R-squared values
#' @param asm.out whether to provide results of assumption checking.
#' @param anova.out whether to provide ANOVA results.
#' @param shapiro.out whether to provide results of the Shapiro-Wilk test.
#' @param bp.out whether to provide results of the Breusch-Pagan test.
#' @param summary.out whether to provide summary of best regression.
#' @param plot.out whether to provide relevant plots.
#' @param conf.out whether to create confidence intervals for coefficients.
#' @param conf.level confidence level for conf.out.
#' @keywords cslr
#' @export
#' @examples
#' cslr()

cslr<-function(x,y,matrix.out=F,asm.out=F,anova.out=F,shapiro.out=F,bp.out=F,
                summary.out=F,plot.out=T,conf.out=F,conf.level=0.95){
  ##Create a data frame of the data and possible transformations
  df.func=data.frame(x,y)
  df.func[,3]=x^2
  df.func[,4]=sqrt(x)
  df.func[,5]=1/x
  df.func[,6]=log(x)
  df.func[,7]=y^2
  df.func[,8]=sqrt(y)
  df.func[,9]=1/y
  df.func[,10]=exp(y)
  df.func=df.func[c(1,3,4,5,6,2,7,8,9,10)]
  names(df.func)<-c('x','x^2','sqrt(x)','1/x','log(x)','y','y^2','sqrt(y)','1/y','log(y)')
  ##Compute r.squared
  mx=matrix(nrow=6,ncol=6)
  mx[2:6,1]=names(df.func)[6:10]
  mx[1,2:6]=names(df.func)[1:5]
  for(i in c(1:5)){
    for(j in c(1:5)){
      if(identical(intersect(c(df.func[,j],df.func[,i+5]),c(Inf,-Inf)),numeric(0))){
        mx[i+1,j+1]=round(summary(lm(df.func[,i+5]~df.func[,j]))$adj.r.squared,4)
        ##CHECK ASSUMPTION 1: We can fit a regression line
        ##ANOVA Test
        ##H_0: b_0 = b_1 = 0
        if(anova(lm(df.func[,i+5]~df.func[,j]))$`Pr(>F)`[1]>0.05) mx[i+1,j+1]<-0
        ##CHECK ASSUMPTION 2: Residuals (error terms) are normally distributed
        ##Shapiro-Wilk Normality Test
        if(shapiro.test(residuals(lm(df.func[,i+5]~df.func[,j])))$p.value<0.05) mx[i+1,j+1]<-0
        ##CHECK ASSUMPTION 3: Error terms have constant variance
        ##Breuch-Pagan Test (Replacement for Levene's Test)
        if(bptest(lm(df.func[,i+5]~df.func[,j]))$p.value<0.05) mx[i+1,j+1]<-0
      } else mx[i+1,j+1]<-0
    }
  }
  maxrsq=c(mx[2:6,2:6])[which.max(mx[2:6,2:6])]
  location=which(mx==maxrsq,arr.ind=TRUE)
  xtrans=mx[1,location[1,2]]
  ytrans=mx[location[1,1],1]
  ##Matrix output is optional (provide matrix.out=TRUE to show)
  if(matrix.out){
    cat("**************** Transformation Comparison Matrix: \n")
    print(mx)
    cat("\n")
  }
  xcol=which(xtrans==names(df.func))
  ycol=which(ytrans==names(df.func))
  lm.relevant=lm(df.func[,ycol]~df.func[,xcol])
  ##Assumption-checking output is optional (provide asm.out=TRUE to show
  if(asm.out){
    cat("**************** Results of Assumption Checks: \n")
    cat("  1 .",ifelse(anova(lm.relevant)$`Pr(>F)`[1]<0.05,"Passed","Failed"),
        "ANOVA Test         -- p-value :",anova(lm.relevant)$`Pr(>F)`[1],"\n",
        ifelse(anova(lm.relevant)$`Pr(>F)`[1]<0.05,
          "       Reject         ",
          "       Fail to Reject "
        ),"H_0 : beta_0 = beta_1 = 0 \n"
        ,"                       H_a : At least one inequality \n"
        )
    cat("  2 .",ifelse(shapiro.test(residuals(lm.relevant))$p.value>0.05,"Passed","Failed"),
        "Shapiro-Wilk Test  -- p-value :",shapiro.test(residuals(lm.relevant))$p.value,"\n",
        ifelse(shapiro.test(residuals(lm.relevant))$p.value>0.05,
          "       Fail to Reject ",
          "               Reject "
        ),"H_0 : The residuals are normally distributed \n"
        ,"                       H_a : The residuals are not normally distributed \n"
        )
    cat("  3 .",ifelse(bptest(lm.relevant)$p.value>0.05,"Passed","Failed"),
        "Breusch-Pagan Test -- p-value :",bptest(lm.relevant)$p.value,"\n",
        ifelse(bptest(lm.relevant)$p.value>0.05,
          "       Fail to Reject ",
          "               Reject "
        ),"H_0 : Error variance is constant \n"
        ,"                       H_a : Error variance is not constant \n"
    )
    cat("  4a.",ifelse(summary(lm.relevant)$coefficients[1,4]<0.05,"Passed","Failed"),
        "beta_0 Test        -- p-value :",summary(lm.relevant)$coefficients[1,4],"\n",
        ifelse(anova(lm.relevant)$`Pr(>F)`[1]<0.05,
               "       Reject         ",
               "       Fail to Reject "
        ),"H_0 : beta_0 = 0 \n"
        ,"                       H_a : beta_0 <> 0 \n")
    cat("  4b.",ifelse(summary(lm.relevant)$coefficients[2,4]<0.05,"Passed","Failed"),
        "beta_1 Test        -- p-value :",summary(lm.relevant)$coefficients[2,4],"\n",
        ifelse(anova(lm.relevant)$`Pr(>F)`[1]<0.05,
               "       Reject         ",
               "       Fail to Reject "
        ),"H_0 : beta_1 = 0 \n"
        ,"                       H_a : beta_1 <> 0 \n \n")
  }
  ##MAIN OUTPUT (NOT OPTIONAL)
  cat("**************** Best Fit:",ytrans,"~",xtrans)
  cat("\nAdjusted R-Squared:",maxrsq)
  cat("\n  b_0 =",round(lm.relevant$coefficients,6)[1])
  cat("\n  b_1 =",round(lm.relevant$coefficients,6)[2])
  cat("\n \n")
  ##ANOVA output is optional (provide anova.out=TRUE to show)
  if(anova.out){
    cat("**************** ANOVA Results:")
    print(anova(lm.relevant))
    cat("\n")
  }
  ##Shapiro-Wilk output is optional (provide shapiro.out=TRUE to show)
  if(shapiro.out){
    cat("**************** Shapiro-Wilk Test Results:")
    print(shapiro.test(residuals(lm.relevant)))
  }
  ##Breusch-Pagan output is optional (provide bp.out=TRUE to show)
  if(bp.out){
    cat("**************** Breusch-Pagan Test Results:")
    print(bptest(lm.relevant))
  }
  ##Summary output is optional (provide summary.out=TRUE to show)
  if(summary.out){
    cat("**************** Summary of Results:")
    print(summary(lm.relevant))
  }
  ##Confidence interval is optional (provide conf.out=TRUE to show)
  if(conf.out){
    mx.c=confint(lm.relevant,level=conf.level)
    cat("**************** ",100*conf.level,"% Confidence Interval: \n")
    cat("  beta_0 : [",mx.c[1,1],",",mx.c[1,2],"] \n")
    cat("  beta_1 : [",mx.c[2,1],",",mx.c[1,2],"] \n \n")
  }
  ##Plot output is optional (provide plot.out=FALSE to suppress)
  if(plot.out){
    plot(df.func[,xcol],df.func[,ycol],
         main=paste("Scatter Plot of",ytrans,"~",xtrans),
         ylab=ytrans,xlab=xtrans)
    abline(lm.relevant)
    plot(residuals(lm.relevant),main=paste("Residual Plot for",ytrans,"~",xtrans))
    abline(h=0)
    qqnorm(residuals(lm.relevant),main=paste("Normal Q-Q Plot for",ytrans,"~",xtrans))
    qqline(residuals(lm.relevant))
  }
}