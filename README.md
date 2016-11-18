# slr_fitme

### Description
This function finds the best linear model transformations for x and y while checking that none of our assumptions have been violated. It is configurable to allow the user to control what output is generated in regard to the results of the model and the assumption tests.


### Usage
    fitme(x,y,matrix.out=FALSE,asm.out=FALSE,anova.out=FALSE,
      shapiro.out=FALSE,bp.out=FALSE,summary.out=FALSE,plot.out=TRUE)

### Arguments

Argument | Definition
--- | ---
x | A vector specifying the values of the independent variable.
y | A vector specifying the values of the dependent variable.
matrix.out | A logical value indicating whether to output a matrix of the adjusted R-squared values for each combination of transformations
asm.out | A logical value indicating whether to output the results of the assumption tests and their p-values.
anova.out | A logical value indicating whether to output the results of the ANOVA test.
shapiro.out | A logical value indicating whether to output the results of the Shapiro-Wilk test.
bp.out | A logical value indicating whether to output the results of the Breusch-Pagan test.
summary.out | A logical value indicating whether to output a summary of the resulting linear model.
plot.out | A logical value indicating whether to provide a plot of x, y, and the linear model; a plot of residuals; and a Q-Q normality plot.

<!---### Details--->
