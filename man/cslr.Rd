% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cslr.R
\name{cslr}
\alias{cslr}
\title{Chatterjee Simple Linear Regression}
\usage{
cslr(x, y, matrix.out = F, asm.out = F, anova.out = F, shapiro.out = F,
  bp.out = F, summary.out = F, plot.out = T, conf.out = F,
  conf.level = 0.95)
}
\arguments{
\item{x}{a vector of x values (representing the independent variable).}

\item{y}{a vector of y values (representing the dependent variable).}

\item{matrix.out}{whether to provide a matrix of adjusted R-squared values}

\item{asm.out}{whether to provide results of assumption checking.}

\item{anova.out}{whether to provide ANOVA results.}

\item{shapiro.out}{whether to provide results of the Shapiro-Wilk test.}

\item{bp.out}{whether to provide results of the Breusch-Pagan test.}

\item{summary.out}{whether to provide summary of best regression.}

\item{plot.out}{whether to provide relevant plots.}

\item{conf.out}{whether to create confidence intervals for coefficients.}

\item{conf.level}{confidence level for conf.out.}
}
\description{
Performs simple linear regression (with transformations) using the methodology
taught by Dr. Arunendu Chatterjee for Fall 2016.
}
\examples{
cslr()
}
\keyword{cslr}

