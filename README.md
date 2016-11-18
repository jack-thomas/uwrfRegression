# uwrfRegression

This R package seeks to automate linear regression in the way that it is taught at the University of Wisconsin-River Falls in Math 327 - Applied Regression Analysis.

### Package Build Process

    #install.packages("devtools")
    library("devtools")
    #devtools::install_github("klutometis/roxygen")
    library(roxygen2)

    setwd("~/projects")
    create("uwrfRegression")

    setwd("~/projects/uwrfRegression")
    document()

    setwd("~/projects")
    install("uwrfRegression")
