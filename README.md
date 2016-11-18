# uwrfRegression

This R package seeks to automate regression in the way that it is taught at the University of Wisconsin-River Falls in Math 327 - Applied Regression Analysis. [This blog post](https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/) was used as a guide for how to create this R package.

## How to Install This Package

    library(devtools)
    install_github("thecrosbyfan/uwrfRegression")

### Initial Package Build Essentials

    #install.packages("devtools")
    library(devtools)
    #devtools::install_github("klutometis/roxygen")
    library(roxygen2)

    setwd("~/projects")
    create("uwrfRegression")

    setwd("~/projects/uwrfRegression")
    document()

    setwd("~/projects")
    install("uwrfRegression")

### Iterative Development Essentials

    library(devtools)
    library(roxygen2)
    setwd("~/projects/uwrfRegression")
    document()
    setwd("../")
    install("uwrfRegression")

### TO DO

- Update ``cslr()`` so that it provides the output of ``lm(y~x)`` regardless of whether it's the best transformation or not. Add a boolean input, ``simple`` (default ``TRUE``), that determines whether to provide ``lm(y~x)`` output. Also, add a boolean input that determines whether to try transformations at all. Earlier in the semester, we didn't use transformations at all.
- Create ``cmlr()``, which automates multiple linear regression. It should be able to check assumptions (including multicollinearity) and do model selection.
- Create ``ctsr()``, which automates time series regression. It should be able to do moving averages, scaling, etc.
- Look at Group Assignments 2-7, and create functions to automate each of those.
- Figure out dependencies. The ``lmtest`` package is certainly one.
- Add stuff to the ``DESCRIPTION`` file. [This](https://github.com/klutometis/roxygen/blob/master/DESCRIPTION) is a fine example.
