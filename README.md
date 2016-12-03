# uwrfRegression

This R package seeks to automate regression in the way that it is taught at the University of Wisconsin-River Falls in Math 327 - Applied Regression Analysis. I used [this](https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/) as a reference for how to create the package, and I used [this](http://kbroman.org/pkg_primer/pages/github.html) as a reference for how to put it on GitHub. [Here](https://cran.r-project.org/doc/manuals/R-exts.html) is the official manual for creating R packages.

DISCLAIMER: This package will not, and likely cannot, cover every possible scenario. It is, as such, not intended to replace the content of the class. Rather, it is intended as a supplemental method of answer validation.

## How to Install This Package

    library(devtools)
    install_github("thecrosbyfan/uwrfRegression")

## Functions In This Package

The following packages are included as part of this package. Please note that some of them are not yet completed.

- ``cmlr()``, multiple linear regression.
- ``cslr()``, simple linear regression.
- ``csts()``, which does seasonal time series stuff.
- ``eSmooth()``, exponential smoothing for time series.
- ``which.poly``, polynomial time series regression.

## Dependencies
The dependencies for this package include the following libraries (all of which are available through CRAN):

- lmtest
- fmsb
- usdm
- MASS
- xts
