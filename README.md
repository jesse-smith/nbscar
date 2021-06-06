
# nbscar <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/nbscar)](https://CRAN.R-project.org/package=nbscar)
[![Codecov test coverage](https://codecov.io/gh/jesse-smith/nbscar/branch/master/graph/badge.svg)](https://codecov.io/gh/jesse-smith/nbscar?branch=master)
[![R-CMD-check](https://github.com/jesse-smith/nbscar/workflows/R-CMD-check/badge.svg)](https://github.com/jesse-smith/nbscar/actions)
<!-- badges: end -->

WORK IN PROGRESS. nbscar is an R package for interfacing with the TN NBS system. It uses the
[webdriver](https://github.com/rstudio/webdriver) package to programmatically
operate the NBS UI. This package is primarily designed for use within the
Shelby County Health Department, but contributions and modifications for use in
other settings are welcome.

## Installation

You can install the development version of nbscar with:

``` r
# install.packages('remotes')
remotes::install_github('jesse-smith/nbscar')
```

## Code of Conduct
  
Please note that the nbscar project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
