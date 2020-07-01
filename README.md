# cre

`cre` is an R package for fitting various interpretations of the Constant Rate Effect (CRE; Kroch 1989) to historical linguistic data. It is in active development and the currently available versions are not to be considered entirely stable (but versions in the master branch of this repository are stabler than those in the dev branch).


## Installation

With devtools:

``` r
devtools::install_github("hkauhanen/cre")
```


## Basic usage

A package vignette is in preparation. In the meantime, the fitting routine based on nonlinear least squares (Kauhanen & Walkden 2018) can be operated as follows (see the folder [`inst/extdata`](inst/extdata) for a mock random data set in various formats):

``` r
df <- read.csv("inst/extdata/mockdata_long.csv")
fit <- fit.cre.nls(df, format="long", model="logistic", budget=100)
```

Setting `model="logistic"` fits the classical model of logistics agreeing in their slope (Kroch 1989); setting `model="bias"` fits the production bias model (Kauhanen & Walkden 2018); setting `model="VRE"` fits a family of logistics freely varying in slopes and intercepts. See the [manual](cre.pdf) for details.

The result, `fit`, is a list of five elements: the data set used, the best-fitting parameters found, the residual sum of squares, the residual sum of squares normalized by number of data points, and the number of data points.


## Alternative input formats

In addition to long format, the data can be a wide-format table of frequencies or a data frame of (binary) response-level data. See again [`inst/extdata`](inst/extdata) for examples. The following all lead to the same outcome:

``` r
# long-format frequency data
df <- read.csv("inst/extdata/mockdata_long.csv")
fit <- fit.cre.nls(df, format="long", model="logistic", budget=100)

# wide-format frequency data
df <- read.csv("inst/extdata/mockdata_wide.csv")
fit <- fit.cre.nls(df, format="wide", model="logistic", budget=100)

# response-level data
df <- read.csv("inst/extdata/mockdata_responses.csv")
df <- frequentize(df)
fit <- fit.cre.nls(df, format="wide", model="logistic", budget=100)
```


## Issues?

If you find a bug, please file an [issue](https://github.com/hkauhanen/cre/issues). If you have a feature request, please consider emailing Henri: [henri@henr.in](mailto:henri@henr.in).


## References

Kauhanen, H. & Walkden, G. (2018) Deriving the Constant Rate Effect. *Natural Language & Linguistic Theory*, 36(2), 483–521. <https://doi.org/10.1007/s11049-017-9380-1>

Kroch, A. (1989) Reflexes of grammar in patterns of language change. *Language Variation and Change*, 1: 199–244. 
