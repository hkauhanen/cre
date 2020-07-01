# cre

`cre` is an R package for fitting various interpretations of the Constant Rate Effect (CRE; Kroch 1989) to historical linguistic data. It is in active development and the currently available versions are not to be considered entirely stable (but versions in the master branch of this repository are stabler than those in the dev branch).


## Installation

With devtools:

```{r}
devtools::install_github("hkauhanen/cre")
```


## Basic usage

A package vignette is in preparation. In the meantime, the fitting routine based on nonlinear least squares (Kauhanen & Walkden 2018) can be operated as follows (see the folder `inst/extdata` for a mock random data set in various formats):

```{r}
df <- read.csv("inst/extdata/mockdata_long.csv")
fit <- fit.cre.nls(df, format="long", model="logistic", budget=100)
```

The result, `fit`, is a list of five elements: the data set used, the best-fitting parameters found, the residual sum of squares, the residual sum of squares normalized by number of data points, and the number of data points. See [manual](cre.pdf).


## Issues?

If you find a bug, please file an [issue](issues). If you have a feature request, please consider emailing Henri: [henri@henr.in](mailto:henri@henr.in).


## References

Kauhanen, H. & Walkden, G. (2018) Deriving the Constant Rate Effect. *Natural Language & Linguistic Theory*, 36(2), 483–521. <https://doi.org/10.1007/s11049-017-9380-1>

Kroch, A. (1989) Reflexes of grammar in patterns of language change. *Language Variation and Change*, 1: 199–244. 
