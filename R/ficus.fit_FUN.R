# the meat of ficus: fits a formula to a dataset across a number of contexts
# using modelling function FUN (e.g. nls, lm, ...)
ficus.fit_FUN <- function(formula,
                          data,
                          group,
                          shared = NULL,
                          FUN,
                          objfun = deviance,
                          ...) {
  # some trivial initializations
  groupnames <- unique(data[[group]])
  fits <- vector("list", length(groupnames))
  names(fits) <- groupnames
  error <- 0

  # modify formula: substitute shared parameter variables with values
  for (par in names(shared)) {
    formula <- formula(gsub(deparse(formula), pattern=par, replacement=shared[[par]]))
  }

  # fit to each group separately; if fitting to any of the groups fails,
  # warn and return nothing
  returned_object <- NULL
  tryCatch({
    free_parameters <- vector("list", length(groupnames))
    names(free_parameters) <- groupnames
    for (gr in groupnames) {
      fit <- FUN(formula=formula, data=data[data[[group]]==factor2character(gr), ], ...)
      fits[[factor2character(gr)]] <- fit
      error <- error + objfun(fit)
      free_parameters[[gr]] <- as.list(coef(fit))
    }
    ret <- list(fits=fits, objfun_value=error, shared=shared, free=free_parameters)
    ret <- build_return_df(data=data, result=ret, group=group)
    ret <- list(parameters=ret, cumul_objfun_value=error)
    returned_object <- ret
  }, error = function(error_condition) {
    warning("Fitting failed for at least one parameter combination")
  })

  # return
  returned_object
}
