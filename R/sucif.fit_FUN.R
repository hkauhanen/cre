# the meat of sucif: fits a formula using modelling function FUN (e.g. nls, 
# lm, ...)
sucif.fit_FUN <- function(formula,
                          data,
                          group,
                          whichgroup,
                          shared = NULL,
                          FUN,
                          objfun = deviance,
                          ...) {
  # modify formula: substitute shared parameter variables with values
  for (par in names(shared)) {
    formula <- formula(gsub(deparse(formula), pattern=par, replacement=shared[[par]]))
  }

  # fit to each group separately; if fitting to any of the groups fails,
  # warn and return nothing
  returned_object <- NULL
  tryCatch({
    fit <- FUN(formula=formula, data=data[data[[group]]==whichgroup, ], ...)
    error <- objfun(fit)
    free_parameters <- as.list(coef(fit))
    ret <- list(fit=fit, objfun_value=error, shared=shared, free=free_parameters)
    #ret <- build_return_df(data=data, result=ret, group=group)
    #ret <- list(parameters=ret, cumul_objfun_value=error)
    returned_object <- ret
  }, error = function(error_condition) {
    warning("Fitting failed for at least one parameter combination")
  })

  # return
  returned_object
}
