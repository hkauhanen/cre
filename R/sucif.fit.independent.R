sucif.fit.independent <- function(formula,
                                  data,
                                  group,
                                  independent,
                                  start,
                                  method = "average",
                                  lower_bounds = NULL,
                                  upper_bounds = NULL,
                                  resolution = NULL,
                                  iterations = NULL,
                                  reduction = NULL,
                                  objfun = deviance,
                                  fitfun = nls,
                                  avgignore = NULL,
                                  returnArguments = FALSE,
                                  ...) {
  shared <- independent

  ret <- NULL

  # take care of 'method' argument
  possible_methods <- c("recursive", "ranges")
  possible_methods2 <- paste0("'", possible_methods, "'")
  match <- grep(x=possible_methods, pattern=paste0("^", method))
  if (length(match) < 1) {
    stop(paste0("Invalid argument 'method'. Please supply one of: ", paste(possible_methods2, collapse=", "), "."))
  }
  if (length(match) > 1) {
    stop(paste0("Argument 'method' matches multiple values. Did you mean one of the following: ", paste(possible_methods2[match], collapse=", "), "?"))
  }

  # run matched method
  if (match == 1) {
    ret <- sucif.fit_recursive(formula=formula, data=data, group=group, start=start, shared=shared, lower_bounds=lower_bounds, upper_bounds=upper_bounds, resolution=resolution, recursions=iterations, reduction=reduction, FUN=fitfun, ...)
  } else if (match == 2) {
    ret <- sucif.fit_range(formula=formula, data=data, group=group, start=start, shared=shared, lower_bounds=lower_bounds, upper_bounds=upper_bounds, resolution=resolution, FUN=fitfun, ...)
  } else {
    stop("Invalid argument 'method'")
  }

  # return together with arguments supplied to this function
  if (returnArguments) {
    ret$arguments <- as.list(match.call())[-1]
    ret$arguments$method <- possible_methods[match]
  }
  ret
}
