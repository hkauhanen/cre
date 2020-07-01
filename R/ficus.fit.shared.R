ficus.fit.shared <- function(formula,
                             data,
                             group,
                             shared,
                             start,
                             method = "average",
                             lower_bounds = NULL,
                             upper_bounds = NULL,
                             resolution = NULL,
                             iterations = NULL,
                             reduction = NULL,
                             sides = NULL,
                             avgfun = mean,
                             objfun = deviance,
                             fitfun = nls,
                             avgignore = NULL,
                             returnArguments = FALSE,
                             ...) {
  ret <- NULL

  # take care of 'method' argument
  possible_methods <- c("average", "stochastic", "recursive", "ranges")
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
    ret <- ficus.fit_average(formula=formula, data=data, group=group, start=start, shared=shared, FUN=fitfun, avgignore=avgignore, ...)
  } else if (match == 2) {
    ret <- ficus.fit_stochastic(formula=formula, data=data, group=group, start=start, shared=shared, budget=iterations, sides=sides, FUN=fitfun, avgignore=avgignore, ...)
  } else if (match == 3) {
    ret <- ficus.fit_recursive(formula=formula, data=data, group=group, start=start, shared=shared, lower_bounds=lower_bounds, upper_bounds=upper_bounds, resolution=resolution, recursions=iterations, reduction=reduction, FUN=fitfun, ...)
  } else if (match == 4) {
    ret <- ficus.fit_range(formula=formula, data=data, group=group, start=start, shared=shared, lower_bounds=lower_bounds, upper_bounds=upper_bounds, resolution=resolution, FUN=fitfun, ...)
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
