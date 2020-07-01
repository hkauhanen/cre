ficus.fit_average <- function(formula,
                              data,
                              group,
                              shared,
                              start,
                              objfun = deviance,
                              avgfun = mean,
                              avgignore = NULL,
                              FUN = nls,
                              ...) {
  groups_in_data <- unique(data[[group]])

  # first fit to each group individually, treating all parameters as free
  pars_across_groups <- vector("list", length(groups_in_data))
  names(pars_across_groups) <- groups_in_data
  avgstart <- start[!(names(start) %in% names(avgignore))]
  result <- ficus.fit_FUN(formula=formula, data=data, group=group, FUN=FUN, shared=avgignore, objfun=objfun, start=avgstart)
  if (is.null(result)) {
    stop("Cannot fit")
  }
  result <- result$parameters

  # take averages of the shared parameters, fit again treating these
  # as constants
  averages <- vector("list", length(shared))
  names(averages) <- shared
  for (par in shared) {
    thisres <- result[result$parameter==par, ]
    averages[[par]] <- avgfun(thisres$value)
  }

  # fit, using above averages for shared parameters
  start <- start[!(names(start) %in% shared)]
  result <- ficus.fit_FUN(formula=formula, data=data, group=group, FUN=FUN, shared=averages, objfun=objfun, start=start, ...)

  # return
  result
}
