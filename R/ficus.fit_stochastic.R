ficus.fit_stochastic <- function(formula,
                                 data,
                                 group,
                                 shared,
                                 sides,
                                 start,
                                 budget,
                                 objfun = deviance,
                                 avgfun = mean,
                                 avgignore = NULL,
                                 temperature = 1,
                                 FUN = nls,
                                 ...) {
  # get average fit as a starting point
  fit <- ficus.fit_average(formula=formula,
                           data=data,
                           group=group,
                           shared=shared,
                           start=start,
                           objfun=objfun,
                           avgfun=avgfun,
                           avgignore=avgignore,
                           FUN=FUN,
                           ...)
  error <- fit$cumul_objfun_value

  # revise this fit stochastically until computing budget is used up
  for (bud in 1:budget) {
    # determine side lengths of the hypercube around current fit
    lower_bounds <- vector("list", length(shared))
    names(lower_bounds) <- shared
    upper_bounds <- lower_bounds
    for (par in shared) {
      avg <- unique(fit$parameters[fit$parameters$parameter==par, ]$value)
      lower_bounds[[par]] <- avg - temperature^(bud - 1)*0.5*sides[[par]]
      upper_bounds[[par]] <- avg + temperature^(bud - 1)*0.5*sides[[par]]
    }

    # pick random values from hypercubes
    rand <- vector("list", length(shared))
    names(rand) <- shared
    for (par in shared) {
      rand[[par]] <- runif(n=1, min=lower_bounds[[par]], max=upper_bounds[[par]])
    }

    # fit on these random values
    start <- start[!(names(start) %in% shared)]
    new_fit <- ficus.fit_FUN(formula=formula,
                             data=data,
                             group=group,
                             shared=rand,
                             FUN=FUN,
                             objfun=objfun,
                             start=start,
                             ...)

    # if new fit improves on old, update
    if (!is.null(new_fit)) {
      new_error <- new_fit$cumul_objfun_value
      if (new_error < error) {
        fit <- new_fit
        error <- new_error
      }
    }
  }

  # return
  if (is.null(fit)) {
    stop("Cannot fit")
  }
  fit
}
