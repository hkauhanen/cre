ficus.fit_recursive <- function(formula,
                                data,
                                group,
                                shared,
                                start,
                                lower_bounds,
                                upper_bounds,
                                resolution,
                                recursions,
                                reduction,
                                objfun = deviance,
                                FUN = nls,
                                ...) {
  n_shared_parameters <- length(shared)
  fit <- NULL
  error <- .Machine$double.xmax
  start <- start[!(names(start) %in% shared)]

  # loop through recursions
  for (rec in 1:recursions) {
    # construct a grid of parameter value combinations
    shared_pars <- vector("list", n_shared_parameters)
    names(shared_pars) <- shared
    for (par in shared) {
      shared_pars[[par]] <- seq(from=lower_bounds[[par]],
                                to=upper_bounds[[par]],
                                length.out=resolution)
    }
    grid <- expand.grid(shared_pars)

    # loop through rows in parameter grid
    for (i in 1:nrow(grid)) {
      # construct list of shared parameters
      this_shared <- vector("list", n_shared_parameters)
      names(this_shared) <- shared
      for (par in shared) {
        this_shared[[par]] <- grid[[par]][i]
      }

      # fit
      result <- ficus.fit_FUN(formula=formula,
                              data=data,
                              group=group,
                              FUN=FUN,
                              shared=this_shared,
                              objfun=objfun,
                              start=start,
                              ...)

      # if error decreased, update
      if (!is.null(result)) {
        new_error <- result$cumul_objfun_value
        if (new_error < error) {
          error <- new_error
          fit <- result
        }
      }
    }

    # update parameter ranges
    for (par in shared) {
      fitted_value <- unique(fit$parameters[fit$parameters$parameter==par,]$value)
      old_lower_bounds <- lower_bounds[[par]]
      old_upper_bounds <- upper_bounds[[par]]
      lower_bounds[[par]] <- fitted_value - reduction*(old_upper_bounds - old_lower_bounds)
      upper_bounds[[par]] <- fitted_value + reduction*(old_upper_bounds - old_lower_bounds)
    }
  }

  # return
  if (is.null(fit)) {
    stop("Cannot fit")
  }
  fit
}
