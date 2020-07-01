#' Fit Constant Rate Effect Models (Nonlinear Least Squares)
#'
#' Fit Constant (and Variable) Rate Effect models to data using a nonlinear least squares algorithm.
#'
#' It is possible to fit three kinds of models, controlled by the \code{model} argument: \code{"logistic"}, the classical model of a family of logistic curves with identical slopes but potentially varying intercepts; \code{"bias"}, the Kauhanen-Walkden production bias model; \code{"VRE"}, a family of logistic curves with independent slopes and intercepts.
#'
#' @param data A data frame
#' @param format One of \code{"wide"} or \code{"long"}; describes the format of the data frame.
#' @param contexts A vector of names of contexts to use in fitting. By default (\code{NULL}), all contexts available in the data are used.
#' @param model Model to fit: currently, one of \code{"logistic"}, \code{"bias"} or \code{"VRE"} (see Details).
#' @param budget Computational budget for fitting. Increasing the budget will improve goodness of fit but leads to longer runtimes. The default value of \code{100} is suitable in many cases, but the user is encouraged to experiment with the value.
#' @param warnOnly Whether a warning only should be issued when fitting fails. If \code{FALSE}, the routine exits with an error upon failure to fit.
#' @return Object of class \code{"logistic"}, \code{"bias"} or \code{"VRE"}, depending on the \code{model} specified; a list with the following elements:
#' \describe{
#' \item{\code{data}}{Data the model was fit on, in long format}
#' \item{\code{parameters}}{Model parameters}
#' \item{\code{cumul_objfun_value}}{Sum of squared residuals between model and data}
#' \item{\code{cumul_objfun_value_normalized}}{Sum of squared residuals divided by number of non-\code{NA} data points}
#' \item{\code{N}}{Number of non-\code{NA} data points}
#' }
#'
#' @importFrom stats coef deviance nls nls.control reshape runif sd
#' @export
fit.cre.nls <- function(data,
                        format,
                        contexts = NULL,
                        model,
                        budget = 100,
                        warnOnly = FALSE) {
  # take care of 'model' argument
  possible_models <- c("VRE", "logistic", "bias")
  possible_models2 <- paste0("'", possible_models, "'")
  match <- grep(x=possible_models, pattern=paste0("^", model))
  if (length(match) < 1) {
    stop(paste0("Invalid argument 'model'. Please supply one of: ", paste(possible_models2, collapse=","), "."))
  } else if (length(match) > 1) {
    stop(paste0("Argument 'model' matched by multiple models. Did you mean one of the following: ", paste(possible_models2[match], collapse=", "), "?"))
  }

  # prepare data
  data <- prepare_data(data=data, format=format, contexts=contexts)
  data <- data[!duplicated(data), ]

  # set arguments based on model
  if (match == 1) {
    # independent logistics
    formula <- frequency~(1/(1 + exp(-s*(date - k))))
    shared <- c("s")
  } else if (match == 2) {
    # fixed slopes
    formula <- frequency~(1/(1 + exp(-s*(date - k))))
    shared <- c("s")
  } else if (match == 3) {
    # production bias model
    formula <- frequency~(1/(1 + exp(-s*(date - k)))) + b*(1/(1 + exp(-s*(date - k))))*(1 - (1/(1 + exp(-s*(date - k)))))
    shared <- c("s", "k")
  }

  # guess ranges
  ranges <- tryCatch({guess_parameter_ranges(data=data, resolution=2)},
    error = function(cond) { NULL })
  if (is.null(ranges)) {
    if (warnOnly) {
      warning("No fit")
    } else {
      stop("No fit")
    }
  }

  lower_bounds <- vector("list", length(shared))
  names(lower_bounds) <- shared
  upper_bounds <- lower_bounds
  for (par in shared) {
    lower_bounds[[par]] <- min(ranges[[par]])
    upper_bounds[[par]] <- max(ranges[[par]])
  }

  # guess starting values
  start <- list()
  start$s <- mean(ranges[["s"]])
  start$k <- mean(ranges[["k"]])
  if (match == 3) {
    start$b <- 0.1
  }

  # set ficus parameters
  avgignore <- NULL
  iterations <- 1
  resolution <- round((budget/iterations)^(1/length(shared)))
  reduction <- 0.5
  sides <- vector("list", length(shared))
  names(sides) <- shared
  for (par in shared) {
    sides[[par]] <- sd(ranges[[par]])
    sides[[par]] <- (upper_bounds[[par]] - lower_bounds[[par]])/2
  }

  # (try to) fit
  fit <- NULL
  if (match == 3) {
    lower <- -1
    upper <- 1
  } else {
    lower <- NULL
    upper <- NULL
  }
  tryCatch({
    if (match == 1) {
      fit <- sucif.fit.independent(formula=formula,
                                   data=data,
                                   group="context",
                                   independent=shared,
                                   start=start,
                                   method="ranges",
                                   lower_bounds=lower_bounds,
                                   upper_bounds=upper_bounds,
                                   resolution=resolution,
                                   iterations=iterations,
                                   reduction=reduction,
                                   objfun=deviance,
                                   avgignore=avgignore,
                                   fitfun=nls,
                                   algorithm="port",
                                   lower=lower,
                                   upper=upper)
    } else {
      fit <- ficus.fit.shared(formula=formula,
                              data=data,
                              group="context",
                              shared=shared,
                              start=start,
                              method="ranges",
                              lower_bounds=lower_bounds,
                              upper_bounds=upper_bounds,
                              resolution=resolution,
                              iterations=iterations,
                              reduction=reduction,
                              sides=sides,
                              avgfun=mean,
                              objfun=deviance,
                              avgignore=avgignore,
                              fitfun=nls,
                              algorithm="port",
                              lower=lower,
                              upper=upper)
    }
  }, error = function(error_condition) {
    warning("Fit failed")
  })

  # figure out number of non-NA data points
  freqs <- data$frequency
  nonNAs <- length(freqs[!is.na(freqs)])

  # return
  if (is.null(fit)) {
    if (warnOnly) {
      warning("No fit")
    } else {
      stop("No fit")
    }
  } else {
    fit <- fit[names(fit) != "arguments"]
    fit$cumul_objfun_value_normalized <- fit$cumul_objfun_value/nonNAs
    fit$N <- nonNAs
    fit$data <- data
    class(fit) <- possible_models[match]
  }
  fit[c("data", "parameters", "cumul_objfun_value", "cumul_objfun_value_normalized", "N")]
}
