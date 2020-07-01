# generator function for data-informed PBCM analysis
generate.datainformed <- function(modeltype,
                                  model,
                                  V,
                                  H,
                                  low_bound = 0.05,
                                  high_bound = 0.95) {
  bounds <- c(low_bound, high_bound)
  if (modeltype == "bias") {
    synthetic_data_from_bias(model=model, sampling_accuracy=V, time_points=H, bounds=bounds)
  } else if (modeltype == "logistic") {
    synthetic_data_from_fixeds(model=model, sampling_accuracy=V, time_points=H, bounds=bounds)
  } else if (modeltype == "VRE") {
    synthetic_data_from_indlog(model=model, sampling_accuracy=V, time_points=H, bounds=bounds)
  } else {
    stop("invalid modeltype")
  }
}

#' @importFrom stats rbinom
synthetic_data_from_bias <- function(model,
                                     sampling_accuracy,
                                     time_points,
                                     bounds) {
  number_of_changes <- length(unique(model$parameters$context))

  # bias for each change
  biases <- model$parameters[model$parameters$parameter=="b", ]$value

  # get low and high times
  mean_slope <- unique(model$parameters[model$parameters$parameter=="s", ]$value)
  k <- unique(model$parameters[model$parameters$parameter=="k", ]$value)
  low_time <- k - (1/mean_slope)*log(1/bounds[1] - 1)
  high_time <- k - (1/mean_slope)*log(1/bounds[2] - 1)

  # generate time sequence (equally spaced)
  times <- seq(from=low_time, to=high_time, length.out=time_points)

  # data frame for output
  out <- expand.grid(context=factor(1:number_of_changes), date=times, frequency=NA)
  out$bias <- biases[out$context]

  # determine frequencies
  for (i in 1:nrow(out)) {
    logi <- logistic(t=out[i,]$date, s=mean_slope, k=k)
    out[i,]$frequency <- sum(rbinom(n=sampling_accuracy, size=1, prob=logi + out[i,]$bias*logi*(1-logi)))/sampling_accuracy
  }

  # round off years
  out$date <- round(out$date)

  out[, c("date", "context", "frequency")]
}


#' @importFrom stats rbinom
synthetic_data_from_fixeds <- function(model,
                                       sampling_accuracy,
                                       time_points,
                                       bounds) {
  number_of_changes <- length(unique(model$parameters$context))

  # k for each change
  k <- model$parameters[model$parameters$parameter=="k", ]$value

  # get low and high times
  mean_slope <- unique(model$parameters[model$parameters$parameter=="s", ]$value)
  mean_k <- mean(k)
  low_time <- mean_k - (1/mean_slope)*log(1/bounds[1] - 1)
  high_time <- mean_k - (1/mean_slope)*log(1/bounds[2] - 1)

  # generate time sequence (equally spaced)
  times <- seq(from=low_time, to=high_time, length.out=time_points)

  # data frame for output
  out <- expand.grid(context=factor(1:number_of_changes), date=times, frequency=NA)
  out$k <- k[out$context]

  # determine frequencies
  for (i in 1:nrow(out)) {
    logi <- logistic(t=out[i,]$date, s=mean_slope, k=out[i,]$k)
    out[i,]$frequency <- sum(rbinom(n=sampling_accuracy, size=1, prob=logi))/sampling_accuracy
  }

  # round off years
  out$date <- round(out$date)

  out[, c("date", "context", "frequency")]
}


#' @importFrom stats rbinom
synthetic_data_from_indlog <- function(model,
                                       sampling_accuracy,
                                       time_points,
                                       bounds) {
  number_of_changes <- length(unique(model$parameters$context))

  # k and s for each change
  k <- model$parameters[model$parameters$parameter=="k", ]$value
  s <- model$parameters[model$parameters$parameter=="s", ]$value

  # get low and high times
  mean_slope <- mean(s)
  mean_k <- mean(k)
  low_time <- mean_k - (1/mean_slope)*log(1/bounds[1] - 1)
  high_time <- mean_k - (1/mean_slope)*log(1/bounds[2] - 1)

  # generate time sequence (equally spaced)
  times <- seq(from=low_time, to=high_time, length.out=time_points)

  # data frame for output
  out <- expand.grid(context=factor(1:number_of_changes), date=times, frequency=NA)
  out$k <- k[out$context]
  out$s <- s[out$context]

  # determine frequencies
  for (i in 1:nrow(out)) {
    logi <- logistic(t=out[i,]$date, s=out[i,]$s, k=out[i,]$k)
    out[i,]$frequency <- sum(rbinom(n=sampling_accuracy, size=1, prob=logi))/sampling_accuracy
  }

  # round off years
  out$date <- round(out$date)

  out[, c("date", "context", "frequency")]
}

