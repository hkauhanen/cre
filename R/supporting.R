# Supporting (internal) functions, no user-level access


# guess suitable parameter starting values and ranges for the regressions
guess_parameter_ranges <- function(data,
                                   resolution,
                                   range_multiplier = 1,
                                   maxiter = 50,
                                   trace = FALSE) {
  # hack
  data$date_dfo <- data$date

  # we start by (auto)fitting a pure logistic to each context independently.
  # For a starting value of k, we simply take mean date_dfo. For a starting
  # value of s, we take something that is inversely proportional to
  # max_date_dfo - min_date_dfo (for increasing changes) or to
  # min_date_dfo - max_date_dfo (for decreasing changes).
  min_date_dfo <- min(data$date_dfo)
  max_date_dfo <- max(data$date_dfo)

  if (sum(data[data$date_dfo==min_date_dfo, ]$frequency, na.rm=TRUE) <
      sum(data[data$date_dfo==max_date_dfo, ]$frequency, na.rm=TRUE)) {
    increasing <- TRUE
  } else {
    increasing <- FALSE
  }

  if (increasing) {
    s0 <- 4/(max_date_dfo - min_date_dfo)
  } else {
    s0 <- 4/(min_date_dfo - max_date_dfo)
  }

  k0 <- mean(data$date_dfo)

  esses <- vector("list", length(unique(data$context)))
  names(esses) <- unique(data$context)
  kays <- esses

  for (con in unique(data$context)) {
    fit <- tryCatch({nls(frequency ~ 1/(1 + exp(-s*(date_dfo - k))),
               data[data$context==con, ],
               start=list(s=s0, k=k0),
               control=nls.control(warnOnly=FALSE, maxiter=maxiter),
               trace=trace)},
                    error = function(cond) {
                      warning("Autoguessing failed for some model")
                      return(NULL)
                    })
    if (is.null(fit)) {
      # if fit failed, the coefficients of 'fit' will have junk values. These
      # will throw subsequent fitting off. In such cases, let's assume s0 and
      # k0 are reasonable estimates.
      esses[[con]] <- s0
      kays[[con]] <- k0
    } else {
      esses[[con]] <- coef(fit)[1]
      kays[[con]] <- coef(fit)[2]
    }
  }

  esses <- unlist(esses)
  kays <- unlist(kays)
  s_diam <- abs(max(esses) - min(esses))
  k_diam <- abs(max(kays) - min(kays))
  ranges_from_s_and_k(s=mean(esses), k=mean(kays), resolution=resolution, s_range_diameter=range_multiplier*s_diam, k_range_diameter=range_multiplier*k_diam)
}


# given lists of s and k parameters, produce ranges
ranges_from_s_and_k <- function(s,
                                k,
                                s_range_diameter,
                                k_range_diameter,
                                resolution) {
  # return ranges as uniform sequences of length 'resolution'
  s_range <- seq(from=s - 0.5*s_range_diameter, to=s + 0.5*s_range_diameter, length.out=resolution)
  k_range <- seq(from=k - 0.5*k_range_diameter, to=k + 0.5*k_range_diameter, length.out=resolution)
  k_range <- k_range
  list(s=s_range, k=k_range)
}


# turn date string (yyyy-mm-dd, yyyy-mm or yyyy) into days from origin (dfo)
string_to_dfo <- function(x) {
  #ymd_pattern <- "^\\d+-\\d+-\\d+$"
  ym_pattern <- "^\\d+/\\d+$"
  y_pattern <- "^\\d+$"

  if (string_matches(x=x, pattern=ym_pattern)) {
    x <- paste0(x, "/15")
  } else if (string_matches(x=x, pattern=y_pattern)) {
    x <- paste0(x, "/07/02")
  }

  as.integer(as.Date(x))
}


# string match
string_matches <- function(x, pattern) {
  if (length(grep(pattern=pattern, x=x)) > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


# turn a factor into a character vector
# (why, oh why, is this not a base R function?)
factor2character <- function(x) {
  if (is.factor(x)) {
    return(levels(x)[as.numeric(x)])
  } else {
    return(x)
  }
}


