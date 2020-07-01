#' Logistic Function
#'
#' Generalized logistic function.
#'
#' @param t Variable (usually time, hence \code{t})
#' @param s Slope
#' @param k Intercept
#' @param U Upper asymptote
#' @param L Lower asymptote
#' @return Value of the function at \code{t}, given the parameters.
#'
#' @export
logistic <- function(t,
                     s = 1,
                     k = 0,
                     U = 1,
                     L = 0) {
  L + (U - L)/(1 + exp(-s*(t - k)))
}
