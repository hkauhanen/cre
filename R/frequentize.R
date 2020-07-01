#' From Response to Frequency Data
#'
#' Turn a response-based dataset into a frequency dataset.
#'
#' @param data Data frame; must contain columns labelled \code{"date"}, \code{"context"} and \code{"response"}.
#' @return A data frame.
#' 
#' @export
frequentize <- function(data) {
  dates <- unique(data$date)
  contexts <- unique(data$context)
  out <- expand.grid(date=dates, context=contexts, applications=NA, nonapplications=NA, N=NA, frequency=NA)
  for (i in 1:nrow(out)) {
    df <- data[data$date==out[i,]$date & data$context==out[i,]$context, ]
    out[i,]$applications <- sum(df$response)
    out[i,]$N <- length(df$response)
    out[i,]$nonapplications <- out[i,]$N - out[i,]$applications
    out[i,]$frequency <- out[i,]$applications/out[i,]$N
  }
  out
}


