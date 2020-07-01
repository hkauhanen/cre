# Prepare mock datasets that can be used for illustration purposes.


# This function turns a response dataset into a frequency dataset.
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
  out[, c("date", "context", "frequency")]
}


# Make a dataset of random binary responses
df <- expand.grid(date=seq(from=1400, to=1600, by=10),
                  speaker=paste0("sp", seq(from=1, to=100, by=1)),
                  context=paste0("con", seq(from=1, to=3, by=1)),
                  response=NA)
df$response <- sample(0:1, size=nrow(df), replace=TRUE)


# Write to file
write.csv(df, file="inst/extdata/mockdata_responses.csv", row.names=FALSE)


# Write frequentized versions in both long and wide format
write.csv(frequentize(df), file="inst/extdata/mockdata_long.csv", row.names=FALSE)
wide_df <- reshape(frequentize(df), idvar="date", timevar="context", direction="wide")
write.csv(wide_df, file="inst/extdata/mockdata_wide.csv", row.names=FALSE)

