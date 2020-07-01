# turn frequency data into response data assuming an average vertical
# resolution V
freq2resp <- function(data,
                      V) {
  responsedata <- NULL
  for (i in 1:nrow(data)) {
    if (!is.na(data[i,]$frequency)) {
      positives <- rep(1, round(data[i,]$frequency*V))
      negatives <- rep(0, V - length(positives))
      responses <- c(positives, negatives)
      here <- data.frame(date=data[i,]$date, context=data[i,]$context, response=responses)
      responsedata <- rbind(responsedata, here)
    }
  }
  print(responsedata)
  responsedata
}

