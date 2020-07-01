#' Prepare Data
#'
#' Prepare data for use with the curve-fitting routines in \code{\link{fit.cre.nls}}.
#'
#' Under normal circumstances, there is no need for the end user to call this function directly: it is called automatically by the \code{\link{fit.cre.nls}} routine. User-level access is provided for completeness and for debugging purposes.
#'
#' @param data A data frame (e.g. from \code{read.csv})
#' @param format Format of the data frame: either "wide" or "long"
#' @param contexts Which contexts to use in fitting. By default (\code{NULL}), all contexts are used.
#' @return A data frame in a format \code{\link{fit.cre.nls}} understands.
#' @export
prepare_data <- function(data,
                         format,
                         contexts = NULL) {
  # check that 'date' OR ('start' AND 'end') exist(s)
  if (!exists("date", where=data)) {
    if (!exists("start", where=data)) {
      stop("No 'date' or 'start' column in data!")
    }
    if (!exists("end", where=data)) {
      stop("No 'date' or 'end' column in data!")
    }
  }

  if (exists("date", where=data)) {
    data$date <- as.numeric(data$date)
  } else {
    start_dfo <- as.numeric(data$start)
    end_dfo <- as.numeric(data$end)
    data$date <- round((start_dfo + end_dfo)/2)
  }

  # if data is in wide format, we need to cast it into long
  if (format == "wide") {
    # figure out contexts, unless contexts vector specifically given
    all_contexts <- names(data)[!(names(data) %in% c("date", "start", "end"))]

    # cast to long
    data <- reshape(data, varying=list(all_contexts), direction="long", timevar="context", times=all_contexts, idvar="date", v.names="frequency")
  }

  # if contexts vector explicitly given, trim down to it
  if (!is.null(contexts)) {
    data <- data[data$context %in% contexts, ]
  }

  # replace row names with something sensible
  rownames(data) <- 1:nrow(data)

  # if frequencies are over 1, assume they are percentages; hence divide by 100
  if (sum(data$frequency > 1, na.rm=TRUE) > 0) {
    data$frequency <- data$frequency/100
  }

  # the Kauhanen-Walkden model predicts and 'underlying' probability of the
  # linguistic variable. We want to reserve a special context called
  # 'underlying' to represent this. Thus check that data frame does not
  # already contain a context with such a name. If it does, stop.
  if ("underlying" %in% unique(data$context)) {
    stop("Illegal name 'underlying' for one of the contexts. Please rename.")
  }

  # return
  data
}


