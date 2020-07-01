# format a list of fits in a nicer way as a data frame
build_return_df <- function(data,
                            result,
                            group) {
  groupnames <- unique(data[[group]])
  shared <- names(result$shared)
  free <- names(result$free[[1]])
  ret <- expand.grid(group=groupnames, parameter=c(shared, free), type=NA, value=NA)
  for (i in 1:nrow(ret)) {
    this_par <- ret[i,]$parameter
    this_group <- ret[i,]$group
    if (this_par %in% free) {
      ret[i,]$type <- "free"
      ret[i,]$value <- result$free[[factor2character(this_group)]][[factor2character(this_par)]]
    } else { # this_par is in shared
      ret[i,]$type <- "shared"
      ret[i,]$value <- result$shared[[factor2character(this_par)]]
    }
  }
  names(ret)[1] <- group
  ret
}
