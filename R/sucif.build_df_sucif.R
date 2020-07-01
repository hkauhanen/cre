build_df_sucif <- function(ret,
                           group,
                           shared) {
  cumul_objfun_value <- 0
  out <- expand.grid(gr=names(ret), parameter=c(names(ret[[1]]$shared), names(ret[[1]]$free)), type=NA, value=NA)
  for (i in 1:nrow(out)) {
    out[i,]$type <- ifelse(out[i,]$parameter %in% shared, "shared", "free")
    out[i,]$value <- ret[[factor2character(out[i,]$gr)]][[factor2character(out[i,]$type)]][[factor2character(out[i,]$parameter)]]
  }
  for (n in names(ret)) {
    cumul_objfun_value <- cumul_objfun_value + ret[[n]]$objfun_value
  }
  out$type <- ifelse(out$type == "shared", "independent", "free")
  names(out)[1] <- group
  list(parameters=out, cumul_objfun_value=cumul_objfun_value)
}

