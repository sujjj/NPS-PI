# prediction by the empirical cdf
ecdf.pred <- function(perc,x,w){
  #perc = percentile
  #x = values
  #w = weights - sum to 1

  # trim x so only have vaules with posative weights
  x <- x[w>0]
  w <- w[w>0]

  # form the empricial cdf
  sort.x <- sort(x,index=TRUE)
  ecdf <- cumsum(w[sort.x$ix])

  # calculate the percentiles
  out <- rep(NA,length(perc))
  for(ii in 1:length(perc)){
    jj <- which.min(abs(ecdf-perc[ii]))
    flag <- TRUE
    while(flag==TRUE){
      if(perc[ii]<=0.5){
        if(ecdf[jj]>perc[ii]){
          jj <- jj-1
        }else{
          flag <- FALSE
        }
      }else{
        if(ecdf[jj]<perc[ii]){
          jj <- jj+1
        }else{
          flag <- FALSE
        }
      }
    }
    out[ii] <- sort.x$x[jj]
  }

  return(out)
}
