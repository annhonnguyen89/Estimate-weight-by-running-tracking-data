# remove outliers
remove_outlier <- function(dfdata, cols){
  # http://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset
  # Remove outliers of column cols of dfdata
  # Args:
  # - dfdata
  # - Set of cols
  # Return: data frame with outliers removed
  
  result <- dfdata
  for (col in cols){
    x = result[[col]]
    qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
    H <- 1.5 * IQR(x, na.rm=TRUE)
    low = qnt[1] - H
    up = qnt[1] + H
    result <- result[with(result, get(col) >= low) & with(result, get(col) < up),]
  }
  
  result <- na.omit(result)
  return (result)
}


compute_time_peaks <- function(df){
  # Compute time gap between 2 peak times (in 10s)
  
  newdf <- df[order(-df$xacc),]
  if(nrow(newdf)>3){
    d1 = newdf[1,"time"] - newdf[2,"time"]
    d2 = newdf[2,"time"] - newdf[3,"time"]
    return (mean(d1,d2))
  }else
    return (NA)
}


# compute distribution of xacc, yacc, zacc
binned_dist <- function(x, min, max, nbin){
  # compute distribution of x
  # Args:
  # - x:
  # - min,max,nbin
  # Return: vector of number of samples falls in each bin
  
  freq <- rep(0, nbin)
  ind <- as.array(seq(1,nbin))
  bin = seq(from=min, to=max, by=(max - min) / nbin)
  freq = apply(ind, 1, function(i) {  low = bin[i]
                                    up = bin[i+1]
                                    length(x[x > low & x<up])
                                 })
  return (freq)
}

