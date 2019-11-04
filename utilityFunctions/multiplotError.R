# this function can be used as a loop plotter in addition to calculating an average 
# error for the given time series. 

multiplotError = function (plot=FALSE, set) {
  
  e = c()
  for (id in ids) {
    if (plot == TRUE){timeSeries(id, Sent2aMod)}
    e = c(e,error(id, set))
  }
  
  return(round(mean(e),3))
}
