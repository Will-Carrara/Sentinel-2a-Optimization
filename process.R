process = function (path, year) {
  data = read.csv(path, header=TRUE)[,2:4]
  
  names(data) = c("ndvi", "id", "date")

  # remove any negative values
  data$ndvi = ifelse(data$ndvi <= 0, NA, data$ndvi)
  data = na.omit(data)
  
  # dates -> doy
  data$date = round(abs(as.numeric(difftime(paste0(year,"-01-01"), data$date, units="days"))),1) + 1
  
  # order time series data
  data = data[order(data$date),]
  
  # remove duplicate dates for every unique id
  data = data[!duplicated(data[c(2,3)]),]

  return(data)
}
