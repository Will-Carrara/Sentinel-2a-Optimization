# function calculates error based on Euclidean distance between a point and 
# a line for every id. Line is composed of points from Landsat 7 & 8. 

error = function(ID, COEFF){
  diff = c()
  
  COEFF = anomaly(ID, COEFF)
  
  xL8 = L8$date[L8$id == ID]
  yL8 = L8$ndvi[L8$id == ID]
  
  xCOEFF = COEFF$date[COEFF$id == ID]
  yCOEFF = COEFF$ndvi[COEFF$id == ID]
  
  # distance of point a from line (b,c) in 2D
  dist2d = function(a,b,c) {
   v1 = b - c
   v2 = a - b
   m = cbind(v1,v2)
   
   return(abs(det(m))/sqrt(sum(v1*v1)))
  } 

  splits = function(ID){
    sub = c()
  
    for (num in 1:c(length(xL8)-1)){
      b = c(xL8[num], yL8[num])
      c = c(xL8[num+1], yL8[num+1])
      
      sub = c(sub,c(b,c))
    }
    return(sub)
  }
  
  lines = splits(ID)
  totalLines = length(lines) / 4
  
  for (num in 1:length(xCOEFF)){
    if (xCOEFF[num] > xL8[1] && xCOEFF[num] < xL8[length(xL8)]){
      for (value in c(seq(1,length(lines), by=4))){
        b = as.vector(lines[c(value, value+1)])
        c = as.vector(lines[c(value+2, value+3)])
        
        if (xCOEFF[num] > b[1] && xCOEFF[num] < c[1]){
          a = c(xCOEFF[num],yCOEFF[num])
          diff = c(diff,dist2d(a,b,c))
        }
      }
    }
  }
  return(round(mean(diff),3))
}
