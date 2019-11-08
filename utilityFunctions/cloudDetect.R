cloudDetect = function (ID, set) {
    thresh = .78
    
    subSET = set[which(set$id == ID), ]
    
    Sx = set$date[set$id == ID] 
    Sy = set$ndvi[set$id == ID] 
    
    L8x = L8$date[L8$id == ID] 
    L8y = L8$ndvi[L8$id == ID] 
    
    L7x = L7$date[L7$id == ID] 
    L7y = L7$ndvi[L7$id == ID]
    
    satsX = c(as.vector(L7x) , as.vector(L8x) , as.vector(Sx))
    satsY = c(as.vector(L7y) , as.vector(L8y) , as.vector(Sy))
    
    satellite <- data.frame(satsX, satsY)
    satellite = satellite[order(satellite$satsX), ]
    satsX = satellite$satsX
    satsY = satellite$satsY
    
    outNDVI = c(Sy[1])
    outDATE = c(Sx[1])

    for (num in 1:length(Sy)){
      p = which(Sy[num] == satsY)
      
      if (num == 1) {
        if (Sy[num] < thresh*satsY[p+1]){
          outNDVI = c(outNDVI, Sy[num])
          outDATE = c(outDATE, Sx[num])
        }
      }
      
      else if (num == length(Sy)){
        if (Sy[num] < thresh*satsY[p-1]){
          outNDVI = c(outNDVI, Sy[num])
          outDATE = c(outDATE, Sx[num])
        }
      }
    
      else if (Sy[num] < (satsY[p-1]-.2) || Sy[num] < thresh*satsY[p+1] || Sy[num] < mean(satsY)/3){
        outNDVI = c(outNDVI, Sy[num])
        outDATE = c(outDATE, Sx[num])
      }
    }
    
    out = list() 
    out[[1]] = outNDVI
    out[[2]] = outDATE
    
    return(subSET[!subSET$date %in% out[[2]],])
}
