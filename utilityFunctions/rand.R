# grab random id 
rand = function (n){
  x = sample(1:nrow(L8), n, replace=F)
  return(L8[x,"id"])
}
