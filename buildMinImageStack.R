buildMinImageStack <- function(gradientMagnitude,maxLevel){
  minStack = array(0, dim=c(nrow(gradientMagnitude), ncol(gradientMagnitude), maxLevel))
  
  for(i in 1:nrow(gradientMagnitude)){
    for(j in 1:ncol(gradientMagnitude)){
      outMin = 1E12
      for(n in max(i-1, 1):min(i+2, nrow(minStack))){
        for(m in max(j-1, 1):min(j+2, ncol(minStack))){
          outMin = min(gradientMagnitude[n,m],outMin);
        }
      }
      minStack[i,j,1] = outMin;
    }
  }
  
  for (ii in 2:maxLevel){
    minImg = minStack[,,ii-1]
    for(i in 1:nrow(gradientMagnitude)){
      for(j in 1:ncol(gradientMagnitude)){
        outMin = 1E12;
        
        for(n in max(i-1, 1):min(i+2, nrow(minStack))){
          for(m in max(j-1, 1):min(j+2, ncol(minStack))){
            outMin = min(minImg[n,m],outMin);
          }
        }
        minStack[i,j,ii] = outMin
      }
    }
  }
  return(minStack)
}