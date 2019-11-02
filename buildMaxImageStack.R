buildMaxImageStack <- function(gradientMagnitude,maxLevel){
  maxStack = array(0, dim=c(nrow(gradientMagnitude), ncol(gradientMagnitude), maxLevel))
  
  for(i in 1:nrow(gradientMagnitude)){
    for(j in 1:ncol(gradientMagnitude)){
      outMax = -1E12
      for(n in max(i-1, 1):min(i+2, nrow(maxStack))){
        for(m in max(j-1, 1):min(j+2, ncol(maxStack))){
          outMax = max(gradientMagnitude[n,m],outMax);
        }
      }
      maxStack[i,j,1] = outMax;
    }
  }
  
  for (ii in 2:maxLevel){
    maxImg = maxStack[,,ii-1]
    for(i in 1:nrow(gradientMagnitude)){
      for(j in 1:ncol(gradientMagnitude)){
        outMax = -1E12;
        
        for(n in max(i-1, 1):min(i+2, nrow(maxStack))){
          for(m in max(j-1, 1):min(j+2, ncol(maxStack))){
            outMin = max(maxImg[n,m],outMax);
          }
        }
        maxStack[i,j,ii] = outMax
      }
    }
  }
  return(maxStack)
}