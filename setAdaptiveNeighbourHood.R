setAdaptiveNeighbourHood <- function(gradientMagnitude, sigmaR, maxLevel){
  adaptiveNeighbourHood <- matrix(0, nrow = nrow(gradientMagnitude), ncol = ncol(gradientMagnitude))
  minStack = buildMinImageStack(gradientMagnitude, maxLevel)
  maxStack = buildMaxImageStack(gradientMagnitude, maxLevel)
  
  for(i in 1:nrow(gradientMagnitude)){
    for(j in 1:ncol(gradientMagnitude)){
      
      upperThreshold = gradientMagnitude[i,j] + sigmaR;
      lowerThreshold = gradientMagnitude[i,j] - sigmaR;
      
      for(lev in 1:maxLevel){
        minImg = minStack[,,lev]
        maxImg = maxStack[,,lev]
        
        if (maxImg[i,j]>upperThreshold || minImg[i,j]<lowerThreshold){
          break;
        }
      }
      adaptiveNeighbourHood[i,j] = 2^(lev-1)
    }
  }
  return(adaptiveNeighbourHood)
}