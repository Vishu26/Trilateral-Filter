DetailBilateralFilter = function(inputImage,adaptiveRegion,xGradientSmooth,yGradientSmooth,sigmaC,sigmaR,maxDomainSize,epsilon){
  outputImage = matrix(0, nrow = nrow(inputImage), ncol = ncol(inputImage))
  domainConst = -2*sigmaC*sigmaC;
  rangeConst = -2*sigmaR*sigmaR;
  
  domainWeight = matrix(0, nrow=maxDomainSize, ncol=maxDomainSize)
  
  for(row in 1:maxDomainSize){
    for(col in 1:maxDomainSize){
      diff_ = col*col + row*row
      domainWeight[row,col] = exp(diff_/domainConst)
    }
  }
  
  for(row in 1:nrow(inputImage)){
    for(col in 1:ncol(inputImage)){
      normFactor = 0
      tmp = 0;
      halfSize = min(adaptiveRegion[row,col],maxDomainSize)
      coeffA = xGradientSmooth[row,col]
      coeffB = yGradientSmooth[row,col]
      coeffC = inputImage[row,col]
      for(n in -halfSize:halfSize){
        for(m in -halfSize:halfSize){
          if (n && m){
            dWeight = domainWeight[abs(n),abs(m)]
            if(dWeight < epsilon) next;
            localX = col + m
            if(localX < 1) next;
            if (localX >= ncol(inputImage)+1) next;
            localY = row + n
            if(localY < 1) next;
            if(localY >= nrow(inputImage)+1) next;
            
            detail = inputImage[localY, localX] - coeffA*m - coeffB*n - coeffC
            rangeWeight = exp(detail^2 / rangeConst)
            tmp = tmp + detail*dWeight*rangeWeight
            normFactor = normFactor + dWeight*rangeWeight
          }
        }
      }
      outputImage[row,col] = tmp/normFactor + coeffC
    }
  }
  return(outputImage)
}