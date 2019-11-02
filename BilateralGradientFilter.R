BilateralGradientFilter = function(xGradient, yGradient, gradientMagnitude,sigmaC,sigmaR,epsilon){
  
  xGradientSmooth = matrix(0, nrow = nrow(xGradient), ncol = ncol(xGradient))
  yGradientSmooth = xGradientSmooth
  domainConst = -2*sigmaC*sigmaC
  rangeConst = -2*sigmaR*sigmaR
  halfSize = ceiling(sigmaC/2)
  domainWeight = matrix(0, nrow = halfSize, ncol = halfSize)
  
  for(row in 1:halfSize){
    for(col in 1:halfSize){
      diff_ = col*col + row*row
      domainWeight[row,col] = exp(diff_/domainConst)
    }
  }
  
  for(row in 1:nrow(gradientMagnitude)){
    for(col in 1:ncol(gradientMagnitude)){
      normFactor = 0
      tmpX = 0;
      tmpY = 0;
      g2 = gradientMagnitude[row,col]
      for(n in -halfSize:halfSize){
        for(m in -halfSize:halfSize){
          if (n && m){
            dWeight = domainWeight[abs(n),abs(m)]
            if(dWeight < epsilon) next;
            localX = col + m
            if(localX < 1) next;
            if (localX >= ncol(gradientMagnitude)+1) next;
            localY = row + n
            if(localY < 1) next;
            if(localY >= nrow(gradientMagnitude)+1) next;
            
            g1 = gradientMagnitude[localY, localX];
            gradDiffSq = (g1-g2)^2
            rangeWeight = exp(gradDiffSq/rangeConst)
            if (rangeWeight < epsilon) next;
            tmpX = tmpX + xGradient[localY,localX]*dWeight*rangeWeight
            tmpY = tmpY + yGradient[localY,localX]*dWeight*rangeWeight
            normFactor = normFactor + dWeight*rangeWeight
          }
        }
      }
      xGradientSmooth[row,col] = tmpX/normFactor
      yGradientSmooth[row,col] = tmpY/normFactor
    }
  }
  return(list(xGradientSmooth,yGradientSmooth))
}