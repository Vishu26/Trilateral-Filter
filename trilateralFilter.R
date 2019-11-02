library(gsubfn)
trilateralFilter <- function(inputImage, sigmaC, epsilon){
  beta <- 0.15;
  list[xGradient, yGradient] <- computeGradients(inputImage);
  gradientMagnitude = computeMagnitude(xGradient,yGradient);
  minGrad = min(min(gradientMagnitude));
  maxGrad = max(max(gradientMagnitude));
  sigmaR = beta*(maxGrad-minGrad);
  maxLUT = round(sigmaC*sqrt(abs(log(epsilon))))+1;
  maxLevel = ceiling(log2(2*maxLUT+1));
  adaptiveNeighbourhood = setAdaptiveNeighbourHood(gradientMagnitude,sigmaR,maxLevel);
  list[xGradientSmooth, yGradientSmooth] = BilateralGradientFilter(xGradient, yGradient,gradientMagnitude,sigmaC,sigmaR,epsilon)
  outputImage = DetailBilateralFilter(inputImage, adaptiveNeighbourhood,xGradientSmooth,yGradientSmooth,sigmaC, sigmaR, maxLUT, epsilon)
  return(outputImage)
}

library(raster)
img = brick("IndianPines.tif")
dataset = as.array(img)
image(dataset[,,3])

filtered = trilateralFilter(dataset[,,3], 8, 0.1)
image(filtered)
storage.mode(filtered) = "double"
