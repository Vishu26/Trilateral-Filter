library(EBImage)
computeGradients <- function(inputImage){
  xKernel <- matrix(c(-1, 0, 1, -2, 0, 2, -1, 0, 1), nrow = 3, ncol = 3, byrow=TRUE);
  yKernel <- matrix(c(1, 2, 1, 0, 0, 0, -1, -2, -1), nrow = 3, ncol = 3, byrow=TRUE);

    xGradient = filter2(inputImage, xKernel)
    yGradient <- filter2(inputImage, yKernel)
    return(list(xGradient, yGradient))
}