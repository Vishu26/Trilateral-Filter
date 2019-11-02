computeMagnitude <- function(xGradient, yGradient){
  gradientMagnitude <- sqrt(xGradient^2 + yGradient^2)
  return(gradientMagnitude)
}