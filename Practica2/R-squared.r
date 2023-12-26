calculo_SSR <- function(x, y, alpha, beta) {
  mean_y <- mean(y)
  y_pred <- alpha + beta * x
  SSR <- sum((y_pred - mean_y)^2)
  return(SSR)
}

calculo_SSy <- function(y) {
  mean_y <- mean(y)
  SSy <- sum((y - mean_y)^2)
  return(SSy)
}

calculo_R_cuadrado <- function(x, y, alpha, beta) {
  SSR <- calculo_SSR(x, y, alpha, beta)
  SSy <- calculo_SSy(y)
  R_cuadrado <- (SSR / SSy)
  return(R_cuadrado)
}