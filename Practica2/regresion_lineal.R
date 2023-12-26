# Función de regresión lineal simple
regresion_lineal <- function(x, y) {
  n <- length(x)
  
  # Calcular las sumas necesarias
  sum_x <- sum(x)
  sum_y <- sum(y)
  sum_xy <- sum(x * y)
  sum_x_squared <- sum(x^2)
  
  # Calcular los coeficientes de la regresión
  beta <- (n * sum_xy - sum_x * sum_y) / (n * sum_x_squared - sum_x^2)
  alpha <- (sum_y - beta * sum_x) / n
  
  # Devolver una lista con los resultados
  return(list(alpha = alpha, beta = beta))
}
