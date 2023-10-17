# FRECUENCIA ABSOLUTA
frecAbsr <- function(vector) {
  if (length(vector) < 1){
    stop("Error, el vector está vacío")
  }

  # Lista de frecuencias vacias
  v_frecs <- list()
  
  # Conteo de las frecuencias de cada valor
  for (value in vector) {
    if (value %in% names(v_frecs)) {
      v_frecs[[as.character(value)]] <- v_frecs[[as.character(value)]] + 1
    } else {
      v_frecs[[as.character(value)]] <- 1
    }
  }

  # Marco de datos con las frecuencias absolutas
  res <- data.frame(Valor = as.numeric(names(v_frecs)), FrecAbs = unname(unlist(v_frecs)))

  return(res)
}

# FRECUENCIA ABSOLUTA ACUMULADA
frecAbsAcumr <- function(vector) {
  # Frecuencias absolutas del vector
  frec_Abs <- frecAbsr(vector)

  # Inicializar la frecuencia acumulada
  frec_Abs$FrecAbsAcum <- rep(0, nrow(frec_Abs))

  # Calculo de las frecuencias absolutas acumuladas
  for (i in seq_along(frec_Abs$FrecAbsAcum)) {
    frec_Abs$FrecAbsAcum[i] <- sum(frec_Abs$FrecAbs[1:i])
  }

  # Marco de datos con las frecuencias absolutas acumuladas
  res <- data.frame(Valor = frec_Abs$Valor, FrecAbsAcum = frec_Abs$FrecAbsAcum)

  return(res)
}

# FRECUENCIA RELATIVA
frecRelr <- function(vector) {
  # Frecuencias absolutas del vector
  frec_Abs <- frecAbsr(vector)
  
  # Calculo de frecuencias relativas
  frec_Rel <- frec_Abs$FrecAbs / length(vector)
  frec_Abs$FrecRel <- frec_Rel
  
  # Marco de datos con las frecuencias relativas
  res <- data.frame(Valor = frec_Abs$Valor, FrecRel = frec_Abs$FrecRel)

  return(res)
}

# FRECUENCIA RELATIVA ACUMULADA
frecRelAcumr <- function(vector) {
  # Frecuencias relativas del vector
  frec_Rel <- frecRelr(vector)

  # Inicializar la frecuencia acumulada
  frec_Rel$FrecRelAcum <- rep(0, nrow(frec_Rel))

  # Calculo de las frecuencias relativas acumuladas
  for (i in seq_along(frec_Rel$FrecRelAcum)) {
    frec_Rel$FrecRelAcum[i] <- sum(frec_Rel$FrecRel[1:i])
  }

  # Marco de datos con las frecuencias relativas acumuladas
  res <- data.frame(Valor = frec_Rel$Valor, frecRelAcum = frec_Rel$FrecRelAcum)

  return(res)
}

# MEDIA
meanr <- function(vector) {
  if (length(vector) < 1) {
    stop("Error, el vector está vacío")
  }
  
  sum <- 0
  # Suma de los elementos del vector
  for (value in vector) {
    sum <- sum + value
  }
    
  # Calculo de la media
  mean <- sum / length(vector)
    
  return(mean)
}

# MEDIANA
medianr <- function(vector) {
  n <- length(vector)
  v <- sort(vector)

  # Comprobar si el numero de elementos del vector es impar o par
  if(n%%2==1) {
    # Si es impar la mediana es el elemento del medio
    median <- v[(n+1)/2]
  }else {
    # Si es par la mediana es la media de los 2 elementos del medio
    median <- (v[n/2] + v[(n/2)+1]) / 2
  }

  return(median)
}

# RANGO
ranger <- function(vector) {
  max_value <- vector[1]
  min_value <- vector[1]
  
  # Encontrar el máximo y mínimo recorriendo el vector
  for (value in 2:length(vector)) {
    if (vector[value] > max_value) {
      max_value <- vector[value]
    }
    if (vector[value] < min_value) {
      min_value <- vector[value]
    }
  }
  
  # Calcular el rango
  range <- max_value - min_value
  
  return(range)
}

# DESVIACION ESTANDAR
sdr <- function(vector) {
    if (length(vector) < 1){
        stop("Error, el vector está vacío")
    }
    
    # Calculo de la media de los elementos del vector
    mean <- meanr(vector)

    # Suma de cuadrados
    for (value in vector) {
        sum <- (value-mean)^2
    }
    
    # Calculo desviacion estandar
    sd <- sqrt(sum/length(vector))

    return(sd)
}

# VARIANZA
varr <- function(values) {
    # Calculo desviacion estandar
    sd <- sdr(values)

    # Calculo varianza
    var <- sd^2

    return(var)
}

# CUANTILES
quantiler <- function(values, p) {
  n <- length(values)
  
  if (n == 0) {
    stop("El vector no puede estar vacío para calcular cuantiles.")
  }
  
  if (p < 0 || p > 1) {
    stop("El cuantil debe estar en el rango de 0 a 1.")
  }
  
  v <- sort(values)
  
  # Calcular posición del cuantil
  pos <- 1 + (n - 1) * p
  
  # Calcular cuantil interpolando
  int <- floor(pos)
  float <- pos - int
  
  quantile <- (1 - float) * v[int] + float * v[int + 1]
  
  return(quantile)
}
