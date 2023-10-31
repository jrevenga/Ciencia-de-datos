kvecinos <- function(matriz){
    # Traspuesta
    matrizt <- t(matriz)
    # Numero de filas
    n <- nrow(matrizt)
    # Matriz distancias
    distancias <- matrix(0, n, n)
    
    # Calculo distancias euclideas
    for (i in 1:n) {
        for (j in 1:n) {
            if (i != j) {
                distancias[i, j] <- distEuc(matrizt[i, ],matrizt[j, ])
            }
        }
    }

    # Ordenacion de las distancias
    for(i in 1:5){
        distancias[,i]=sort(distancias[,i])
    } 
    
    distanciasordenadas=distancias
    print(distanciasordenadas)

    # Calculo de los outliers
    for (i in 1:5) {
        if (distanciasordenadas[4,i]>2.5) {
            print(i)
            print("Es un suceso anómalo")
        }
    }
}

distEuc <- function(x1, x2) {
    # Calcular la distancia euclidiana
    distancia <- sqrt(sum((x1 - x2)^2))
    
    return(distancia)
}