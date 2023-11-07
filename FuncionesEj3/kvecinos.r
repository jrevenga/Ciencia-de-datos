kvecinos <- function(matriz,k,d){
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
                distancias[i, j] <- round(distEuc(matrizt[i, ],matrizt[j, ]),2)
            }
        }
    }

    # Ordenacion de las distancias
    for(i in 1:n){
        distancias[,i]=sort(distancias[,i])
    } 
    
    distanciasordenadas <- distancias
    print(distanciasordenadas)

    # Calculo de los outliers
    for (i in 1:n) {
        if (distanciasordenadas[k,i]>d) {
            print(paste("Para k =",k," el suceso ",i," es anómalo"))
        }
    }
}

distEuc <- function(x1, x2) {
    # Calcular la distancia euclidiana
    distancia <- sqrt(sum((x1 - x2)^2))
    
    return(distancia)
}