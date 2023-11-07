# Recibe la matriz, el nombre de la fila que quiere evaluar y el valor d
cajaBigotes <- function(matriz,fila,d){
    # Traspuesta
    matrizt <- t(matriz)
    # Dataframe
    matrizt <- data.frame(matrizt)

    # Obtiene indice de la fila a evaluar
    fila <- which(colnames(matrizt) == fila)

    # Calculo de 1er y 3er cuartil de los valores de dicha fila
    cuart1 <- quantiler(matrizt[,fila],0.25)
    cuart3 <- quantiler(matrizt[,fila],0.75)

    # Calculo de los limites
    limites <- c(cuart1-d*(cuart3-cuart1),cuart3+d*(cuart3-cuart1))

    # Calculo de los outliers
    for(i in 1:length(matrizt$d)) {
        # Si el punto se encuentra fuera de los limites es outlier
        if(matrizt[i,fila]<limites[1] || matrizt[i,fila]>limites[2]) {
            print(paste("El suceso", i, ":", matrizt[i, fila], "es un dato anómalo"))
        }
    }
}