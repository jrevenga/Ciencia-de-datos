# Recibe la matriz, el nombre de la fila que quiere evaluar y el valor d
desvTip <- function(matriz,fil,d){
    # Traspuesta
    matrizt <- t(matriz)
    # Dataframe
    matrizt <- data.frame(matrizt)

    # Obtiene indice de la fila a evaluar
    fila <- which(colnames(matrizt) == fil)

    # Calculo de la media de los valores de dicha fila
    media <- meanr(matrizt[,fila])
    # Calculo de la desviacion de los valores de dicha fila
    sd <- sdr(matrizt[,fila])

    # Calculo de los limites
    limites <- c(media-d*sd,media+d*sd)

    # Calculo de los outliers
    for(i in 1:length(matrizt[,fila])) { 
        # Si el punto se encuentra fuera de los limites es outlier
        if(matrizt[i,fila]<lim[1] || matrizt[i,fila]>lim[2]) { 
            print(paste("El suceso", i, ":", matrizt[, fila][i], "es un dato anómalo"))
        }
    }
}