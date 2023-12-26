# Datos proporcionados
datos <- matrix(c(3.5, 4.5, 0.75, 3.25, 0, 3, 1.75, 0.75, 3, 3.75,
                  3.75, 4.5, 1.25, 0.75, 0.25, 3, 3.5, 4.25, 1.5, 0.5,
                  1, 1, 3, 4, 0.5, 3, 2, 0.25, 0, 2.5), ncol = 2, byrow = TRUE)

# Número de puntos
n_puntos <- nrow(datos)

# Crear una copia de la matriz original de distancias
matriz_distancias_original <- as.matrix(dist(datos))

# Crear una lista de clusters, cada punto es un cluster en la primera iteración
clusters <- lapply(1:n_puntos, function(i) list(etiqueta = as.character(i), elementos = i))

# Función para calcular la distancia con MAX (complete link)
calcular_distancia_maxima <- function(cluster1, cluster2, matriz_distancias) {
  max_dist <- max(matriz_distancias[cluster1$elementos, cluster2$elementos])
  return(max_dist)
}

# Algoritmo jerárquico aglomerativo con MAX (complete link)
iteraciones <- list()
etiqueta <- 1
while (length(clusters) > 1) {
  # Inicializar la matriz de distancias entre clusters con Inf
  distancias_clusters <- matrix("", nrow = length(clusters), ncol = length(clusters), dimnames = list(sapply(clusters, function(x) x$etiqueta), sapply(clusters, function(x) x$etiqueta)))
  
  # Calcular la distancia entre cada par de clusters utilizando la copia de la matriz original
  for (i in 1:(length(clusters) - 1)) {
    for (j in (i + 1):length(clusters)) {
      # Evitar imprimir Inf en la matriz
      dist <- calcular_distancia_maxima(clusters[[i]], clusters[[j]], matriz_distancias_original)
      distancias_clusters[j, i] <- if (is.finite(dist)) {
        # Imprimir los valores utilizados para calcular el max entre los clusters
        valores_usados <- matriz_distancias_original[clusters[[i]]$elementos, clusters[[j]]$elementos]
        if (length(valores_usados) > 1) {
          cat("Valores para calcular el max entre", clusters[[i]]$etiqueta, "y", clusters[[j]]$etiqueta, ":", round(valores_usados, 2), "\n")
        }
        round(dist, 2)
      } else {
        ""
      }
    }
  }
  
  # Si la matriz está completamente vacía (todos los elementos son ""), no imprimir
  if (any(distancias_clusters != "")) {
    # Guardar la matriz de distancias actualizada
    cat("Matriz de distancias", etiqueta, ":\n")
    print(distancias_clusters, quote = FALSE)
  }
  
  # Encontrar el par de clusters más cercano
  max_dists <- as.numeric(distancias_clusters[distancias_clusters != ""])
  min_dist <- min(as.numeric(distancias_clusters), na.rm = TRUE)
  min_index <- which(distancias_clusters == min_dist, arr.ind = TRUE)
  
  # Imprimir la distancia seleccionada y opciones
  cat("En la iteración", etiqueta, "se elige la distancia", round(min_dist, 2), "de entre las opciones:", round(max_dists, 2), "\n")
  
  # Unir los dos clusters más cercanos en uno nuevo
  new_cluster <- list(etiqueta = paste("C", etiqueta, sep = ""),
                      elementos = c(clusters[[min_index[1]]]$elementos,
                                    clusters[[min_index[2]]]$elementos))
  
  # Guardar la iteración actual
  iteraciones[[etiqueta]] <- list(cluster1 = clusters[[min_index[1]]],
                                  cluster2 = clusters[[min_index[2]]],
                                  nuevo_cluster = new_cluster,
                                  distancia = min_dist)
  
  # Mostrar los clusters que se unen y forman
  cat("Se unen los clusters", iteraciones[[etiqueta]]$cluster1$etiqueta,
      "y", iteraciones[[etiqueta]]$cluster2$etiqueta, "con una distancia de",
      round(iteraciones[[etiqueta]]$distancia, 2), "para formar el cluster",
      iteraciones[[etiqueta]]$nuevo_cluster$etiqueta, "\n")
  cat("-----------------\n\n")
  
  # Incrementar la etiqueta para la próxima iteración
  etiqueta <- etiqueta + 1
  
  # Eliminar los clusters antiguos
  clusters <- clusters[-c(min_index[1], min_index[2])]
  
  # Agregar el nuevo cluster
  clusters <- append(clusters, list(new_cluster))
}

# Imprimir el resultado final
cat("Resumen:\n")
for (i in seq_len(length(iteraciones))) {
  cat("Iteración", i, ": Se unen los clusters", iteraciones[[i]]$cluster1$etiqueta,
      "y", iteraciones[[i]]$cluster2$etiqueta, "con una distancia de",
      round(iteraciones[[i]]$distancia, 2), "para formar el cluster",
      iteraciones[[i]]$nuevo_cluster$etiqueta, "\n")
}



