# Datos proporcionados
datos <- matrix(c(0.89, 2.94, 4.36, 5.21, 3.75, 1.12, 6.25, 3.14, 4.1, 1.8, 3.9, 4.27), ncol = 2, byrow = TRUE)

# Número de puntos
n_puntos <- nrow(datos)

# Crear una copia de la matriz original de distancias
matriz_distancias_original <- as.matrix(dist(datos))

# Crear una lista de clusters, cada punto es un cluster en la primera iteración
clusters <- lapply(1:n_puntos, function(i) list(etiqueta = as.character(i), elementos = i))

# ...

# Función para calcular la distancia con MAX (complete link)
calcular_distancia_maxima <- function(cluster1, cluster2, matriz_distancias) {
  max_dist <- max(matriz_distancias[cluster1$elementos, cluster2$elementos])
  return(max_dist)
}

# Algoritmo jerárquico aglomerativo con MAX (complete link)
iteraciones_max <- list()
etiqueta_max <- 1
while (length(clusters) > 1) {
  # Inicializar la matriz de distancias entre clusters con Inf
  distancias_clusters_max <- matrix("", nrow = length(clusters), ncol = length(clusters), dimnames = list(sapply(clusters, function(x) x$etiqueta), sapply(clusters, function(x) x$etiqueta)))
  
  # Calcular la distancia entre cada par de clusters utilizando la copia de la matriz original
  for (i in 1:(length(clusters) - 1)) {
    for (j in (i + 1):length(clusters)) {
      # Evitar imprimir Inf en la matriz
      dist_max <- calcular_distancia_maxima(clusters[[i]], clusters[[j]], matriz_distancias_original)
      distancias_clusters_max[j, i] <- if (is.finite(dist_max)) {
        # Imprimir los valores utilizados para calcular el max si hay más de un valor
        valores_usados_max <- matriz_distancias_original[clusters[[i]]$elementos, clusters[[j]]$elementos]
        if (length(valores_usados_max) > 1) {
          cat("Valores para calcular el max entre", clusters[[i]]$etiqueta, "y", clusters[[j]]$etiqueta, ":", round(valores_usados_max, 2), "\n")
        }
        round(dist_max, 2)
      } else {
        ""
      }
    }
  }
  
  # Si la matriz está completamente vacía (todos los elementos son ""), no imprimir
  if (any(distancias_clusters_max != "")) {
    # Guardar la matriz de distancias actualizada
    cat("Matriz de distancias MAX", etiqueta_max, ":\n")
    print(distancias_clusters_max, quote = FALSE)
  }
  
  # Encontrar el par de clusters más cercano
  max_dists_max <- as.numeric(distancias_clusters_max[distancias_clusters_max != ""])
  min_dist_max <- min(max_dists_max, na.rm = TRUE)
  min_index_max <- which(distancias_clusters_max == format(min_dist_max, nsmall = 2), arr.ind = TRUE)
  
  # Unir los dos clusters más cercanos en uno nuevo
  new_cluster_max <- list(etiqueta = paste("C", etiqueta_max, sep = ""),
                          elementos = c(clusters[[min_index_max[1, 1]]]$elementos,
                                        clusters[[min_index_max[1, 2]]]$elementos))
  
  # Guardar la iteración actual
  iteraciones_max[[etiqueta_max]] <- list(cluster1 = clusters[[min_index_max[1, 1]]],
                                          cluster2 = clusters[[min_index_max[1, 2]]],
                                          nuevo_cluster = new_cluster_max,
                                          distancia_max = min_dist_max)
  
  # Mostrar los clusters que se unen y forman
  cat("Se unen los clusters", iteraciones_max[[etiqueta_max]]$cluster1$etiqueta,
      "y", iteraciones_max[[etiqueta_max]]$cluster2$etiqueta, "con una distancia de",
      round(iteraciones_max[[etiqueta_max]]$distancia_max, 2), "para formar el cluster",
      iteraciones_max[[etiqueta_max]]$nuevo_cluster$etiqueta, "\n\n")
  
  # Incrementar la etiqueta para la próxima iteración
  etiqueta_max <- etiqueta_max + 1
  
  # Eliminar los clusters antiguos
  clusters <- clusters[-c(min_index_max[1, 1], min_index_max[1, 2])]
  
  # Agregar el nuevo cluster
  clusters <- append(clusters, list(new_cluster_max))
}

# Imprimir el resultado final
cat("Resumen MAX:\n")
for (i in seq_len(length(iteraciones_max))) {
  cat("Iteración", i, ": Se unen los clusters", iteraciones_max[[i]]$cluster1$etiqueta,
      "y", iteraciones_max[[i]]$cluster2$etiqueta, "con una distancia de",
      round(iteraciones_max[[i]]$distancia_max, 2), "para formar el cluster",
      iteraciones_max[[i]]$nuevo_cluster$etiqueta, "\n")
}




