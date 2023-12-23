# Datos proporcionados
datos <- matrix(c(0.89, 2.94, 4.36, 5.21, 3.75, 1.12, 6.25, 3.14, 4.1, 1.8, 3.9, 4.27), ncol = 2, byrow = TRUE)

# Número de puntos
n_puntos <- nrow(datos)

# Crear una lista de clusters, cada punto es un cluster en la primera iteración
clusters <- vector("list", n_puntos)
for (i in 1:n_puntos) {
  clusters[[i]] <- list(etiqueta = as.character(i), elementos = i)
}

# Función para calcular la distancia euclidiana entre dos clusters
dist_clusters <- function(cluster1, cluster2) {
  min_dist <- Inf
  for (i in cluster1$elementos) {
    for (j in cluster2$elementos) {
      dist <- sqrt(sum((datos[i,] - datos[j,])^2))
      if (dist < min_dist) {
        min_dist <- dist
      }
    }
  }
  return(min_dist)
}

# Algoritmo jerárquico aglomerativo con enlace único (single link)
iteraciones <- list()
etiqueta <- 1
while (length(clusters) > 1) {
  # Inicializar la matriz de distancias entre clusters con Inf
  distancias_clusters <- matrix(Inf, nrow = length(clusters), ncol = length(clusters))
  
  # Calcular la distancia entre cada par de clusters
  for (i in 1:(length(clusters) - 1)) {
    for (j in (i + 1):length(clusters)) {
      distancias_clusters[i, j] <- dist_clusters(clusters[[i]], clusters[[j]])
    }
  }
  
  # Encontrar el par de clusters más cercano
  min_dist <- min(distancias_clusters)
  min_index <- which(distancias_clusters == min_dist, arr.ind = TRUE)
  
  # Unir los dos clusters más cercanos en uno nuevo
  new_cluster <- list(etiqueta = paste("C", etiqueta, sep = ""),
                      elementos = c(clusters[[min_index[1, 1]]]$elementos,
                                    clusters[[min_index[1, 2]]]$elementos))
  
  # Guardar la iteración actual
  iteraciones[[length(iteraciones) + 1]] <- list(cluster1 = clusters[[min_index[1, 1]]],
                                                 cluster2 = clusters[[min_index[1, 2]]],
                                                 nuevo_cluster = new_cluster,
                                                 distancia = min_dist)
  
  # Incrementar la etiqueta para la próxima iteración
  etiqueta <- etiqueta + 1
  
  # Eliminar los clusters antiguos
  clusters <- clusters[-c(min_index[1, 1], min_index[1, 2])]
  
  # Agregar el nuevo cluster
  clusters <- c(clusters, list(new_cluster))
}

# Imprimir el resultado final
for (i in seq_along(iteraciones)) {
  cat("Iteración", i, ": Se unen los clusters", iteraciones[[i]]$cluster1$etiqueta,
      "y", iteraciones[[i]]$cluster2$etiqueta, "para formar el cluster",
      iteraciones[[i]]$nuevo_cluster$etiqueta, "con una distancia de",
      iteraciones[[i]]$distancia, "\n")
}



































































