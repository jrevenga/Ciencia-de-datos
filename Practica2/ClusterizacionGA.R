# Datos proporcionados
datos <- matrix(c(0.89, 2.94, 4.36, 5.21, 3.75, 1.12, 6.25, 3.14, 4.1, 1.8, 3.9, 4.27), ncol = 2, byrow = TRUE)

# Número de puntos
n_puntos <- nrow(datos)

# Crear una lista de clusters, cada punto es un cluster en la primera iteración
clusters <- vector("list", n_puntos)
for (i in 1:n_puntos) {
  clusters[[i]] <- list(etiqueta = as.character(i), elementos = i)
}

# Función para calcular la distancia "Group Average" entre dos clusters
dist_clusters_group_average <- function(cluster1, cluster2) {
  total_dist <- 0
  count_pairs <- 0
  
  for (i in cluster1$elementos) {
    for (j in cluster2$elementos) {
      total_dist <- total_dist + sqrt(sum((datos[i,] - datos[j,])^2))
      count_pairs <- count_pairs + 1
    }
  }
  
  return(total_dist / count_pairs)
}

# Utilizar la función dist_clusters_group_average
dist_clusters <- dist_clusters_group_average

# Algoritmo jerárquico aglomerativo con Group Average
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
  
  # Mostrar la matriz de distancias actual
  cat("\nMatriz de distancias en la iteración", etiqueta - 1, ":\n")
  print(round(distancias_clusters, 2))
  
  # Encontrar el par de clusters más cercano
  min_dist <- min(distancias_clusters)
  min_index <- which(distancias_clusters == min_dist, arr.ind = TRUE)
  
  # Mostrar los clusters más cercanos y su distancia
  cat("\nClusters más cercanos en la iteración", etiqueta - 1, ":\n")
  cat("  - Cluster", clusters[[min_index[1, 1]]]$etiqueta, "\n")
  cat("  - Cluster", clusters[[min_index[1, 2]]]$etiqueta, "\n")
  cat("Distancia entre clusters:", round(min_dist, 2), "\n")
  
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

# Imprimir la matriz inicial de distancias
cat("\nMatriz inicial de distancias:\n")
print(round(dist(datos), 2))

# Imprimir el resultado final
cat("\nIteraciones:\n")
for (i in seq_along(iteraciones)) {
  cat("Iteración", i, ": Se unen los clusters", iteraciones[[i]]$cluster1$etiqueta,
      "y", iteraciones[[i]]$cluster2$etiqueta, "con una distancia de",
      round(iteraciones[[i]]$distancia, 2), "para formar el cluster",
      iteraciones[[i]]$nuevo_cluster$etiqueta, "\n")
}

# Construir y mostrar el dendrograma
hc <- hclust(dist(datos))
dendro <- as.dendrogram(hc)
plot(dendro)





