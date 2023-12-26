# Datos proporcionados
datos <- matrix(c(0.89, 2.94, 4.36, 5.21, 3.75, 1.12, 6.25, 3.14, 4.1, 1.8, 3.9, 4.27), ncol = 2, byrow = TRUE)

# Número de puntos
n_puntos <- nrow(datos)

# Crear una copia de la matriz original de distancias
matriz_distancias_original <- as.matrix(dist(datos))

# Crear una lista de clusters, cada punto es un cluster en la primera iteración
clusters <- lapply(1:n_puntos, function(i) list(etiqueta = as.character(i), elementos = i))

# Función para calcular la distancia con MIN (single link)
dist_clusters <- function(cluster1, cluster2, matriz_distancias) {
  min_dist <- min(matriz_distancias[cluster1$elementos, cluster2$elementos])
  return(min_dist)
}

# Algoritmo jerárquico aglomerativo
iteraciones <- list()
etiqueta <- 1
while (length(clusters) > 1) {
  # Inicializar la matriz de distancias entre clusters con Inf
  distancias_clusters <- matrix(Inf, nrow = length(clusters), ncol = length(clusters))
  
  # Calcular la distancia entre cada par de clusters utilizando la copia de la matriz original
  for (i in 1:(length(clusters) - 1)) {
    for (j in (i + 1):length(clusters)) {
      distancias_clusters[j, i] <- dist_clusters(clusters[[i]], clusters[[j]], matriz_distancias_original)
    }
  }
  
  # Guardar la matriz de distancias actualizada
  cat("Iteración", etiqueta, ":\n")
  cat("Matriz de distancias", etiqueta, ":\n")
  print(distancias_clusters)
  
  # Encontrar el par de clusters más cercano
  min_dist <- min(distancias_clusters)
  min_index <- which(distancias_clusters == min_dist, arr.ind = TRUE)
  
  # Unir los dos clusters más cercanos en uno nuevo
  new_cluster <- list(etiqueta = paste("C", etiqueta, sep = ""),
                      elementos = c(clusters[[min_index[1, 1]]]$elementos,
                                    clusters[[min_index[1, 2]]]$elementos))
  
  # Guardar la iteración actual
  iteraciones[[etiqueta]] <- list(cluster1 = clusters[[min_index[1, 1]]],
                                  cluster2 = clusters[[min_index[1, 2]]],
                                  nuevo_cluster = new_cluster,
                                  distancia = min_dist)
  
  # Mostrar los clusters que se unen y forman
  cat("Se unen los clusters", iteraciones[[etiqueta]]$cluster1$etiqueta,
      "y", iteraciones[[etiqueta]]$cluster2$etiqueta, "con una distancia de",
      round(iteraciones[[etiqueta]]$distancia, 2), "para formar el cluster",
      iteraciones[[etiqueta]]$nuevo_cluster$etiqueta, "\n\n")
  
  # Incrementar la etiqueta para la próxima iteración
  etiqueta <- etiqueta + 1
  
  # Eliminar los clusters antiguos
  clusters <- clusters[-c(min_index[1, 1], min_index[1, 2])]
  
  # Agregar el nuevo cluster
  clusters <- c(clusters, list(new_cluster))
}

# Imprimir el resultado final
cat("Resumen:\n")
for (i in seq_along(iteraciones)) {
  cat("Iteración", i, ": Se unen los clusters", iteraciones[[i]]$cluster1$etiqueta,
      "y", iteraciones[[i]]$cluster2$etiqueta, "con una distancia de",
      round(iteraciones[[i]]$distancia, 2), "para formar el cluster",
      iteraciones[[i]]$nuevo_cluster$etiqueta, "\n")
}





























































