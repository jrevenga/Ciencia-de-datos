# Datos proporcionados
datos <- matrix(c(3.5, 4.5, 0.75, 3.25, 0, 3, 1.75, 0.75, 3, 3.75, 3.75, 4.5,
                  1.25, 0.75, 0.25, 3, 3.5, 4.25, 1.5, 0.5, 1, 1, 3, 4, 0.5, 3,
                  2, 0.25, 0, 2.5), ncol = 2, byrow = TRUE)

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

library(dendextend)

# Función para construir un dendrograma interactivo
build_dendrogram <- function(iteraciones) {
  dendro <- list()
  for (i in seq_along(iteraciones)) {
    dendro[[i]] <- list(
      type = "scatter",
      mode = "markers+lines",
      x = rep(iteraciones[[i]]$nuevo_cluster$etiqueta, 2),
      y = c(0, iteraciones[[i]]$distancia),
      text = paste("Cluster ", iteraciones[[i]]$nuevo_cluster$etiqueta),
      hoverinfo = "text+x+y",
      marker = list(size = 5)
    )
  }
  
  layout <- list(
    xaxis = list(title = "Clusters"),
    yaxis = list(title = "Distancia"),
    showlegend = FALSE
  )
  
  fig <- plot_ly(data = dendro, type = "scatter", mode = "markers+lines") %>% layout(layout)
  
  fig <- fig %>% layout(xaxis = list(categoryorder = "total ascending"))
  
  fig <- fig %>% layout(title = "Dendrograma Interactivo de la Clusterización")
  
  fig
}

# Construir y mostrar el dendrograma
hc <- hclust(dist(datos))
dendro <- as.dendrogram(hc)
dendro <- color_branches(dendro, k = length(iteraciones))

# Imprimir el resultado final
for (i in seq_along(iteraciones)) {
  cat("Iteración", i, ": Se unen los clusters", iteraciones[[i]]$cluster1$etiqueta,
      "y", iteraciones[[i]]$cluster2$etiqueta, "para formar el cluster",
      iteraciones[[i]]$nuevo_cluster$etiqueta, "con una distancia de",
      round(iteraciones[[i]]$distancia, 2), "\n")
}

# Mostrar el dendrograma interactivo
plot(dendro)










































































