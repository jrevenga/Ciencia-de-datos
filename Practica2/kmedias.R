kmedias <- function(datos, centroides = NULL, num_clusters = 2, max_iter = 100, mostrar_proceso = FALSE) {
  # A. Inicialización
  if (is.null(centroides)) {
    # Seleccionar los primeros num_clusters datos como centroides
    centroides <- datos[1:num_clusters, ]
  }
  
  #Inicialización de vectores para almacenar la asignación de cluster, los centroides antiguos y las distancias a los centroides
  clusters <- rep(0, nrow(datos))
  old_centroides <- matrix(0, nrow = nrow(centroides), ncol = ncol(centroides))
  distancias <- matrix(0, nrow = nrow(datos), ncol = num_clusters)
  
  # Iteraciones
  for (iter in 1:max_iter) {
    # B. Cálculo de la distancia euclidiana y C. asignación al cluster del centroide
    for (i in 1:nrow(datos)) {
      distancias[i, ] <- sapply(1:num_clusters, function(j) distEuc(datos[i, ], centroides[j, ]))
      clusters[i] <- which.min(distancias[i, ])
    }

    if(mostrar_proceso == TRUE){
      #Mostrar la iteración en la que estamos
      cat("Iteración: ", iter, "\n")

      # Mostrar la matriz de distancias
      cat("Matriz de Distancias:\n")
      print(distancias)

      # Mostrar clusters asignados en esta iteración
      cat("Clusters Asignados:\n")
      print(clusters)
    }

    # A. Recálculo de los centroides
    old_centroides <- centroides

    # Calcular nuevos centroides
    centroides <- matrix(0, nrow = nrow(old_centroides), ncol = ncol(old_centroides))

    for (j in 1:num_clusters) {
      # Extraer los valores de los datos asignados a cada cluster
      cluster_indices <- clusters == j
      cluster_data <- datos[cluster_indices, ]
      
      # Para cada cluster
      if (nrow(cluster_data) > 0) {
        # Calcular la media 
        for (col in 1:ncol(cluster_data)) {
          centroides[j, col] <- meanr(cluster_data[, col])
        }
      } else {
        warning("Cluster", j, " está vacío.")
      }
    }
  
    if(mostrar_proceso == TRUE){
      # Mostrar nuevos centroides
      cat("Nuevos centroides:", "\n")
      print(centroides)
    }
    
    # C. Recálculo de los clusters en función de los centroides
    for (i in 1:nrow(datos)) {
      distancias[i,] <- sapply(1:num_clusters, function(j) distEuc(datos[i, ], centroides[j, ]))
      clusters[i] <- which.min(distancias[i, ])
    }

    
    # Verificar convergencia
    if (is.logical(all.equal(old_centroides, centroides)) && all.equal(old_centroides, centroides)) {
      cat("Convergencia alcanzada en la iteración:", iter, "\n")
      break
    }

  }
  
  # Retornar resultado
  return(list(cluster = clusters, centers = centroides))
}

# Distancia euclidea
distEuc <- function(x1, x2) {
    # Calcular la distancia euclidiana
    distancia <- sqrt(sum((x1 - x2)^2))
    
    return(distancia)
}

# MEDIA
meanr <- function(vector) {
  if (length(vector) < 1) {
    stop("Error, el vector está vacío")
  }
  
  sum <- 0
  # Suma de los elementos del vector
  for (value in vector) {
    sum <- sum + value
  }
    
  # Calculo de la media
  mean <- sum / length(vector)
    
  return(mean)
}