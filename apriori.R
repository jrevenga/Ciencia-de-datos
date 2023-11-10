# Define los sucesos
transactions <- list(
  c("Pan", "Agua", "Leche", "Naranjas"),
  c("Pan", "Agua", "Café", "Leche"),
  c("Pan", "Agua", "Leche"),
  c("Pan", "Café", "Leche"),
  c("Pan", "Agua"),
  c("Leche")
)

# Define soporte y confianza
soporte <- 0.5
confianza <- 0.8

calculate_support <- function(itemset, transactions) {
  # Función para calcular el soporte de un conjunto de elementos en las transacciones
  count <- sum(sapply(transactions, function(transaction) all(itemset %in% transaction)))
  return(count / length(transactions))
}

filtar_sucesos_elementales <- function(transactions, soporte) {
  # Obtener todos los elementos únicos en los sucesos
  sucesos_elementales <- unique(unlist(transactions))

  # Inicializar lista para almacenar sucesos después de aplicar el umbral de soporte
  sucesos_filtrados <- list()

  # Calcular el soporte para cada elemento y filtrar
  for (suceso in sucesos_elementales) {
    soporte_suceso <- calculate_support(c(suceso), transactions)
    if (soporte_suceso >= soporte) {
      sucesos_filtrados <- c(sucesos_filtrados, list(c(suceso)))
    }
  }

  return(sucesos_filtrados)
}

apriori_gen <- function(conjuntos_anteriores, k) {
  sucesos_candidatos <- list()

  if (length(conjuntos_anteriores) < 2) {
    return(sucesos_candidatos)
  }

  for (i in 1:(length(conjuntos_anteriores) - 1)) {
      for (j in (i + 1):length(conjuntos_anteriores)) {
        conjunto_a <- conjuntos_anteriores[[i]]
        conjunto_b <- conjuntos_anteriores[[j]]

      if (k == 2) {
        # Generación de candidatos para k = 2
        if (conjunto_a[1] != conjunto_b[1]) {
          nuevo_conjunto <- c(conjunto_a, conjunto_b[1])
          sucesos_candidatos <- append(sucesos_candidatos, list(nuevo_conjunto))
        }
      } else if (k > 2) {
        # Generación de candidatos para k > 2
        if (identical(conjunto_a[1:(k-2)], conjunto_b[1:(k-2)]) && conjunto_a[(k-1)] != conjunto_b[(k-1)]) {
          nuevo_conjunto <- c(conjunto_a, conjunto_b[(k-1)])
          sucesos_candidatos <- append(sucesos_candidatos, list(nuevo_conjunto))
        }
      }
    }
  }

  # Llamada recursiva
  sucesos_candidatos_recursivo <- apriori_gen(sucesos_candidatos, k + 1)
  sucesos_candidatos <- append(sucesos_candidatos, sucesos_candidatos_recursivo)

  return(sucesos_candidatos)
}

filtrar_sucesos_con_hash_tree <- function(sucesos_candidatos, transactions) {
  sucesos_filtrados <- list()

  for (suceso in sucesos_candidatos) {
    soporte_suceso <- calculate_support(suceso, transactions)
    if (soporte_suceso >= soporte) {
      sucesos_filtrados <- c(sucesos_filtrados, list(suceso))
    }
  }

  return(sucesos_filtrados)
}

calcular_confianza <- function(regla, transactions) {
  antecedente <- regla$antecedente
  consecuente <- regla$consecuente

  soporte_regla <- calculate_support(c(antecedente, consecuente), transactions)
  soporte_antecedente <- calculate_support(antecedente, transactions)

  confianza_regla <- soporte_regla / soporte_antecedente

  return(confianza_regla)
}

ap_genrules <- function(sucesos_filtrados, transactions, confianza) {
  reglas <- list()

  for (i in 1:length(sucesos_filtrados)) {
    suceso <- sucesos_filtrados[[i]]
    k <- length(suceso)

    if (k >= 2) {
      conjuntos_anteriores <- combn(suceso, k - 1, simplify = FALSE)

      for (conjunto_anterior in conjuntos_anteriores) {
        antecedente <- conjunto_anterior
        consecuente <- setdiff(suceso, antecedente)

        confianza_regla <- calcular_confianza(list(antecedente = antecedente, consecuente = consecuente), transactions)

        if (confianza_regla >= (confianza-0.001)) {
          reglas <- append(reglas, list(list(antecedente = antecedente, consecuente = consecuente, soporte = calculate_support(suceso, transactions), confianza = confianza_regla)))
        }
      }
    }
  }

  return(reglas)
}


apriori <- function(transactions, soporte, confianza) {
  # Paso A:
    # Paso A.1: Filtrar sucesos elementales que llegan al soporte
    sucesos_elem_candidatos <- filtar_sucesos_elementales(transactions, soporte)
    # Paso A.2:
      # Paso A.2.1: Identificar sucesos candidatos en cada dimensión
      sucesos_candidatos <- apriori_gen(sucesos_elem_candidatos, 2)
      # Paso A.2.2: Calcular soporte de los sucesos candidatos y filtrar
      sucesos_filtrados <- filtrar_sucesos_con_hash_tree(sucesos_candidatos, transactions)

  # Paso B: Aplicar la función ap-genrules para establecer asociaciones con el umbral de confianza
  rules <- ap_genrules(sucesos_filtrados, transactions, confianza)

  return(rules)
}

# Llamar a la función Apriori
resultados <- apriori(transactions, soporte, confianza)

# Imprimir las reglas de asociación
for (i in 1:length(resultados)) {
  antecedente <- unlist(resultados[[i]]$antecedente)
  consecuente <- unlist(resultados[[i]]$consecuente)
  soporte <- resultados[[i]]$soporte
  confianza <- resultados[[i]]$confianza
  
  cat(
    sprintf("[%d] {%s} => {%s}  support: %.7f  confidence: %.7f\n",
            i, paste(antecedente, collapse = ", "), paste(consecuente, collapse = ", "),
            soporte, confianza)
  )
}