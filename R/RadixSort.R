CountingSort <- function(arr, position) {
  n <- length(arr)
  result <- numeric(n)
  count <- numeric(10) # Initialise le vecteur de comptage avec des zéros

  # Compter les occurrences de chaque chiffre
  for(j in 1:n) {
    element <- arr[j] %/% position

    count[element %% 10 + 1] <- count[element %% 10 + 1] + 1
  }
  # Comptage cumulatif
  for(j in 2:10) {
    count[j] <- count[j] + count[j - 1]
  }

  # Placer les éléments dans l'ordre trié
  for(i in n:1) {
    element <- arr[i] %/% position
    result[count[element %% 10 + 1]] <- arr[i]
    count[element %% 10 + 1] <- count[element %% 10 + 1] - 1
  }

  # Copier les éléments triés dans le tableau original
  for(j in 1:n) {
    arr[j] <- result[j]
  }

  arr
}

#' Tri par Radix Sort
#'
#' Cette fonction trie un vecteur d'entiers en utilisant l'algorithme Radix Sort.
#'
#' @param arr Le vecteur d'entiers à trier.
#' @return Le vecteur d'entiers trié.
#' @details L'algorithme Radix Sort est un algorithme de tri non comparatif
#' qui trie les éléments en examinant les chiffres de leurs représentations
#' décimales.
#' @examples
#' arr <- c(170, 45, 75, 90, 802, 24, 2, 66)
#' RadixSort(arr)
RadixSort <- function(arr) {
  maximum <- max(arr) # chiffre max dans le vecteur
  position <- 1

  while(maximum %/% position > 0) { # on se déplace en fonction du max
    arr <- CountingSort(arr, position)
    position <- position * 10
  }

  return(arr)
}


