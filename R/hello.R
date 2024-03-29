# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}

countingSortAlgo <- function(arr, position) {
  n <- length(arr)
  result <- numeric(n)
  count <- numeric(10) # Initialise le vecteur de comptage avec des zéros

  # Compter les occurrences de chaque chiffre
  for(j in 1:n) {
    element <- arr[j] %/% position

    count[element %% 10 + 1] <- count[element %% 10 + 1] + 1
  }
  print(count)
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

radixSortAlgo <- function(arr) {
  maximum <- max(arr) # chiffre max dans le vecteur
  position <- 1

  while(maximum %/% position > 0) { # on se déplace en fonction du max
    arr <- countingSortAlgo(arr, position)
    print(arr)
    position <- position * 10
  }

  arr
}
