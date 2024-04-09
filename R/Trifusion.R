


######## Fonction pour fusionner deux sous-tableaux  #############


Fusion <- function(gauche, droite) {
  resultat <- c()  # Crée un vecteur vide pour stocker le résultat
  while (length(gauche) > 0 && length(droite) > 0) {
    if (gauche[1] <= droite[1]) {
      resultat <- c(resultat, gauche[1])
      gauche <- gauche[-1]  # Supprime le premier élément
    } else {
      resultat <- c(resultat, droite[1])
      droite <- droite[-1]  # Supprime le premier élément
    }
  }
  # Ajoute les éléments restants (si un des deux sous-tableaux n'est pas vide)
  resultat <- c(resultat, gauche, droite)
  return(resultat)
}



#######  Fonction de tri fusion  ############

#' Tri par fusion
#'
#' Cette fonction trie un vecteur d'entiers en utilisant l'algorithme de tri fusion.
#'
#' @param arr Le vecteur d'entiers à trier.
#' @return Le vecteur d'entiers trié.
#' @details L'algorithme de tri fusion est un algorithme de tri efficace
#' et stable qui divise le vecteur en deux moitiés, trie récursivement
#' chaque moitié, puis fusionne les deux moitiés triées pour produire
#' le résultat final trié.
#' @examples
#' arr <- c(38, 27, 43, 3, 9, 82, 10)
#' TriFusion(arr)
TriFusion <- function(tableau) {
  # Condition de base : si le tableau a 1 ou aucun élément, il est déjà trié
  if (length(tableau) <= 1) {
    return(tableau)
  }
  milieu <- floor(length(tableau) / 2)  # Trouve le milieu du tableau
  gauche <- tableau[1:milieu]  # Divise le tableau en deux moitiés
  droite <- tableau[(milieu + 1):length(tableau)]

  # Tri récursivement les deux moitiés
  gauche <- TriFusion(gauche)
  droite <- TriFusion(droite)

  # Fusionne les deux moitiés triées
  resultat <- Fusion(gauche, droite)
  return(resultat)
}



