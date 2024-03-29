


######## Fonction pour fusionner deux sous-tableaux  #############


fusion <- function(gauche, droite) {
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



triFusion <- function(tableau) {
  # Condition de base : si le tableau a 1 ou aucun élément, il est déjà trié
  if (length(tableau) <= 1) {
    return(tableau)
  }
  milieu <- floor(length(tableau) / 2)  # Trouve le milieu du tableau
  gauche <- tableau[1:milieu]  # Divise le tableau en deux moitiés
  droite <- tableau[(milieu + 1):length(tableau)]

  # Tri récursivement les deux moitiés
  gauche <- triFusion(gauche)
  droite <- triFusion(droite)

  # Fusionne les deux moitiés triées
  resultat <- fusion(gauche, droite)
  return(resultat)
}



