
#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
NumericVector rcpp_fusion(NumericVector gauche, NumericVector droite) {
  int n_gauche = gauche.size(), n_droite = droite.size();
  NumericVector resultat(n_gauche + n_droite);

  int index_g = 0, index_d = 0, index_r = 0;
  while (index_g < n_gauche && index_d < n_droite) {
    if (gauche[index_g] <= droite[index_d]) {
      resultat[index_r++] = gauche[index_g++];
    } else {
      resultat[index_r++] = droite[index_d++];
    }
  }

  while (index_g < n_gauche) {
    resultat[index_r++] = gauche[index_g++];
  }

  while (index_d < n_droite) {
    resultat[index_r++] = droite[index_d++];
  }

  return resultat;
}


//' Tri par fusion (implémenté en C++)
//'
//' Cette fonction trie un vecteur d'entiers en utilisant l'algorithme de tri fusion.
//'
//' @param tableau Le vecteur d'entiers à trier.
//' @return Le vecteur d'entiers trié.
//' @details L'algorithme de tri fusion est un algorithme de tri efficace
//' et stable qui divise le vecteur en deux moitiés, trie récursivement
//' chaque moitié, puis fusionne les deux moitiés triées pour produire
//' le résultat final trié.
//' @examples
//' tableau <- c(38, 27, 43, 3, 9, 82, 10)
//' rcpp_TriFusion(tableau)
//' @export
// [[Rcpp::export]]
NumericVector rcpp_TriFusion(NumericVector tableau) {
  int n = tableau.size();
  if (n <= 1) {
    return tableau;
  }

  int milieu = n / 2;
  NumericVector gauche = tableau[Range(0, milieu - 1)];
  NumericVector droite = tableau[Range(milieu, n - 1)];

  gauche = rcpp_TriFusion(gauche);
  droite = rcpp_TriFusion(droite);

  return rcpp_fusion(gauche, droite);
}


