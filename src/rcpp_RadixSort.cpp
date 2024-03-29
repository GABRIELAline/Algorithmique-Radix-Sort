#include <Rcpp.h>

using namespace Rcpp;
// This is a simple function using Rcpp that creates an R list
// containing a character vector and a numeric vector.
//
// Learn more about how to use Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//
// and browse examples of code using Rcpp at:
//
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
List rcpp_hello() {
  CharacterVector x = CharacterVector::create("foo", "bar");
  NumericVector y   = NumericVector::create(0.0, 1.0);
  List z            = List::create(x, y);
  return z;
}



// Fonction de tri Counting Sort
// Cette fonction trie un vecteur d'entiers en utilisant l'algorithme Counting Sort
// arr : vecteur d'entiers à trier
// position : position actuelle dans le nombre (1, 10, 100, etc.)
// [[Rcpp::export]]
IntegerVector countingSortAlgoRcpp(IntegerVector arr, int position) {
  int n = arr.size();
  IntegerVector result(n);
  IntegerVector count(10);

  // Compter les occurrences de chaque chiffre
  for(int j = 0; j < n; ++j) {
    int element = arr[j] / position % 10;
    count[element]++;
  }

  // Comptage cumulatif
  for(int j = 1; j < 10; ++j) {
    count[j] += count[j - 1];
  }

  // Placer les éléments dans l'ordre trié
  for(int i = n - 1; i >= 0; --i) {
    int element = arr[i] / position % 10;
    result[count[element] - 1] = arr[i];
    count[element]--;
  }

  // Copier les éléments triés dans le tableau original
  for(int j = 0; j < n; ++j) {
    arr[j] = result[j];
  }

  return arr;
}

// Fonction de tri Radix Sort
// Cette fonction trie un vecteur d'entiers en utilisant l'algorithme Radix Sort
// arr : vecteur d'entiers à trier
// [[Rcpp::export]]
IntegerVector radixSortAlgoRcpp(IntegerVector arr) {
  int maximum = max(arr); // chiffre max dans le vecteur
  int position = 1;

  while(maximum / position > 0) { // on se déplace en fonction du max
    arr = countingSortAlgoRcpp(arr, position);
    position *= 10;
  }

  return arr;
}



