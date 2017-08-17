#include <omp.h>
// [[Rcpp::plugins(openmp)]]
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
float getTeCpp(List mat) {
	NumericVector x = as<NumericVector>(mat["x"]);
	NumericVector y = as<NumericVector>(mat["y"]);
	float suma = x[0];
	int numElem = 1;
	//omp_set_num_threads(4);
	#pragma omp parallel for shared(x, y) reduction(+:suma,numElem)
	for (int i = 0; i < x.size(); i++) {
		if (y[i] > -1) {
			suma += x[i];
			numElem += 1;
		}
	}
	return suma / numElem;
}