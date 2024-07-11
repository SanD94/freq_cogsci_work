#include <Rcpp.h>
#include <cstdio>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector compute_power(double effect, double stddev, int nsim = 100000L, int n = 10) {
    Function qt("qt");
    Function rnorm("rnorm");
    Function t_test("t.test");
    Function mean("mean");

    double crit_t = abs(as<double>(qt(0.025, _["df"] = n - 1)));
    NumericVector power (nsim);

    for(int i = 0; i < nsim; i++) {
        NumericVector y = rnorm(n, _["mean"] = effect, _["sd"] = stddev);
        List t_eval = t_test(y);
        power[i] = as<double>(t_eval["statistic"]) > crit_t ? 1 : 0;
    }

    return mean(power);
}
