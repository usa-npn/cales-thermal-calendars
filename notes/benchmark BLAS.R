# https://cran.r-project.org/bin/macosx/RMacOSX-FAQ.html#Which-BLAS-is-used-and-how-can-it-be-changed_003f
library(microbenchmark)
d <- 1e3
x <- matrix(rnorm(d^2), d, d)
microbenchmark(tcrossprod(x), solve(x), svd(x), times = 10L)
# Unit: milliseconds
#          expr       min        lq      mean    median        uq       max neval
# tcrossprod(x)  10.28771  10.43338  14.44232  12.21282  18.69042  24.71473    10
#      solve(x)  50.95068  62.08074  88.06996  80.17619 110.93473 149.82411    10
#        svd(x) 408.90578 438.84437 508.58660 517.46735 556.94280 641.92371    10