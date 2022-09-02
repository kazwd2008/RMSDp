
# RMSDp

<!-- badges: start -->
<!-- badges: end -->

The package RMSDp contains a function RMSDp. It aims to provide a tool for multivariate outlier detection based on the Modified Stahel-Donoho estimators to cope with higher dimensional datasets than the function RMSD in the package RMSD. On the other hand, the function RMSD is faster than the function RMSDp for lower dimensional datasets.

Since the functions RMSDp uses random numbers, the results between runs could differ unless a random seed is provided using the parameter “sd.”

The default threshold set to decide outlier is 99.9 percentile point of F-statistics. It can be changed using the parameter “pt.”

This function is an improved version of “msd.parallel” at https://github.com/kazwd2008/MSD.parallel by adding the last step to decide outliers by calculating the squared Mahalanobis distance of each observation and F-statistics to decide outliers.


## Installation

You can install the released version of RMSD from [GitHub](https://github.com/kazwd2008) with:

``` r
install.packages("devtools")
devtools::install_github("kazwd2008/RMSDp")
```

## Example [13 variables]

This is an example with the wine data set from the UCI machine learning repository (https://archive.ics.uci.edu/ml/datasets/wine). Download "wine.data" in your current directory.

``` r
library(RMSD)
data(starsCYG, package="robustbase")
ot1 <- RMSD(starsCYG)

# scatterplot 
plot(starsCYG, col=ot1$ot, pch=19, main="Scatterplot (outliers are shown in red)")

# final weights
plot(ot1$wt, col=ot1$ot, pch=19, main="Degree of Outlyingness")
```

## Example 2 [five variables]

This is an example with Aircraft Data in robustbase package.

Recommended thresholds for outlier detection for MSD estimators is 99.9 percentile point of F-statistics (pt=0.999).

Without setting a same random seed, results (robust mean vector, covariance matrix, F-statistics and mahalanobis distance) of ot2 and ot3 may slightly differ.

``` r
library(RMSD)
data(aircraft, package="robustbase")

# Thresholds of outlier: 0.999 (F-statistics)
ot2 <- RMSD(aircraft, sd=2)
pairs(aircraft, pch=19, col=ot2$ot)

# Thresholds of outlier: 0.95 (F-statistics)
ot3 <- RMSD(aircraft, sd=2, pt=0.95)
pairs(aircraft, pch=19, col=ot3$ot)

# See if the results are the same
sum(abs(ot2$mah-ot3$mah))

# Effect of different thresholds
plot(ot2$FF, col=(ot2$ot+ot3$ot-1), pch=19, main="Test statistics")
abline(h=ot2$cf, col="blue", lty=3)
abline(h=ot3$cf, col="cyan", lty=3)
```
