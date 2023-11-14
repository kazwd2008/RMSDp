# RMSDp

<!-- badges: start -->
<!-- badges: end -->

The package RMSDp contains a function RMSDp. It aims to provide a tool for multivariate outlier detection based on the Modified Stahel-Donoho estimators to cope with higher dimensional datasets than the function RMSD in the package RMSD. On the other hand, the function RMSD is faster than the function RMSDp for lower dimensional datasets.

The parameter 'cores' is for controlling number of cores used for the computation.  If this parameter is omitted, all the cores are used.

Since the functions RMSDp uses random numbers, the results between runs could differ unless a random seed is provided using the parameter 'sd.'

The default threshold set to decide outlier is 99.9 percentile point of F-statistics. It can be changed using the parameter 'pt.'

This function is an improved version of 'msd.parallel' at https://github.com/kazwd2008/MSD.parallel by adding the last step to decide outliers by calculating the squared Mahalanobis distance of each observation and F-statistics to decide outliers.


## Installation


### from CRAN

The package RMSDP is on CRAN.

``` r
install.packages("RMSDps")
library(RMSDp)
```

### from github

You can also install the package from [GitHub](https://github.com/kazwd2008) with:

``` r
install.packages("devtools")
devtools::install_github("kazwd2008/RMSDp")
```

## Example: wine data [13 variables]

This is an example with the wine data set from the UCI machine learning repository (https://archive.ics.uci.edu/ml/datasets/wine). Download 'wine.data' in your current directory. The dataset contains 1 categorical variable and 13 numerical variables.

RMSEp assumes an elliptical data distribution.

``` r
# number of cores of your PC
getOption("mc.cores", parallel::detectCores())

library(RMSDp)
wine <- read.csv("wine.data", header=F)
ot1 <- RMSDp(wine[,-1], cores=2)

# scatterplot with the outlier flag
plot(wine, col=ot1$ot, pch=19, main="Scatterplot (outliers are shown in red)")

# final weights
plot(ot1$wt, pch=19, col=ot1$ot)

# parallel coordinate plot
MASS::parcoord(wine[,-1], col=ot1$ot, lty=c(3,1)[ot1$ot], lwd=c(1,2)[ot1$ot])

```
