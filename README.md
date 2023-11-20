
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lrm2

<!-- badges: start -->

[![R-CMD-check](https://github.com/Xintong-ZHAI/lrm2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Xintong-ZHAI/lrm2/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of lrm is to estimate the parameters and test the significance
of any given matrix X and vector Y by linear regression model. It
conducts the hypothesis testing, including F test, partial t test and
general linear hypothesis.

Compared with lm function, lrm use Crossprod function to accelerate the
speed of matrix calculation, some details will be shown below.

## Installation

You can install the development version of lrm2 from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Xintong-ZHAI/lrm2")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(lrm2)
```

Use mtcars dataset as an example to show the details of lrm library.

The default function is use lrm to estimate the parameters of a linear
regression model with intercept.

``` r
X.vec <- mtcars$wt
Y <- mtcars$mpg
m <- lrm(Y, X.vec)
m
#>       Estimate  Std.Err
#> [1,] 37.285126 1.877627
#> [2,] -5.344472 0.559101
```

Set Ftest=TRUE to test the significance of the whole model.

``` r
m <- lrm(Y, X.vec, estimate=FALSE, anova=TRUE, Ftest=TRUE)
m
#>              SS df         MS F_statistic      p_value
#> model  847.7252  1 847.725250    91.37533 1.293958e-10
#> error  278.3219 30   9.277398          NA           NA
#> total 1126.0472 31         NA          NA           NA
```

Set partialtest=TRUE to test the significance of a selected parameter.

``` r
m <- lrm(Y, X.vec, test.variable=2, estimate=FALSE, partialtest=TRUE)
m
#>       Estimate  Std.Err t_statistic      p_value
#> [1,] -5.344472 0.559101   -9.559044 1.293958e-10
```

Set GLH=TRUE to test the significance of a subset of parameters

``` r
m <- lrm(Y, X.vec, test.matrix=matrix(c(0,1),nrow=1,ncol=2), estimate=FALSE, GLH=TRUE)
m
#>      F_statistic       pvalue
#> [1,]    91.37533 1.293958e-10
```

## Comparison with lm

Compared with the most popular lm function, all the test results are the
same, the running time is shorter and lrm also gives the direct solution
of GLH, which is not included in lm function.

``` r
summary(lm(mpg~wt, data=mtcars))
#> 
#> Call:
#> lm(formula = mpg ~ wt, data = mtcars)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -4.5432 -2.3647 -0.1252  1.4096  6.8727 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  37.2851     1.8776  19.858  < 2e-16 ***
#> wt           -5.3445     0.5591  -9.559 1.29e-10 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 3.046 on 30 degrees of freedom
#> Multiple R-squared:  0.7528, Adjusted R-squared:  0.7446 
#> F-statistic: 91.38 on 1 and 30 DF,  p-value: 1.294e-10
anova(lm(mpg~wt, data=mtcars))
#> Analysis of Variance Table
#> 
#> Response: mpg
#>           Df Sum Sq Mean Sq F value    Pr(>F)    
#> wt         1 847.73  847.73  91.375 1.294e-10 ***
#> Residuals 30 278.32    9.28                      
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
system.time(summary(lm(mpg~wt, data=mtcars)))
#>    user  system elapsed 
#>   0.001   0.000   0.001
system.time(lrm(Y, X.vec))
#>    user  system elapsed 
#>       0       0       0
```
