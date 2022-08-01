Marco Cacciabue, Roman Gonzalez Mora, Pablo Aguilera

<!-- README.md is generated from README.Rmd. Please edit that file -->

# **prisoneR** <img src='man/figures/hex.png' align="right" height="200" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/marcocacciabue/prisoneR/workflows/R-CMD-check/badge.svg)](https://github.com/marcocacciabue/prisoneR/actions)
<!-- badges: end -->

The goal of prisoneR is to …

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("marcocacciabue/prisoneR")
```

## Example

This is a basic example which shows you how to a simple simalation

``` r
library(prisoneR)

## First we create a simple list of parameters to run the simulation

params<- prepare_parameters()

# we can control or modify the relevant arguments accordingly

params
#> $mutation1
#> [1] 1e-06
#> 
#> $mutation2
#> [1] 1e-06
#> 
#> $genome1
#> [1] 5000
#> 
#> $genome2
#> [1] 8500
#> 
#> $r1_1_1
#> [1] 0.9
#> 
#> $r2_1_1
#> [1] 0.19
#> 
#> $a12_1_1
#> [1] 0.012
#> 
#> $a21_1_1
#> [1] -0.2
#> 
#> $r1_1_2
#> [1] 0.1
#> 
#> $r2_1_2
#> [1] 0.0018
#> 
#> $a12_1_2
#> [1] -1
#> 
#> $a21_1_2
#> [1] 0.1
#> 
#> $r1_2_1
#> [1] 0.15
#> 
#> $r2_2_1
#> [1] 0.18
#> 
#> $a12_2_1
#> [1] -1
#> 
#> $a21_2_1
#> [1] -0.001
#> 
#> $r1_2_2
#> [1] 0.8
#> 
#> $r2_2_2
#> [1] -0.8
#> 
#> $a12_2_2
#> [1] -0.1
#> 
#> $a21_2_2
#> [1] -0.1
```

Now that we have the parameters, we can run the simulation

``` r
library(prisoneR)
simulationLV <- game(type="Lotka",
                     play1="Count_defective", #strategy of player 
                     play2="Count_defective", #strategy of player 2,#
                     parameters=params #list of parameters to pass to ode solver
 )
#> Running  Lotka  simulation for  50  generations with the following parameters 
#>  population 1 initial condition  0.1 
#>  population 2 initial condition  0.1
```

*prisoneR* includes simple helpers funtions to plot the results, for
example plotting the relative Fst:

<img src="man/figures/README-relative-1.png" width="100%" /> or the
Absolute Fst

<img src="man/figures/README-absolute-1.png" width="100%" /> We can also
run the simulation under May logistic map

``` r
library(prisoneR)
simulationMAY <- game(type="May",
                     play1="Count_defective", #strategy of player 
                     play2="Count_defective", #strategy of player 2,#
                     parameters=params #list of parameters to pass to ode solver
 )
#> Running  May  simulation for  50  generations with the following parameters 
#>  population 1 initial condition  0.1 
#>  population 2 initial condition  0.1
```

And then plot the results

<img src="man/figures/README-may-1.png" width="100%" />
