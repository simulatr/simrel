# Simulation of Multivariate Linear Model Data

## Introduction
`Simrel` r-package is a versatile tool for simulation of multivariate linear model data. The package consist of four core functions -- `simrel`, `simrel2`, `simrel_m` and `simulatr` for simulation and a plot function `plot.simulatr`. As the name suggests, `simrel` function is used for simulating univariate linear model data, `simrel2` simulates bivariate linear model data where user can specify the correlation between two responses with and without given $\bf{X}$. In addition, this function allows users to get responses ($y$) having common relevant components.

An extension of `simrel2` is `simrel_m`, by which user can simulate multivariate linear model data with multiple responses. In this simulation, each response must have exclusive set of predictors and relevant predictors components. Following examples will give a clear picture of these functions. The forth function `simulatr` wraps around these function and calls them according to what type of data a user is simulating.

Following parameters (arguments) are used in these function,

| Parameters | Descriptions                                                        |
|------------|---------------------------------------------------------------------|
| `n`        | Number of training samples                                          |
| `p`        | Number of predictor variables                                       |
| `q`        | Number of relevant predictors                                       |
| `relpos`   | Position of relevant components                                     |
| `R2`       | Coefficient of determinations                                       |
| `rho`      | Correlation between two responses (only applicable on `simrel2`)    |
| `gamma`    | Decaying factor of eigenvalues of predictor matrix                  |
| `m`        | Number of required response vector (only applicable for `simrel_m`) |

## Installation
Install the package from GitHub,


```r
# install.pacakges("devtools")
devtools::install_github("therimalaya/simulatr")
```

## Examples
I will go through some problem and show how `simulatr` solves those tasks,

1. Simulate a univariate linear model data with 100 training samples and 500 test samples having 10 predictors ($\bf{X}$) where only 8 of them are relevant for the variation in the response vector. The population model should explain 80% of the variation present in the response. In addition, only 1st and 3rd principal components of $\bf{X}$ should be relevant for $y$ and the eigenvalues of $\bf{X}$ decreases exponentially by a factor of 0.7.


```r
library(simulatr)
sim_obj <- 
  simulatr(
    n      = 100,         # 100 training samples
    p      = 10,          # 10 predictor variables
    q      = 8,           # only 8 of them are relevant
    R2     = 0.8,         # 80% of variation is explained by the model
    relpos = c(1, 3),     # First and third principal components are relevant
    gamma  = 0.7,         # decay factor of eigenvalue of X is 7
    ntest  = 500,         # 500 Test observations
    type   = "univariate" # Univariate linear model data simulation
  )
```

Here `sim_obj` is a object with class `simrel` and constitue of a list of simulated linear model data along with other relevant properties. Lets use `plot.simulatr` function to overview the situation,


```r
plot_simulatr(sim_obj, ask = FALSE)
```

