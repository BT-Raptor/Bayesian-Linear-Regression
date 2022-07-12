Bayesian Linear Regression
================
BT_Raptor
21/04/2022

``` r
require(data.table)
```

    ## Loading required package: data.table

``` r
require(stats)
require(factoextra)
```

    ## Loading required package: factoextra

    ## Warning: package 'factoextra' was built under R version 4.1.3

    ## Loading required package: ggplot2

    ## Warning: package 'ggplot2' was built under R version 4.1.3

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
setwd("C:/Users/ricke/Desktop/Raptor's Data Science/")
options(scipen=999)
```

Data Acquisition

``` r
all_data=fread("opp.csv")
all_data=na.omit(all_data)
head(all_data)
```

    ##    case site Pop sex age hdlngth skullw totlngth taill footlgth earconch  eye
    ## 1:    1    1 Vic   m   8    94.1   60.4     89.0  36.0     74.5     54.5 15.2
    ## 2:    2    1 Vic   f   6    92.5   57.6     91.5  36.5     72.5     51.2 16.0
    ## 3:    3    1 Vic   f   6    94.0   60.0     95.5  39.0     75.4     51.9 15.5
    ## 4:    4    1 Vic   f   6    93.2   57.1     92.0  38.0     76.1     52.2 15.2
    ## 5:    5    1 Vic   f   2    91.5   56.3     85.5  36.0     71.0     53.2 15.1
    ## 6:    6    1 Vic   f   1    93.1   54.8     90.5  35.5     73.2     53.6 14.2
    ##    chest belly
    ## 1:  28.0    36
    ## 2:  28.5    33
    ## 3:  30.0    34
    ## 4:  28.0    34
    ## 5:  28.5    33
    ## 6:  30.0    32

Creating a new dataframe, where we do the column tweaking

``` r
all_data=all_data[,`:=`(case=NULL,site=NULL,Pop=NULL,sex=NULL)]
head(all_data)
```

    ##    age hdlngth skullw totlngth taill footlgth earconch  eye chest belly
    ## 1:   8    94.1   60.4     89.0  36.0     74.5     54.5 15.2  28.0    36
    ## 2:   6    92.5   57.6     91.5  36.5     72.5     51.2 16.0  28.5    33
    ## 3:   6    94.0   60.0     95.5  39.0     75.4     51.9 15.5  30.0    34
    ## 4:   6    93.2   57.1     92.0  38.0     76.1     52.2 15.2  28.0    34
    ## 5:   2    91.5   56.3     85.5  36.0     71.0     53.2 15.1  28.5    33
    ## 6:   1    93.1   54.8     90.5  35.5     73.2     53.6 14.2  30.0    32

Training & Test Split

``` r
train_data=all_data[seq(1,52),]
test_data=all_data[seq(53,104),]
test_actuals=test_data[,c('age')]
test_data[,age:=NULL]
```

Train the Model (Normal Distribution)

``` r
require(rstanarm)
```

    ## Loading required package: rstanarm

    ## Warning: package 'rstanarm' was built under R version 4.1.3

    ## Loading required package: Rcpp

    ## This is rstanarm version 2.21.3

    ## - See https://mc-stan.org/rstanarm/articles/priors for changes to default priors!

    ## - Default priors may change, so it's safest to specify priors, even if equivalent to the defaults.

    ## - For execution on a local, multicore CPU with excess RAM we recommend calling

    ##   options(mc.cores = parallel::detectCores())

``` r
model=stan_glm(age~.,data=train_data,family=poisson())
```

    ## 
    ## SAMPLING FOR MODEL 'count' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 0.222 seconds (Warm-up)
    ## Chain 1:                0.228 seconds (Sampling)
    ## Chain 1:                0.45 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL 'count' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 0.245 seconds (Warm-up)
    ## Chain 2:                0.259 seconds (Sampling)
    ## Chain 2:                0.504 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL 'count' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 0.232 seconds (Warm-up)
    ## Chain 3:                0.218 seconds (Sampling)
    ## Chain 3:                0.45 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL 'count' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 0.227 seconds (Warm-up)
    ## Chain 4:                0.227 seconds (Sampling)
    ## Chain 4:                0.454 seconds (Total)
    ## Chain 4:

``` r
summary(model)
```

    ## 
    ## Model Info:
    ##  function:     stan_glm
    ##  family:       poisson [log]
    ##  formula:      age ~ .
    ##  algorithm:    sampling
    ##  sample:       4000 (posterior sample size)
    ##  priors:       see help('prior_summary')
    ##  observations: 52
    ##  predictors:   10
    ## 
    ## Estimates:
    ##               mean   sd   10%   50%   90%
    ## (Intercept) -2.7    3.4 -7.0  -2.6   1.6 
    ## hdlngth     -0.1    0.1 -0.1  -0.1   0.0 
    ## skullw       0.0    0.0  0.0   0.0   0.1 
    ## totlngth     0.0    0.0  0.0   0.0   0.1 
    ## taill        0.0    0.1 -0.1   0.0   0.1 
    ## footlgth     0.0    0.0  0.0   0.0   0.1 
    ## earconch     0.0    0.0  0.0   0.0   0.1 
    ## eye          0.3    0.1  0.1   0.3   0.4 
    ## chest        0.0    0.1  0.0   0.0   0.1 
    ## belly        0.0    0.0  0.0   0.0   0.1 
    ## 
    ## Fit Diagnostics:
    ##            mean   sd   10%   50%   90%
    ## mean_PPD 4.1    0.4  3.6   4.1   4.6  
    ## 
    ## The mean_ppd is the sample average posterior predictive distribution of the outcome variable (for details see help('summary.stanreg')).
    ## 
    ## MCMC diagnostics
    ##               mcse Rhat n_eff
    ## (Intercept)   0.1  1.0  3563 
    ## hdlngth       0.0  1.0  2156 
    ## skullw        0.0  1.0  3536 
    ## totlngth      0.0  1.0  2373 
    ## taill         0.0  1.0  2063 
    ## footlgth      0.0  1.0  2326 
    ## earconch      0.0  1.0  2315 
    ## eye           0.0  1.0  2684 
    ## chest         0.0  1.0  3982 
    ## belly         0.0  1.0  4029 
    ## mean_PPD      0.0  1.0  4764 
    ## log-posterior 0.1  1.0  1882 
    ## 
    ## For each parameter, mcse is Monte Carlo standard error, n_eff is a crude measure of effective sample size, and Rhat is the potential scale reduction factor on split chains (at convergence Rhat=1).

Test the model

``` r
preds=round(predict(model,test_data))
test_actuals$preds=preds
head(test_actuals)
```

    ##    age preds
    ## 1:   3     2
    ## 2:   4     2
    ## 3:   3     2
    ## 4:   2     2
    ## 5:   2     1
    ## 6:   7     1
