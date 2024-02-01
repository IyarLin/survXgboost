survXgboost package
================
Iyar Lin
01 February, 2024

The xgboost package survival model returns predictions on the hazard
ratio scale (i.e., as \$HR = exp(marginal_prediction) in the
proportional hazard function
![h(t) = h0(t) \* HR](https://latex.codecogs.com/png.latex?h%28t%29%20%3D%20h0%28t%29%20%2A%20HR "h(t) = h0(t) * HR").
This quantity is equivalent to the type = “risk” in coxph. This package
provides a thin wrapper that enables using the xgboost package to
perform full survival curve estimation.

See also
[discussion](https://datascience.stackexchange.com/questions/65266/how-do-i-predict-survival-curves-using-xgboost)
in stackoverflow overflow

Below is a short usage demo.

First, prepare the data:

``` r
library(survXgboost)
library(survival)
library(xgboost)
data("lung")
lung <- lung[complete.cases(lung), ] # doesn't handle missing values at the moment
lung$status <- lung$status - 1 # format status variable correctly such that 1 is event/death and 0 is censored/alive
label <- ifelse(lung$status == 1, lung$time, -lung$time)

val_ind <- sample.int(nrow(lung), 0.1 * nrow(lung))
x_train <- as.matrix(lung[-val_ind, !names(lung) %in% c("time", "status")])
x_label <- label[-val_ind]
x_val <- xgb.DMatrix(as.matrix(lung[val_ind, !names(lung) %in% c("time", "status")]),
                     label = label[val_ind])
```

Below we train an xgboost survival model using the function from the
survXgboost package rather than the xgboost package:

``` r
# train surv_xgboost
surv_xgboost_model <- xgb.train.surv(
  params = list(
    objective = "survival:cox",
    eval_metric = "cox-nloglik",
    eta = 0.05 # larger eta leads to algorithm not converging, resulting in NaN predictions
  ), data = x_train, label = x_label,
  watchlist = list(val2 = x_val),
  nrounds = 1000, early_stopping_rounds = 30
)
```

    ## [1]  val2-cox-nloglik:1.986334 
    ## Will train until val2_cox_nloglik hasn't improved in 30 rounds.
    ## 
    ## [2]  val2-cox-nloglik:1.988136 
    ## [3]  val2-cox-nloglik:1.999317 
    ## [4]  val2-cox-nloglik:2.000369 
    ## [5]  val2-cox-nloglik:2.011067 
    ## [6]  val2-cox-nloglik:1.999203 
    ## [7]  val2-cox-nloglik:2.000823 
    ## [8]  val2-cox-nloglik:2.014079 
    ## [9]  val2-cox-nloglik:2.015103 
    ## [10] val2-cox-nloglik:2.016976 
    ## [11] val2-cox-nloglik:2.020993 
    ## [12] val2-cox-nloglik:2.024742 
    ## [13] val2-cox-nloglik:2.027878 
    ## [14] val2-cox-nloglik:2.034186 
    ## [15] val2-cox-nloglik:2.042607 
    ## [16] val2-cox-nloglik:2.046746 
    ## [17] val2-cox-nloglik:2.052691 
    ## [18] val2-cox-nloglik:2.063710 
    ## [19] val2-cox-nloglik:2.074147 
    ## [20] val2-cox-nloglik:2.072881 
    ## [21] val2-cox-nloglik:2.084827 
    ## [22] val2-cox-nloglik:2.089368 
    ## [23] val2-cox-nloglik:2.104786 
    ## [24] val2-cox-nloglik:2.113672 
    ## [25] val2-cox-nloglik:2.122862 
    ## [26] val2-cox-nloglik:2.129615 
    ## [27] val2-cox-nloglik:2.127782 
    ## [28] val2-cox-nloglik:2.132626 
    ## [29] val2-cox-nloglik:2.135991 
    ## [30] val2-cox-nloglik:2.140200 
    ## [31] val2-cox-nloglik:2.144522 
    ## Stopping. Best iteration:
    ## [1]  val2-cox-nloglik:1.986334

Next we can predict full survival curves:

``` r
# predict survival curves
times <- seq(10, 1000, 50)
survival_curves <- predict(object = surv_xgboost_model, newdata = x_train, type = "surv", times = times)
matplot(times, t(survival_curves[1:5, ]), type = "l")
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

We can also predict the risk scores like in the original xgboost
package:

``` r
# predict risk score
risk_scores <- predict(object = surv_xgboost_model, newdata = x_train, type = "risk")
hist(risk_scores)
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
