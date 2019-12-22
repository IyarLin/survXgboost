library(survival)
data("lung")
library(survXgboost)
x <- lung[complete.cases(lung), ] # doesn't handle missing values at the moment
x$status <- x$status - 1 # format status variable correctly such that 1 is event/death and 0 is censored/alive

# train surv_xgboost
surv_xgboost_model <- xgb.train.surv(
  x = x, time_var = "time", status_var = "status",
  val_frac = 0.1,
  param_list = list(
    objective = "survival:cox",
    eval_metric = "cox-nloglik",
    eta = 0.05 # larger eta leads to algorithm not converging, resulting in NaN predictions
  ),
  nrounds = 1000,
  early_stopping_rounds = 10
)

# predict survival curves
times <- seq(10, 1000, 50)
survival_curves <- predict(object = surv_xgboost_model, newdata = x, type = "surv", times = times)
matplot(times, t(survival_curves[1:5, ]), type = "l")

# predict risk score
risk_scores <- predict(object = surv_xgboost_model, newdata = x, type = "risk")
hist(risk_scores)
