# survXgboost package

The xgboost package survival model returns predictions on the hazard ratio scale (i.e., as HR = exp(marginal_prediction) in the proportional hazard function h(t) = h0(t) * HR. This quantity is equivalent to the type = "risk" in coxph. This package provides a thin wrapper that enables using the xgboost package to perform full survival curve estimation. 