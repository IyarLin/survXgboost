#' @title Train a survival xgboost including baseline hazard
#'
#' @description \code{xgb.train.surv} is a thin wrapper around \code{xgboost::xgb.train} that
#' produces a \code{surv_xgboost} object. This object has a predict method that enables full
#' survival curve prediction in addition to the usual relative risk score predictions.
#' @param x trainig dataset. either a data.frame or a matrix
#' @param time_var name of variable denoting time to event
#' @param status_var name of variable denoting event (1 - event/dead, 0 - right censored/alive)
#' @param val_frac fraction of training data set aside for validation in case early stopping rounds are used.
#' If not null (numeric between 0 and 1) then \code{early_stopping_rounds} should also be set
#' @param param_list parameter list passed to xgboost. If null (default) then objective is set
#' to \code{survival:cox} and eval_metric is set to \code{cox-nloglik}
#' @param nrounds same as in \code{xgb.train}
#' @param verbose same as in \code{xgb.train}
#' @param print_every_n same as in \code{xgb.train}
#' @param early_stopping_rounds same as in \code{xgb.train}. Should be set in case val_frac is set
#' @param save_period same as in \code{xgb.train}
#' @param save_name same as in \code{xgb.train}, though default is different
#' @param ... more arguments to be passed to the underlying xgboost function call
#' @return an object of class \code{surv_xgboost}
#' @example inst/train_predict_surv_xgboost.R
#' @seealso \code{\link{predict.surv_xgboost}}
#' @details The xgboost package supports the cox proportional hazards model but the predict method
#' returns only the risk score (which is equivalent to \eqn{exp(X\beta)} or \code{type = "risk"} in \code{survival::coxph}).
#' This function returns a \code{surv_xgboost} object which enables prediction of both the risk score as well
#' the entire survival curve. Baseline hazard rate is obtained using the \code{survival::basehaz} function
#' which is then scaled to fit the original baseline hazard computed (but not returned) by the \code{xgboost::xgb.train} function
#' @importFrom prodlim Hist
#' @export

xgb.train.surv <- function(x, time_var, status_var, val_frac = NULL, param_list = NULL, nrounds, verbose = 1,
                           print_every_n = 1L, early_stopping_rounds = NULL, save_period = NULL,
                           save_name = "surv_xgboost.model", ...) {
  if (class(x) == "data.frame") {
    if (any(sapply(x, function(x) any(is.na(x))))) stop("x must not contain any missing values")
    x_mat <- model.matrix(as.formula(paste0("~ . - 1 - ", time_var, " - ", status_var)), data = x)
    label <- ifelse(x[[status_var]] == 1, x[[time_var]], -x[[time_var]])
  } else if (class(x) == "matrix") {
    if (any(apply(x, 2, function(x) any(is.na(x))))) stop("x must not contain any missing values")
    x_mat <- x[, !colnames(x) %in% c(time_var, status_var)]
    label <- ifelse(x[, colnames(x) == status_var] == 1, x[, colnames(x) == time_var], -x[, colnames(x) == time_var])
  } else {
    stop("x must be either a matrix or a data.frame object")
  }

  if (!is.null(param_list)) {
    if (param_list$objective != "survival:cox") stop("param_list objective must be set to survival:cox")
    if (param_list$eval_metric != "cox-nloglik") stop("param_list eval_metric must be set to cox-nloglik")
  } else {
    param_list <- list(
      objective = "survival:cox",
      eval_metric = "cox-nloglik"
    )
  }

  # train xgboost model
  x_mat_DMatrix <- xgboost:::xgb.DMatrix(x_mat, label = label)
  if (is.null(val_frac)) {
    xgboost_model <- xgboost:::xgb.train(
      params = param_list, data = x_mat_DMatrix, nrounds = nrounds, verbose = verbose,
      print_every_n = print_every_n, early_stopping_rounds = early_stopping_rounds,
      save_period = save_period, save_name = save_name, ...
    )
  } else {
    if (val_frac < 0 | val_frac > 1) stop("val_frac must be in (0,1)")
    if (is.null(early_stopping_rounds)) stop("If val_frac is not null then early_stopping_rounds should also be set")
    val_idx <- sample.int(nrow(x_mat), size = nrow(x_mat) * val_frac, replace = F)
    x_mat_subset_DMatrix <- xgboost:::xgb.DMatrix(x_mat[-val_idx, ], label = label[-val_idx])
    x_mat_val_DMatrix <- xgboost:::xgb.DMatrix(x_mat[val_idx, ], label = label[val_idx])
    xgboost_model <- xgboost:::xgb.train(
      params = param_list, data = x_mat_subset_DMatrix, nrounds = nrounds, verbose = verbose,
      print_every_n = print_every_n, early_stopping_rounds = early_stopping_rounds,
      save_period = save_period, save_name = save_name,
      watchlist = list(val1 = x_mat_subset_DMatrix, val2 = x_mat_val_DMatrix), ...
    )
  }

  # generate baseline hazard
  if (is.matrix(x)) x <- as.data.frame(x)

  cox_model <- survival:::coxph(formula = as.formula(paste0("Surv(", time_var, ", ", status_var, ") ~ .")), data = x)
  baseline_hazard <- survival:::basehaz(cox_model)

  HR <- xgboost:::predict.xgb.Booster(object = xgboost_model, newdata = x_mat_DMatrix)
  baseline_pred <- function(const) {
    risk <- HR * const
    surv <- exp(risk %*% -matrix(c(0, baseline_hazard[, 1]), nrow = 1))
    Models <- list(
      "xgboost" = surv
    )

    PredError <- pec:::pec(
      object = Models,
      formula = Surv(time, status) ~ 1,
      data = x,
      cens.model = "marginal",
      splitMethod = "none",
      times = baseline_hazard[, 2],
      exact = F,
      verbose = F,
      reference = F
    )

    return(pec:::crps(PredError))
  }

  optimal_const <- optim(par = 1, fn = baseline_pred, method = "Brent", lower = 0, upper = 10)
  baseline_hazard[, 1] <- baseline_hazard[, 1] * optimal_const$par
  xgboost_model$baseline_hazard <- baseline_hazard
  class(xgboost_model) <- "surv_xgboost"
  return(xgboost_model)
}
