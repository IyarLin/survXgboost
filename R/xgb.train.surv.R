#' @title Train a survival xgboost including baseline hazard
#'
#' @description \code{xgb.train.surv} is a thin wrapper around \code{xgboost::xgb.train} that
#' produces a \code{xgb.Booster.surv} object. This object has a predict method that enables full
#' survival curve prediction in addition to the usual relative risk score predictions.
#' @param params same as in xgb.train. If provided then objective must be set to "survival:cox"
#' and eval_metric must be set to "cox-nloglik"
#' @param data must be a covariates **matrix**
#' @param label survival label. These are survival times with negative magnitude for censored
#' cases (so a case where someone survived 10 days and hasn't died yet would be coded as -10)
#' @param weight (optional) weight vector for training samples
#' @param nrounds same as in xgb.train
#' @param watchlist same as in xgb.train. This can be tricky, see example
#' @param verbose same as in xgb.train
#' @param print_every_n same as in xgb.train
#' @param early_stopping_rounds same as in xgb.train
#' @param save_period same as in xgb.train
#' @param save_name same as in xgb.train, defaults to
#' @param xgb_model same as in xgb.train
#' @param callbacks same as in xgb.train
#' @param ... additional arguments passed to xgb.train
#' @return an object of class \code{xgb.Booster.surv}
#' @example inst/train_predict_xgb.Booster.surv.R
#' @seealso \code{\link{predict.xgb.Booster.surv}}
#' @details The xgboost package supports the cox proportional hazards model but the predict method
#' returns only the risk score (which is equivalent to \eqn{exp(X\beta)} or \code{type = "risk"} in \code{survival::coxph}).
#' This function returns a \code{xgb.Booster.surv} object which enables prediction of both the risk score as well
#' the entire survival curve. Baseline hazard rate is obtained using the \code{survival::basehaz} function
#' which is then scaled to fit the original baseline hazard computed (but not returned) by the \code{xgboost::xgb.train} function
#' @importFrom prodlim Hist
#' @export

xgb.train.surv <- function(params = list(), data, label, weight = NULL, nrounds,
                           watchlist = list(), verbose = 1, print_every_n = 1L,
                           early_stopping_rounds = NULL, save_period = NULL,
                           save_name = "xgboost_surv.model", xgb_model = NULL, callbacks = list(), ...) {
  if (length(params) > 0) {
    if (params$objective != "survival:cox") stop("params objective must be set to survival:cox")
    if (params$eval_metric != "cox-nloglik") stop("params eval_metric must be set to cox-nloglik")
  } else {
    params <- list(
      objective = "survival:cox",
      eval_metric = "cox-nloglik"
    )
  }

  if(is.null(weight)) weight <- rep(1, nrow(data))

  data_DMatrix <- xgboost:::xgb.DMatrix(data = data, label = label, weight = weight)

  xgboost_model <- xgboost:::xgb.train(
    params = params, data = data_DMatrix, nrounds = nrounds, watchlist = watchlist, verbose = verbose,
    print_every_n = print_every_n, early_stopping_rounds = early_stopping_rounds, save_period = save_period,
    save_name = "surv_xgboost.model", xgb_model = xgb_model, callbacks = callbacks, ...
  )


  # generate baseline hazard
  data_data.frame <- data.frame(data, time = abs(label), status = ifelse(sign(label) == 1, 1, 0))

  cox_model <- survival:::coxph(formula = Surv(time, status) ~ ., data = data_data.frame)
  baseline_hazard <- survival:::basehaz(cox_model)

  if (baseline_hazard[1, 2] != 0) {
    baseline_hazard <- rbind(c(0, 0), baseline_hazard) # pec always requests time = 0 survival as well
  }

  HR <- xgboost:::predict.xgb.Booster(object = xgboost_model, newdata = data_DMatrix)
  baseline_pred <- function(const) {
    risk <- HR * const
    surv <- exp(risk %*% -matrix(baseline_hazard[, 1], nrow = 1))

    Models <- list(
      "xgboost" = surv
    )

    PredError <- pec:::pec(
      object = Models,
      formula = Surv(time, status) ~ 1,
      data = data_data.frame,
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
  class(xgboost_model) <- "xgb.Booster.surv"
  return(xgboost_model)
}
