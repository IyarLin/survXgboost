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
#' the entire survival curve. Baseline hazard rate is obtained using the \code{survival::survfit} function with stype = 2 to obtain the Breslow estimator
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

  times_total <- label
  events <- times_total > 0
  times_total <- abs(times_total)
  times_events <- times_total[events]
  times_events_unique <- sort(unique(times_events))
  predictions <- predict(xgboost_model, data_DMatrix, outputmargin = TRUE)
  haz <- numeric(length(times_events_unique))
  for(i in seq_along(times_events_unique)){
    t <- times_events_unique[i]
    haz[i] = sum(times_events == t) / sum(exp(predictions[times_total >= t]))
  }
  cumhaz <- cumsum(haz)
  if(!0 %in% times_events_unique){
    cumhaz <- c(0, cumhaz)
    times_events_unique <- c(0, times_events_unique)
  }
  xgboost_model$baseline_hazard <- data.frame(hazard = c(0, cumhaz), time = c(0, times_events_unique))

  # Alternatetively, use riskRegression to create a baseline hazard
  # riskRegression can use Efron's correction for tied times

  # predictions <- predict(xgboost_model, data_DMatrix, outputmargin = TRUE)
  # sort_order <- order(abs(label))
  # predictions <- predictions[sort_order]
  # label <- label[sort_order]
  # event_times <- label[label > 0]
  # basehaz <- riskRegression::baseHaz_cpp(rep(0, length(label)), abs(label), ifelse(label > 0, 1L, 0L), exp(predictions), seq_along(label), unique(event_times), max(event_times), length(label), 1, 1, TRUE)
  # out$basehaz <- data.frame(hazard = basehaz$cumhazard, time = basehaz$times)

  class(xgboost_model) <- c("xgb.Booster.surv", "xgb.Booster")
  return(xgboost_model)
}
