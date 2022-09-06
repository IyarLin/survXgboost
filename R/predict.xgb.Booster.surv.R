#' @title Prediction method for xgb.Booster.surv model
#'
#' @description \code{predict.xgb.Booster.surv} is a method for xgb.Booster.surv
#' objects that enables preidcting either risk (implemented also in the xgboost package)
#' or the full survival curve.
#' @param object an xgb.Booster.surv object obtained by \code{xgb.train.surv}
#' @param newdata a data.frame/matrix to make predictions for
#' @param type either "risk" or "surv"
#' @param times times at which to estimate the survival curve at. Default is original dataset unique death times.
#' @return for \code{type = "risk"} a vector of risk scores, for \code{type = "surv"} a matrix with
#' columns corresponding to times and rows corresponding to input newdata rows.
#' @example inst/train_predict_xgb.Booster.surv.R
#' @seealso \code{\link{xgb.train.surv}}
#' @export

predict.xgb.Booster.surv <- function(object, newdata, type = "risk", times = NULL) {
  risk <- xgboost:::predict.xgb.Booster(object, newdata)
  if (type == "risk") {
    return(risk)
  } else if (type == "surv") {
    if (!is.null(times)) {
      if (max(times) > max(object$baseline_hazard[, 2])) {
        object$baseline_hazard <- rbind(object$baseline_hazard, c(max(object$baseline_hazard[, 1]), max(times)))
      }
    } else {
      times <- object$baseline_hazard[, 2]
    }
    surv <- exp(risk %*% -matrix(object$baseline_hazard[, 1], nrow = 1))
    surv <- surv[, findInterval(times, object$baseline_hazard[, 2])]
    colnames(surv) <- times
    return(surv)
  } else {
    stop('type must be one of "risk", "surv"')
  }
}
