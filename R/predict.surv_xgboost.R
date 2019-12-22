#' @title Prediction method for surv_xgboost model
#'
#' @description \code{predict.surv_xgboost} is a method for surv_xgboost
#' objects that enables preidcting either risk (implemented also in the xgboost package)
#' or the full survival curve.
#' @param object a surv_xgboost object obtained by \code{xgb.train.surv}
#' @param newdata A data.frame/matrix to make predictions for
#' @param type Either "risk" or "surv"
#' @param times Times at which to estimate the survival curve at. Default is original dataset unique death times.
#' @return For \code{type = "risk"} a vector of risk scores, for \code{type = "surv"} a matrix with
#' columns corresponding to times and rows corresponding to input newdata rows.
#' @example inst/train_predict_surv_xgboost.R
#' @seealso \code{\link{xgb.train.surv}}
#' @export

predict.surv_xgboost <- function(object, newdata, type = "risk", times = NULL) {
  if (class(newdata) == "data.frame") {
    if (any(sapply(newdata, function(newdata) any(is.na(newdata))))) stop("newdata must not contain any missing values")
    newdata <- model.matrix(as.formula(paste0("~ - 1 + ", paste0(object$feature_names, collapse = " + "))), data = newdata)
  } else if (class(newdata) == "matrix") {
    if (any(apply(newdata, 2, function(x) any(is.na(x))))) stop("newdata must not contain any missing values")
  } else {
    stop("newdata must be either a matrix or a data.frame object")
  }
  class(object) <- "xgb.Booster"
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
