#' @title Prediction method for xgb.Booster.surv model
#'
#' @description \code{predict.xgb.Booster.surv} is a method for xgb.Booster.surv
#' objects that enables preidcting either risk (implemented also in the xgboost package)
#' or the full survival curve.
#' @param object an xgb.Booster.surv object obtained by \code{xgb.train.surv}
#' @param newdata a data.frame/matrix to make predictions for
#' @param type either "risk" or "age"
#' @param times times at which to estimate the survival curve at. Default is original dataset unique death times.
#' @return for \code{type = "risk"} a vector of risk scores, for \code{type = "age"}.
#' @example inst/train_predict_xgb.Booster.surv.R
#' @seealso \code{\link{xgb.train.surv}}
#' @export

predict.xgb.Booster.surv <- function(object, newdata, type = "risk") {
  class(object) <- "xgb.Booster"
  risk <- xgboost:::predict.xgb.Booster(object, newdata)
  if (type == "risk") {
    return(risk)
  } else if (type == "age") {
    a <- object$a
    b <- object$b
    pred <- exp(a + b * log(risk))
    return(pred)
  } else {
    stop('type must be one of "risk", "age"')
  }
}
