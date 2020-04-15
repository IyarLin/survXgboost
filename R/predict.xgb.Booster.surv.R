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

predict.xgb.Booster.surv <- function(object, newdata, type = "risk", times = NULL, conditional_after = NULL) {
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
    
    if (!is.null(conditional_after)) {
      baseline_hazard <- object$baseline_hazard

      interpolate_at_times <- function(baseline_hazard, times) {
        haz_max <- max(baseline_hazard$hazard)
        haz_min <- min(baseline_hazard$hazard)
        interp <- function(time) {
          return(approx(
            x = baseline_hazard$time, y = baseline_hazard$hazard, xout = time, yleft = haz_min, yright = haz_max
            )$y
          )
        }
        return(lapply(times, interp))
      }
      
      n <- nrow(newdata)
      
      #times_to_evaluate_at <- t(matrix(rep(times, n), ncol = n)) + conditional_after
      surv <- list()
      for (i in 1:n) {
        times_to_evaluate_at <- times + conditional_after[i]
        c_0 <- interpolate_at_times(baseline_hazard, times_to_evaluate_at)
        c_0_conditional_after <- unlist(c_0[1], use.names=FALSE)
        c_0 <- pmax(0, unlist(c_0, use.names=FALSE) - c_0_conditional_after)
        surv <- append(surv, exp(risk[i] * -1.0 * c_0))
      }
      surv <- matrix(surv, nrow = n, byrow = TRUE)
    } else {
      surv <- exp(risk %*% -matrix(object$baseline_hazard[, 1], nrow = 1))
    }
    
    #surv <- exp(risk %*% -matrix(object$baseline_hazard[, 1], nrow = 1))
    surv <- surv[, findInterval(times, object$baseline_hazard[, 2])]
    colnames(surv) <- times
    return(surv)
  } else {
    stop('type must be one of "risk", "surv"')
  }
}
