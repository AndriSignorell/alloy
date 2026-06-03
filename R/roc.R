
#' @export
roc <- function (x, resp = NULL, ...) {
  
  if(is.null(resp))
    roc(predictor = predict(x, type="prob")[, 2], 
        response = response(x), plot=FALSE, ...)
  else
    roc(predictor=x, response = resp, plot=FALSE, ...)
  
}

#' @export
confint.ROC <- function(object, parm, level = 0.95, ...) {
  pROC::ci.coords(roc=object, conf.level = level, ...)
}



