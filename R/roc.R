
roc <- function (x, resp = NULL, ...) {
  
  if(is.null(resp))
    roc(predictor = predict(x, type="prob")[, 2], response = Response(x), plot=FALSE, ...)
  else
    roc(predictor=x, response = resp, plot=FALSE, ...)
  
}


confint.ROC <- function(object, parm, level = 0.95, ...) {
  pROC::ci.coords(roc=object, conf.level = level, ...)
}



