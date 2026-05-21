

bestCut <- function(x, method=c("youden", "closest.topleft")){
  pROC::coords(roc=x, x="best", best.method=method,
               transpose=TRUE)
}

