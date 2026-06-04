

.onLoad <- function(libname, pkgname) {
  
  # # presetting DescTools options not already defined by the user
  # op <- options()
  # pkg.op <- list(
  #   
  # )
  # 
  # toset <- !(names(pkg.op) %in% names(op))
  # if (any(toset)) options(pkg.op[toset])
  
}




# if starting environment is somwhere needed
# .DescToolsEnv <- new.env(parent = emptyenv())

#' @useDynLib alloy, .registration = TRUE
#' 
#' @importFrom stats na.omit AIC BIC as.formula coef confint confint.default cov2cor deviance fitted formula glm logLik model.frame model.matrix model.response nobs pchisq pf pnorm predict terms vcov weights lm
#'             
#' @importFrom graphics hist 
#' @importFrom DescToolsX brierScore cStat
#' @importFrom aurora fm style strAlign
#' @importFrom bedrock setNamesX unwhich isDichotomous
#' 
#' @importFrom nnet multinom
#' @importFrom AER tobit
#'              
NULL
