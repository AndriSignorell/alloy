

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


#' @useDynLib alloy, .registration = TRUE
#' 
#' @importFrom stats na.omit AIC BIC as.formula coef confint confint.default cov2cor deviance fitted formula glm logLik model.frame model.matrix model.response nobs pchisq pf pnorm predict terms vcov weights lm model.matrix model.weights terms drop1 update na.pass qnorm contr.poly anova drop.scope pt update.formula family delete.response model.offset df.residual qt
#' @importFrom utils getFromNamespace
#'             
#' @importFrom graphics hist par
#' @importFrom DescToolsX brierScore cStat ordAssocs conf
#' @importFrom pharos fm style strAlign plotDot addOpacity boxedText fade pal getTheme .useTheme .resolveTitle .withGraphicsState .applyParFromDots .marTop .drawGrid band .drawBox plotQQ plotBubble abcCoords plotFacet
#' @importFrom bedrock setNamesX unwhich isDichotomous appendX nDec checkConfLevel callIf
#' 
#' @importFrom MASS polr glm.nb lda qda
#' @importFrom survival Surv coxph survreg
#' @importFrom nnet multinom nnet
#' 
NULL
