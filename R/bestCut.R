
#' Best cut-point of an ROC curve
#'
#' A thin convenience wrapper around \code{\link[pROC]{coords}} that returns
#' the optimal cut-point of an ROC curve together with the corresponding
#' sensitivity and specificity.
#'
#' @param x An object of class \code{"roc"} as returned by
#'   \code{\link[pROC]{roc}}.
#' @param method Character string specifying the optimality criterion.
#'   \code{"youden"} (default) maximises the Youden index
#'   \eqn{J = \text{sensitivity} + \text{specificity} - 1};
#'   \code{"closest.topleft"} minimises the Euclidean distance to the
#'   top-left corner \eqn{(0, 1)} of the ROC space.
#'
#' @return A named numeric vector with elements \code{threshold},
#'   \code{specificity}, and \code{sensitivity} at the optimal cut-point.
#'   When ties exist, \code{\link[pROC]{coords}} may return multiple columns;
#'   the result is then a matrix (one column per tied optimum).
#'
#' @examples
#' library(pROC)
#' data(aSAH)
#' r <- roc(aSAH$outcome, aSAH$s100b)
#'
#' bestCut(r)
#' bestCut(r, method = "closest.topleft")
#'
#' @seealso \code{\link[pROC]{coords}}, \code{\link[pROC]{roc}}


#' @export
bestCut <- function(x, method = c("youden", "closest.topleft")) {
  method <- match.arg(method)
  pROC::coords(roc = x, x = "best", best.method = method,
               transpose = TRUE)
}