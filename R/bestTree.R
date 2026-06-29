
#' Best tree size using the 1-SE rule
#'
#' Selects the most parsimonious tree whose cross-validated error is within
#' one standard error of the minimum, following Breiman et al. (1984).
#'
#' @param x A fitted \code{rpart} object.
#'
#' @return A list with components:
#'   \describe{
#'     \item{\code{leaves}}{Number of terminal nodes in the best tree.}
#'     \item{\code{cp}}{Complexity parameter to pass to \code{prune()}.}
#'   }
#'
#' @examples
#' r <- rpart::rpart(Species ~ ., data = iris)
#' bt <- bestTree(r)
#' r.pruned <- rpart::prune(r, cp = bt$cp)
#'
#' @references
#' Breiman, L., Friedman, J., Olshen, R., & Stone, C. (1984).
#' \emph{Classification and Regression Trees}. Wadsworth.
#'


#' @family tree  
#' @concept classification  
#' @concept modelling
#'
#'
#' @export
bestTree <- function(x) {
  
  if (!inherits(x, "rpart"))
    stop("'x' must be an rpart object")
  
  ct <- x$cptable
  # 1-SE rule: smallest tree with xerror <= min(xerror) + xstd at that minimum
  i  <- ct[, "xerror"] <= min(ct[, "xerror"]) + ct[which.min(ct[, "xerror"]), "xstd"]
  
  list(
    leaves = 1L + as.integer(ct[, "nsplit"][i][1L]),
    cp     = as.numeric(ct[, "CP"][i][1L]) + 1e-6  # small offset for rounding safety
  )
}
