
#' Complexity parameter table for an rpart tree
#'
#' A lightweight wrapper around \code{\link[rpart]{printcp}} and
#' \code{\link[rpart]{plotcp}} that stores the CP table and the minimum
#' cross-validated error CP as a structured object.
#'
#' @param x A fitted \code{rpart} object.
#' @param ... Currently unused.
#'
#' @return An object of class \code{"CP"} with components \code{cp}
#'   (the full CP table), \code{mincp} (CP at minimum xerror), and
#'   \code{x} (the original rpart object).
#'
#' @examples
#' r <- rpart::rpart(Species ~ ., data = iris)
#' cp <- cParam(r)
#' cp
#' plot(cp)
#'
#' @seealso \code{\link{bestTree}}, \code{\link[rpart]{printcp}},
#'   \code{\link[rpart]{plotcp}}
#' @export
cParam <- function(x, ...) {
  
  if (!inherits(x, "rpart"))
    stop("'x' must be an rpart object")
  
  structure(
    list(
      cp    = x$cptable,
      mincp = x$cptable[which.min(x$cptable[, "xerror"]), "CP"],
      x     = x
    ),
    class = "CP"
  )
}


#' @export
print.CP <- function(x, digits = getOption("digits") - 2L, ...) {
  rpart::printcp(x$x, digits = digits, ...)
  cat("\n")
  invisible(x)
}


#' @export
plot.CP <- function(x, minline = TRUE, lty = 3L, col = 1L,
                    upper = c("size", "splits", "none"), ...) {
  rpart::plotcp(x$x, minline = minline, lty = lty, col = col,
                upper = match.arg(upper), ...)
  invisible(x)
}

