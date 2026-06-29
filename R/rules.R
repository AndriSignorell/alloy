
#' Decision rules for an rpart tree
#'
#' Extracts the decision path rules for selected nodes (or all nodes / leaves
#' only) of a fitted \code{rpart} tree.
#'
#' @param x A fitted \code{rpart} object.
#' @param node Character or numeric vector of node ids to extract.  If
#'   \code{NULL} (default) all nodes are returned.
#' @param leafonly Logical.  If \code{TRUE} only terminal (leaf) nodes are
#'   returned.  Default is \code{FALSE}.
#'
#' @return An object of class \code{"rules"}, a list with components
#'   \code{frame}, \code{ylevels}, \code{ds.size}, and \code{path}.
#'   Returns \code{NA} if no nodes match the selection.
#'
#' @examples
#' r <- rpart::rpart(Species ~ ., data = iris)
#' rules(r)
#' rules(r, leafonly = TRUE)
#'
#' @seealso \code{\link{node}}, \code{\link[rpart]{path.rpart}}



#' @family tree  
#' @concept classification  
#' @concept modelling
#'
#'
#' @export
rules <- function(x, node = NULL, leafonly = FALSE) {
  
  if (!inherits(x, "rpart"))
    stop("'x' must be an rpart object")
  
  node <- if (is.null(node)) rownames(x$frame) else as.character(node)
  frm  <- x$frame[node, ]
  
  if (leafonly)
    frm <- frm[frm$var == "<leaf>", ]
  
  if (nrow(frm) == 0L)
    return(NA)
  
  structure(
    list(
      frame    = frm,
      ylevels  = attr(x, "ylevels"),
      ds.size  = x$frame[1L, ]$n,
      path     = rpart::path.rpart(x, nodes = as.numeric(rownames(frm)),
                                   print.it = FALSE)
    ),
    class = "rules"
  )
}


#' @export
print.rules <- function(x, ...) {
  for (i in seq_len(nrow(x$frame))) {
    cat(sprintf("\n Rule number: %s ", rownames(x$frame)[i]))
    if (x$frame[i, 1L] == "<leaf>")
      cat(sprintf("[yval=%s cover=%d (%.0f%%) prob=%0.2f]",
                  x$ylevels[x$frame[i, ]$yval],
                  x$frame[i, ]$n,
                  100 * x$frame[i, ]$n / x$ds.size,
                  x$frame[i, ]$yval2[, 5L]))
    cat("\n")
    cat(sprintf("   %s\n", x$path[[i]][-1L]), sep = "")
  }
  cat("\n")
  invisible(x)
}
