
#' Split labels for each node of an rpart tree
#'
#' Returns a two-column character matrix with the left and right split labels
#' for every node of a fitted \code{rpart} tree.  Leaf nodes are represented
#' by empty strings.
#'
#' @param x A fitted \code{rpart} object.
#'
#' @return A character matrix with columns \code{"cutleft"} and
#'   \code{"cutright"} and one row per node.
#'
#' @examples
#' r <- rpart::rpart(Species ~ ., data = iris)
#' splits(r)
#'



#' @family tree  
#' @concept classification  
#' @concept modelling
#'
#'
#' @export
splits <- function(x) {
  
  if (!inherits(x, "rpart"))
    stop("'x' must be an rpart object")
  
  out <- labels(x, collapse = FALSE)
  # Mark leaf nodes
  is_leaf <- apply(out, 2L, `==`, "<leaf>")
  out[is_leaf] <- ""
  # Categorical splits get a ":" prefix; numeric splits keep their sign
  is_cat <- (!is_leaf) &
    apply(out, 2L, substr, 1L, 1L) != "<" &
    apply(out, 2L, substr, 1L, 1L) != ">"
  out[is_cat]  <- paste0(":", out[is_cat])
  out[!is_cat & !is_leaf] <- paste0(
    apply(out, 2L, substr, 1L, 1L),
    apply(out, 2L, substring, 3L, 1e4)
  )[!is_cat & !is_leaf]
  colnames(out) <- c("cutleft", "cutright")
  out
}
