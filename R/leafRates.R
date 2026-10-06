
#' Misclassification rates per leaf node
#'
#' Computes the number of correctly and incorrectly classified observations
#' in each terminal (leaf) node of a classification tree.
#'
#' @param fit A fitted \code{rpart} classification object (i.e.
#'   \code{fit$method != "anova"}).
#'
#' @return An object of class \code{c("leafRates", "list")} with components:
#'   \describe{
#'     \item{\code{node}}{node ids of the leaf nodes.}
#'     \item{\code{freq}}{Integer matrix with columns \code{"right"} and
#'       \code{"wrong"}.}
#'     \item{\code{p.row}}{Row proportions of \code{freq}.}
#'     \item{\code{mfreq}}{Total observations per leaf.}
#'     \item{\code{mperc}}{Proportion of total observations per leaf.}
#'   }
#'
#' @examples
#' r <- rpart::rpart(Species ~ ., data = iris)
#' leafRates(r)
#'


#' @family tree  
#' @concept classification  
#' @concept modelling
#'
#'
#' @export
leafRates <- function(fit) {
  
  if (!inherits(fit, "rpart"))
    stop("'fit' must be an rpart object")
  if (fit$method == "anova") {
    warning("leafRates is not available for regression trees.")
    return(NA_real_)
  }
  
  xx <- fit$frame$yval2[fit$frame$var == "<leaf>", ]
  
  if (is.matrix(xx)) {
    z        <- matrix(0L, nrow(xx), 2L)
    for (i in seq_len(nrow(xx)))
      z[i, 1L] <- xx[i, xx[i, 1L] + 1L]
    z[, 2L]  <- rowSums(xx[, 2L:((ncol(xx) - 1L) / 2L + 1L)]) - z[, 1L]
  } else {
    z <- matrix(xx[c(3L, 2L)], 1L, 2L)
  }
  
  colnames(z) <- c("right", "wrong")
  rownames(z) <- rownames(fit$frame[fit$frame$var == "<leaf>", ])
  
  structure(
    list(
      node  = rownames(z),
      freq  = z,
      p.row = prop.table(z, 1L),
      mfreq = rowSums(z),
      mperc = prop.table(rowSums(z))
    ),
    class = c("leafRates", "list")
  )
}


#' @export
print.leafRates <- function(x, digits = 1L, ...) {
  out <- cbind(
    node  = x$node,
    fm(x$freq,  fmt = "abs"),
    fm(x$p.row, fmt = "%", digits = digits),
    total = fm(x$mfreq, fmt = "abs"),
    perc  = fm(x$mperc, fmt = "%", digits = digits)
  )
  colnames(out) <- c("node", "right", "wrong", "right%", "wrong%",
                     "total", "perc%")
  print(out, quote = FALSE, right = TRUE)
  invisible(x)
}

