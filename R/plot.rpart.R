
#' Plot an rpart tree using rpart.plot with node labels
#'
#' Overrides the default \code{rpart} plot and delegates to
#' \code{\link[rpart.plot]{rpart.plot}}, optionally overlaying node id
#' labels on each box.
#'
#' @param x A fitted \code{rpart} object.
#' @param type Integer passed to \code{rpart.plot}.  Default \code{2}.
#' @param extra Passed to \code{rpart.plot}.  Default \code{"auto"}.
#' @param under Logical.  Default \code{FALSE}.
#' @param fallen.leaves Logical.  Default \code{TRUE}.
#' @param digits Integer.  Default \code{2}.
#' @param varlen Integer.  Default \code{0} (full names).
#' @param faclen Integer.  Default \code{0} (full factor labels).
#' @param roundint Logical.  Default \code{TRUE}.
#' @param cex Numeric or \code{NULL}.
#' @param tweak Numeric.  Default \code{1}.
#' @param clip.facs Logical.  Default \code{FALSE}.
#' @param clip.right.labs Logical.  Default \code{TRUE}.
#' @param snip Logical.  Default \code{FALSE}.
#' @param box.palette Character or list.  Default \code{"auto"} uses the
#'   package palette.
#' @param shadow.col Colour of node shadows.  Default \code{0} (none).
#' @param node.labels Logical.  If \code{TRUE} (default), node ids are
#'   printed above each box.
#' @param ... Further arguments passed to \code{rpart.plot}.
#'
#' @return Invisibly returns the list returned by \code{rpart.plot}.
#'
#' @export
plot.rpart <- function(x      = stop("no 'x' arg"),
                       type   = 2L,
                       extra  = "auto",
                       under  = FALSE,
                       fallen.leaves    = TRUE,
                       digits           = 2L,
                       varlen           = 0L,
                       faclen           = 0L,
                       roundint         = TRUE,
                       cex              = NULL,
                       tweak            = 1,
                       clip.facs        = FALSE,
                       clip.right.labs  = TRUE,
                       snip             = FALSE,
                       box.palette      = "auto",
                       shadow.col       = 0,
                       node.labels      = TRUE,
                       ...) {

  if (identical(box.palette, "auto"))
    box.palette <- as.list(fade(pal()))

  b <- rpart.plot::rpart.plot(
    x               = x,
    type            = type,
    extra           = extra,
    under           = under,
    fallen.leaves   = fallen.leaves,
    digits          = digits,
    varlen          = varlen,
    faclen          = faclen,
    roundint        = roundint,
    cex             = cex,
    tweak           = tweak,
    clip.facs       = clip.facs,
    clip.right.labs = clip.right.labs,
    snip            = snip,
    box.palette     = box.palette,
    shadow.col      = shadow.col,
    ...
  )

  if (node.labels) {
    oldpar <- par(xpd = TRUE)
    on.exit(par(oldpar), add = TRUE)
    boxedText(
      x      = b$boxes$x1,
      y      = b$boxes$y2,
      labels = rownames(x$frame),
      border  = NA,
      font    = 2L,
      col     = "steelblue",
      bg      = addAlpha("white", 0.7),
      cex     = 0.8
    )
  }

  invisible(b)
}


# Internal helpers --------------------------------------------------------

#' @keywords internal
.treeDepth <- function(nodes) {
  depth <- floor(log(nodes, base = 2) + 1e-7)
  as.vector(depth - min(depth))
}



