
#' Structured node information for an rpart tree
#'
#' Returns a structured list with detailed information about selected nodes
#' of a fitted \code{rpart} tree, including split details, class counts,
#' and probabilities.
#'
#' @param x A fitted \code{rpart} object (must have been fitted with
#'   \code{model = TRUE} and \code{y = TRUE}).
#' @param node Character or numeric vector of node ids.  If \code{NULL}
#'   (default) all nodes are returned.
#' @param type Character string.  One of \code{"all"} (default),
#'   \code{"split"} (internal nodes only), or \code{"leaf"} (terminal
#'   nodes only).
#' @param digits Integer.  Number of significant digits for split values.
#'   Default is \code{3}.
#'
#' @return An object of class \code{"node"}, a named list with one element
#'   per selected node.  Each element contains: \code{id}, \code{vname},
#'   \code{isleaf}, \code{nobs}, \code{group}, \code{ycount}, \code{yprob},
#'   \code{nodeprob}, \code{complexity}, \code{tprint}, and (for split
#'   nodes) \code{sons}, \code{sons_n}, \code{primarysplits},
#'   \code{surrogatesplits}.
#'
#' @examples
#' r <- rpart::rpart(Species ~ ., data = iris)
#' node(r)
#' node(r, type = "leaf")
#'
#' @seealso \code{\link{rules}}


#' @export
node <- function(x, node = NULL, type = c("all", "split", "leaf"),
                 digits = 3L) {
  
  if (!inherits(x, "rpart"))
    stop("'x' must be an rpart object")
  
  type   <- match.arg(type)
  ff     <- x$frame
  ylevel <- attr(x, "ylevels")
  id     <- as.integer(rownames(ff))
  rows   <- seq_along(id)
  is.leaf <- ff$var == "<leaf>"
  index   <- cumsum(c(1L, ff$ncompete + ff$nsurrogate + !is.leaf))
  
  # Build cut labels for all splits
  cuts <- character(0L)
  sname <- NULL
  if (!all(is.leaf)) {
    sname <- rownames(x$splits)
    cuts  <- character(nrow(x$splits))
    temp  <- x$splits[, 2L]
    for (i in seq_along(cuts)) {
      cuts[i] <- if (temp[i] == -1L)
        paste("<", format(signif(x$splits[i, 4L], digits)))
      else if (temp[i] == 1L)
        paste("<", format(signif(x$splits[i, 4L], digits)))
      else
        paste("splits as ",
              paste(c("L", "-", "R")[x$csplit[x$splits[i, 4L], seq_len(temp[i])]],
                    collapse = "", sep = ""),
              collapse = "")
    }
    if (any(temp < 2L))
      cuts[temp < 2L] <- format(cuts[temp < 2L], justify = "left")
    cuts <- paste0(cuts, ifelse(temp >= 2L, ",",
                                ifelse(temp == 1L, " to the right,",
                                       " to the left, ")))
  }
  
  tmp <- if (is.null(ff$yval2)) ff$yval[rows]
  else                    ff$yval2[rows, , drop = FALSE]
  tmp    <- unname(tmp)
  tprint <- x$functions$summary(tmp, ff$dev[rows], ff$wt[rows], ylevel, digits)
  nclass <- if (is.matrix(tmp)) (ncol(tmp) - 2L) / 2L else 0L
  
  # Determine which nodes to include
  nid <- if (is.null(node))
    seq_along(rows)
  else
    seq_along(rows)[which(!is.na(rows[match(rownames(ff), as.character(node))]))]
  
  nlst <- vector("list", length(nid))
  names(nlst) <- as.character(id[nid])
  
  for (k in seq_along(nid)) {
    ii   <- nid[k]
    i    <- rows[ii]
    nlbl <- as.character(id[i])
    nn   <- ff$n[i]
    
    nlst[[k]] <- list(
      id         = id[i],
      vname      = as.character(ff$var[i]),
      isleaf     = is.leaf[i],
      nobs       = nn,
      group      = if (nclass > 0L) ylevel[tmp[i, 1L]] else NA,
      ycount     = if (nclass > 0L) tmp[i, 1L + seq_len(nclass)] else NA,
      yprob      = if (nclass > 0L) tmp[i, 1L + nclass + seq_len(nclass)] else NA,
      nodeprob   = if (nclass > 0L) tmp[i, 2L * nclass + 2L] else NA,
      complexity = ff$complexity[i],
      tprint     = tprint[ii]
    )
    
    if (!is.leaf[i]) {
      sons   <- 2L * id[i] + c(0L, 1L)
      sons_n <- ff$n[match(sons, id)]
      j      <- seq(index[i], length.out = 1L + ff$ncompete[i])
      temp_j <- if (all(nchar(cuts[j], "w") < 25L))
        format(cuts[j], justify = "left")
      else cuts[j]
      
      nlst[[k]]$sons   <- setNamesX(sons,   c("left", "right"))
      nlst[[k]]$sons_n <- setNamesX(sons_n, c("left", "right"))
      nlst[[k]]$primarysplits <- data.frame(
        split     = sname[j],
        direction = temp_j,
        improve   = x$splits[j, 3L],
        missing   = nn - x$splits[j, 1L],
        stringsAsFactors = FALSE
      )
      
      if (ff$nsurrogate[i] > 0L) {
        j2     <- seq(1L + index[i] + ff$ncompete[i], length.out = ff$nsurrogate[i])
        temp_j2 <- if (all(nchar(cuts[j2], "w") < 25L))
          format(cuts[j2], justify = "left")
        else cuts[j2]
        nlst[[k]]$surrogatesplits <- data.frame(
          split     = sname[j2],
          direction = temp_j2,
          agree     = x$splits[j2, 3L],
          adj       = x$splits[j2, 5L],
          n_split   = x$splits[j2, 1L],
          stringsAsFactors = FALSE
        )
      }
    }
    names(nlst)[k] <- nlbl
  }
  
  # Filter by type
  if (type == "leaf")
    nlst <- nlst[vapply(nlst, `[[`, logical(1L), "isleaf")]
  else if (type == "split")
    nlst <- nlst[!vapply(nlst, `[[`, logical(1L), "isleaf")]
  
  structure(nlst, class = "node")
}


#' @export
print.node <- function(x, digits = 3L, ...) {
  
  if (length(x) == 0L) {
    cat("list()\n")
    return(invisible(x))
  }
  
  for (nd in x) {
    cat(sprintf("\nnode number %d: %d observation%s",
                nd$id, nd$nobs, if (nd$nobs == 1L) "" else "s"))
    if (nd$isleaf)
      cat("\n")
    else
      cat(sprintf(",    complexity param = %s\n",
                  format(signif(nd$complexity, digits))))
    
    cat(nd$tprint, "\n")
    
    if (!nd$isleaf) {
      cat(sprintf("  left son=%d (%d obs)  right son=%d (%d obs)\n",
                  nd$sons[1L], nd$sons_n[1L],
                  nd$sons[2L], nd$sons_n[2L]))
      cat("  Primary splits:\n")
      for (k in seq_len(nrow(nd$primarysplits))) {
        lx <- nd$primarysplits[k, ]
        cat(sprintf("      %-20s %s improve=%s, (%d missing)\n",
                    lx$split, lx$direction,
                    format(signif(lx$improve, digits)),
                    lx$missing))
      }
      if (!is.null(nd$surrogatesplits)) {
        cat("  Surrogate splits:\n")
        for (k in seq_len(nrow(nd$surrogatesplits))) {
          lx <- nd$surrogatesplits[k, ]
          cat(sprintf("      %-20s %s agree=%s, adj=%s, (%d split)\n",
                      lx$split, lx$direction,
                      format(round(lx$agree, 3L)),
                      format(round(lx$adj,   3L)),
                      lx$n_split))
        }
      }
    }
    cat("\n")
  }
  invisible(x)
}
