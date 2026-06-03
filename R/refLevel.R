
#' Reference level of factor predictors in a model
#'
#' Extracts the reference (baseline) category for every factor predictor
#' in a fitted model object.  The reference level is derived from the
#' contrast matrix that was actually used during fitting, so the result is
#' correct even when \code{base} in \code{\link[stats]{contr.treatment}} is
#' not 1 or when contrasts have been set globally via
#' \code{\link[base]{options}}.
#'
#' @param x A fitted model object with a \code{terms} attribute and a
#'   \code{model} data frame (e.g. objects of class \code{"lm"},
#'   \code{"glm"}, \code{"lmerMod"}, or \code{"glmerMod"}).
#'
#' @return A named character vector whose names are the factor predictor
#'   variables and whose values are the corresponding reference levels.
#'   Returns a zero-length named character vector when the model contains
#'   no factor predictors.
#'
#' @details
#' The function inspects \code{attr(model.matrix(x), "contrasts")} for
#' each factor predictor.
#'
#' \itemize{
#'   \item If the contrast is stored as a \emph{character string}
#'         (e.g. \code{"contr.treatment"}), the first level of the factor
#'         is returned as the reference.
#'   \item If the contrast is stored as a \emph{matrix}, the reference
#'         level is identified as the unique row whose entries are all zero
#'         (i.e. the row that does not map to any dummy column).
#' }
#'
#' Contrasts other than treatment contrasts (e.g. \code{contr.sum},
#' \code{contr.helmert}) are not supported and trigger an informative error.
#'
#' @examples
#' m <- lm(Sepal.Length ~ Species, data = iris)
#' refLevel(m)
#' # Species
#' # "setosa"
#'
#' # Custom base level
#' iris2 <- iris
#' contrasts(iris2$Species) <- contr.treatment(3, base = 2)
#' m2 <- lm(Sepal.Length ~ Species, data = iris2)
#' refLevel(m2)
#' # Species
#' # "versicolor"
#'
#' @seealso \code{\link[stats]{contrasts}}, \code{\link[stats]{contr.treatment}}
#' @export
refLevel <- function(x) {
  
  if (!inherits(x, c("lm", "glm", "lmerMod", "glmerMod", "lmrob")))
    stop("x must be a model object (lm, glm, lmer, ...)")
  
  # Build contrast list once — reused for every predictor
  cs <- attr(model.matrix(x), "contrasts")
  
  # Return the reference level for a single factor variable
  refCat <- function(var) {
    ct  <- cs[[var]]
    lvl <- levels(x$model[[var]])
    
    # Contrast stored as a string (e.g. after options(contrasts = ...))
    if (is.character(ct)) {
      if (ct == "contr.treatment")
        return(lvl[1L])
      stop(sprintf("Variable '%s': unsupported contrast '%s'", var, ct))
    }
    
    # Contrast stored as a matrix: reference row has no 1-entry
    if (is.matrix(ct)) {
      ros     <- rowSums(ct == 1)
      ref_idx <- which(ros == 0L)
      if (length(ref_idx) != 1L)
        stop(sprintf(
          "Variable '%s': cannot determine reference level from contrast matrix",
          var
        ))
      return(lvl[ref_idx])
    }
    
    stop(sprintf("Variable '%s': unrecognised contrast format", var))
  }
  
  # Identify all factor predictors (exclude the response variable)
  dc    <- attr(x[["terms"]], "dataClasses")
  resp  <- all.vars(formula(x))[1L]
  fpred <- names(dc)[dc %in% c("factor", "ordered") & names(dc) != resp]
  
  # Keep only predictors that actually have contrast information
  fpred <- intersect(fpred, names(cs))
  
  if (length(fpred) == 0L)
    return(setNamesX(character(0L), character(0L)))
  
  sapply(fpred, refCat)
}

