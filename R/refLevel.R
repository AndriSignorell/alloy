
#' Reference level of factor predictors in a model
#'
#' Extracts the reference (baseline) category for every factor predictor
#' in a fitted model object.  The reference level is derived from the
#' contrast matrix that was actually used during fitting, so the result is
#' correct even when \code{base} in \code{\link[stats]{contr.treatment}} is
#' not 1 or when contrasts have been set globally via
#' \code{\link[base]{options}}.
#'
#' @param fit A fitted model object with a \code{terms} attribute and a
#'   \code{model} data frame (e.g. objects of class \code{"lm"},
#'   \code{"glm"}, \code{"lmerMod"}, \code{"glmerMod"} or
#'   \code{"betareg"}).  Mixed models fitted with \code{\link{fitMod}}
#'   are accepted as well.
#'
#' @return A named character vector whose names are the factor predictor
#'   variables and whose values are the corresponding reference levels.
#'   Returns a zero-length named character vector when the model contains
#'   no factor predictors.
#'
#' @details
#' The function inspects \code{attr(model.matrix(fit), "contrasts")} for
#' each factor predictor.
#'
#' \itemize{
#'   \item If the contrast is stored as a \emph{character string}
#'         (e.g. \code{"contr.treatment"}), the first level of the factor
#'         is returned as the reference.
#'   \item If the contrast is stored as a \emph{matrix}, the reference
#'         level is identified as the unique row whose entries are all zero
#'         (i.e. the row that does not map to any dummy column).  The
#'         matrix must be a treatment coding, that is consist of 0 and 1
#'         only, with a single 1 in every other row.
#' }
#'
#' Contrasts other than treatment contrasts (e.g. \code{contr.sum},
#' \code{contr.helmert}) are not supported and trigger an informative error,
#' whether they are stored by name or as a matrix.
#'
#' For beta regressions (\code{"betareg"}) the factor predictors of the mean
#' and of the precision model are returned together.
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

#' @family regression.utils  
#' @concept regression
#'
#'
#' @export
refLevel <- function(fit) {
  
  # mixed models fitted via fitMod() wrap the lme4 object
  if (inherits(fit, "FitMod.lme4"))
    fit <- fit$model
  
  if (!inherits(fit, c("lm", "glm", "lmerMod", "glmerMod", "lmrob", "survreg",
                       "coxph", "betareg")))
    stop("'fit' must be a model object (lm, glm, lmer, ...)")
  
  .refLevels(fit, strict = TRUE)
}



# == internal helper functions ============================================


# Workhorse of refLevel().  With strict = FALSE, factors whose reference
# level cannot be determined (no treatment coding) are left out instead of
# raising an error; this is what the print methods need, a contrast must
# never keep a model from being printed.
# 'part' only matters for beta regressions, see .factorCoding().
#' @keywords internal
.refLevels <- function(fit, strict = TRUE,
                       part = c("mean", "precision")) {
  
  coding <- .factorCoding(fit, part)
  fpred  <- names(coding$levels)
  
  if (length(fpred) == 0L)
    return(setNamesX(character(0L), character(0L)))
  
  ref <- vapply(fpred, function(var)
    .refCategory(coding$contrasts[[var]], coding$levels[[var]], var, strict),
    character(1L))
  
  ref[!is.na(ref)]
}


# Contrasts and levels of the factor predictors of a model, as two lists
# named by variable.
#' @keywords internal
.factorCoding <- function(fit, part = c("mean", "precision")) {
  
  # betareg keeps terms, levels and contrasts per model part.  The levels
  # are accessed by position, their names depend on the distribution
  # ("mean"/"precision" or "mu"/"phi").
  if (inherits(fit, "betareg")) {
    part <- match.arg(part, several.ok = TRUE)
    cs   <- list()
    lvl  <- list()
    for (p in part) {
      cs  <- c(cs, attr(model.matrix(fit, model = p), "contrasts"))
      lvl <- c(lvl, fit[["levels"]][[match(p, c("mean", "precision"))]])
    }
    fpred <- intersect(unique(names(lvl)), names(cs))
    return(list(contrasts = cs[fpred], levels = lvl[fpred]))
  }
  
  cs <- attr(model.matrix(fit), "contrasts")
  
  # lme4 objects are S4: no $, the model frame carries terms and levels
  isS4fit <- isS4(fit)
  mf      <- if (isS4fit) model.frame(fit) else fit$model
  tt      <- if (isS4fit) attr(mf, "terms") else fit[["terms"]]
  xlev    <- if (isS4fit) NULL else fit$xlevels
  
  # Identify all factor predictors (exclude the response variable)
  dc    <- attr(tt, "dataClasses")
  resp  <- all.vars(formula(fit))[1L]
  fpred <- names(dc)[dc %in% c("factor", "ordered") & names(dc) != resp]
  
  # Keep only predictors that actually have contrast information
  fpred <- intersect(fpred, names(cs))
  
  # For survreg/tobit: levels are not in model$model - get from xlevels
  lvl <- lapply(setNamesX(fpred, fpred), function(var)
    if (!is.null(xlev[[var]])) xlev[[var]] else levels(mf[[var]]))
  
  list(contrasts = cs[fpred], levels = lvl)
}


# Reference level of a single factor, from its contrast (name or matrix)
# and its levels.  Without a treatment coding there is no reference level:
# an error if strict, NA otherwise.
#' @keywords internal
.refCategory <- function(ct, lvl, var, strict = TRUE) {
  
  fail <- function(msg) {
    if (strict)
      stop(sprintf("Variable '%s': %s", var, msg), call. = FALSE)
    NA_character_
  }
  
  if (is.character(ct)) {
    if (identical(ct, "contr.treatment"))
      return(lvl[1L])
    return(fail(sprintf("unsupported contrast '%s'", ct[1L])))
  }
  
  if (is.matrix(ct)) {
    # treatment coding: 0 and 1 only, a single all-zero row (the
    # reference) and exactly one 1 in every other row.  Looking for the
    # row without a 1 alone would take the last level of a sum coding
    # for a reference.
    isRef <- rowSums(ct != 0) == 0L
    if (all(ct %in% 0:1) && sum(isRef) == 1L && all(rowSums(ct) <= 1))
      return(lvl[isRef])
    return(fail("cannot determine reference level from contrast matrix"))
  }
  
  fail("unrecognised contrast format")
}

