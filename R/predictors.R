#' Predictors of a Fitted Model
#'
#' Returns the names of the terms on the right hand side of a fitted
#' model. The information is in \code{attr(terms(x), "term.labels")}; this
#' is the same thing under a name one can guess.
#'
#' @details
#' What comes back are \emph{terms}, not variables: a transformed term
#' appears as it was written (\code{"log(insulin)"}), an interaction as
#' \code{"a:b"}, and a variable used twice appears once per term. That is
#' the right granularity for looping over diagnostics, because a diagnostic
#' is per term. For the underlying variable names - what a data frame would
#' have to contain - use \code{all.vars(formula(x))[-1]}.
#'
#' \code{numeric = TRUE} keeps the terms that are numeric in the model
#' frame. Diagnostics of functional form only apply to those: linearity in
#' the logit is not a question one can ask of a factor.
#'
#' @param x a fitted model - anything with a \code{\link[stats]{terms}}
#'   method, including \code{"FitMod"}, \code{"glm"} and \code{"lm"}.
#' @param numeric logical; keep only terms that are numeric in the model
#'   frame. Default \code{FALSE}.
#'
#' @return a character vector of term labels, empty for an
#'   intercept-only model.
#'
#' @seealso
#' \code{\link{binnedResid}} and \code{\link{plotPartialResid}}, the two
#' diagnostics that are computed per term;
#' [model-diagnostics-overview] for an overview.
#'
#' @family modelling
#' @concept modelling
#'
#' @examples
#' fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
#'
#' predictors(fitLogit)
#' predictors(fitLogit, numeric = TRUE)     # rank drops out
#'
#' # the loop this exists for
#' bins <- binnedResid(fitLogit, var = predictors(fitLogit))
#' plotFacet(bins, dim = c(1, 3), panelFun = panelBinnedResid,
#'           xlim = "free", ylab = "mean residual")
#'
#' @export
predictors <- function(x, numeric = FALSE) {

  fit <- .stripFitMod(x)

  labs <- attr(stats::terms(fit), "term.labels")

  if (is.null(labs))
    return(character(0))

  if (!numeric)
    return(labs)

  mf <- stats::model.frame(fit)

  labs[vapply(labs,
              function(nm) !is.null(mf[[nm]]) && is.numeric(mf[[nm]]),
              logical(1L))]
}
