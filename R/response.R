
#' Extract the response variable from a fitted model
#'
#' Retrieves the response vector from a fitted model object, with special
#' handling for model classes that do not store a standard \code{terms}
#' component (\code{C5.0}, \code{rpart}, \code{naive_bayes}).  The name of
#' the response variable is attached as the \code{"response"} attribute of
#' the return value.
#'
#' @param x A fitted model object.  Supported classes include \code{"lm"},
#'   \code{"glm"}, \code{"multinom"}, \code{"polr"}, \code{"rpart"},
#'   \code{"C5.0"}, and \code{"naive_bayes"}.  Any other class is attempted
#'   via the default \code{\link[stats]{model.frame}} path.
#' @param ... Currently unused; reserved for future methods.
#'
#' @return The response vector as returned by
#'   \code{\link[stats]{model.response}}, or a factor reconstructed from
#'   internal model slots for \code{rpart}.  The attribute
#'   \code{"response"} carries the name of the response variable as a
#'   character string, or \code{NA_character_} if it cannot be determined.
#'
#' @seealso \code{\link[stats]{model.response}}, \code{\link[stats]{model.frame}}


#' @family regression.utils  
#' @concept regression
#'
#'
#' @export
response <- function(x, ...) {
  
  # Unwrap special wrappers - extract response from stored formula
  if (inherits(x, "FitMod.xgboost") || inherits(x, "FitMod.lme4")) {
    if (is.null(x$formula) || is.null(x$call$data))
      stop("Cannot recover response: formula or data not stored in object")
    mf  <- model.frame(x$formula,
                       data = eval(x$call$data, envir = parent.frame()))
    res <- model.response(mf)
    attr(res, "response") <- as.character(x$formula[[2L]])
    return(res)
  }
  
  # glmnet: formula stored directly on the object      # <-- hier einfügen
  if (!is.null(x$fitfn) && x$fitfn == "glmnet" && !is.null(x$formula)) {
    mf  <- model.frame(x$formula,
                       data = eval(x$call$data, envir = parent.frame()))
    res <- model.response(mf)
    attr(res, "response") <- as.character(x$formula[[2L]])
    return(res)
  }
  
  res <- if (inherits(x, "C5.0")) {
    .response_from_call(x)
    
  } else if (inherits(x, "rpart")) {
    ylevels <- x$ylevels %||% attr(x, "ylevels")
    if (is.null(ylevels))
      stop("'x' is an rpart regression tree; cannot extract factor response")
    factor(ylevels[x$y], levels = ylevels)
    
  } else if (inherits(x, "naive_bayes")) {
    .response_from_call(x)
    
  } else {
    mf <- tryCatch(
      model.frame(x),
      error = function(e) stop(
        "Cannot extract model frame from object of class '",
        paste(class(x), collapse = "/"), "': ", conditionMessage(e)
      )
    )
    model.response(mf)
  }
  
  attr(res, "response") <- .response_name(x)
  res
}




# == internal helper functions =============================================


#' Extract and validate a binary response from a fitted model
#'
#' Wrapper around \code{\link{response}} that additionally validates that
#' the response is a factor with exactly two levels, as required for ROC
#' analysis.
#'
#' @inheritParams response
#'
#' @return A factor with exactly two levels.  The attribute
#'   \code{"response"} carries the name of the response variable.
#'
#' @seealso \code{\link{response}}, \code{\link[pROC]{roc}}
#' @keywords internal
.response_binary <- function(x, ...) {
  res <- response(x, ...)
  
  if (!is.factor(res))
    stop("Response variable is not a factor; a binary classification model is required")
  
  if (nlevels(res) != 2L)
    stop(
      "Response variable has levels: ",
      paste(levels(res), collapse = ", "),
      ". Exactly two levels are required for ROC analysis."
    )
  
  res
}



# Extract response name from terms, works for any model that has x$terms
.response_name <- function(x) {
  tryCatch({
    tm <- terms(x)
    as.character(attr(tm, "variables"))[attr(tm, "response") + 1L]
  }, error = function(e) NA_character_)
}


# Recover response vector for models that store formula in call (C5.0, naive_bayes)
.response_from_call <- function(x) {
  if (is.null(x$call$formula))
    stop(
      "Object of class '", paste(class(x), collapse = "/"),
      "' does not contain a recoverable formula in 'x$call$formula'"
    )
  f <- tryCatch(
    eval(x$call$formula, envir = parent.frame()),
    error = function(e) stop(
      "Cannot evaluate formula from call for object of class '",
      paste(class(x), collapse = "/"), "'. ",
      "Was the model created in a different environment? ",
      "Consider storing the data explicitly. Original error: ",
      conditionMessage(e)
    )
  )
  x$terms <- f
  model.response(model.frame(x))
}

