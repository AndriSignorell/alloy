#' Predict method for FitMod objects
#'
#' Unified predict interface for all models fitted via \code{\link{fitMod}}.
#' For regression and survival models the predicted values are returned as a
#' numeric vector.  For classification models either class probabilities,
#' predicted classes, or both are returned as a \code{data.frame} with
#' consistent column names across all model types.
#'
#' @param object A fitted model of class \code{"FitMod"}.
#' @param newdata Optional data frame of new observations.  If omitted,
#'   fitted values on the training data are returned.
#' @param output Character string controlling the output for classification
#'   models.  One of \code{"prob"} (default), \code{"class"}, or
#'   \code{"both"}.  For \code{fitfn = "rpart"} additionally
#'   \code{"where"} (row index of the predicted leaf in the tree frame) or
#'   \code{"leaf"} (node label of the predicted leaf) are available.
#'   Ignored for regression and survival models.
#' @param s For \code{fitfn = "glmnet"} only: the value of the penalty
#'   parameter \eqn{\lambda} at which predictions are made.  Passed to
#'   \code{\link[glmnet]{predict.cv.glmnet}}.  Default is
#'   \code{"lambda.1se"}.
#' @param type For regression models (\code{lm}, \code{glm}, etc.): the
#'   \code{type} argument passed to the underlying \code{predict} method.
#'   If not supplied, \code{"response"} is used, so that predictions are
#'   returned on the response scale both with and without \code{newdata}.
#'   For Cox models the default is \code{"risk"}; for parametric survival
#'   models (incl. \code{tobit}) the default is \code{"response"}.
#'   Ignored for classification models (use \code{output} instead).
#' @param ... Further arguments passed to the underlying predict method.
#'
#' @return
#' \describe{
#'   \item{Regression models}{A numeric vector of fitted/predicted values
#'     on the response scale.  This includes regression variants of the
#'     machine-learning methods (\code{rpart} with \code{method = "anova"},
#'     \code{randomForest}/\code{svm}/\code{nnet} regressions,
#'     \code{glmnet} with gaussian/poisson family, \code{xgboost} with a
#'     regression objective).}
#'   \item{Survival models (\code{coxph})}{A numeric vector of predicted
#'     risk scores (\code{type = "risk"} by default).}
#'   \item{Parametric survival models (\code{weibull}, \code{exponential},
#'     \code{lognormal}, \code{loglogistic}, \code{tobit})}{A numeric
#'     vector of predicted survival times / expected responses
#'     (\code{type = "response"} by default).}
#'   \item{Classification models}{
#'     \describe{
#'       \item{\code{output = "prob"}}{A \code{data.frame} with one column
#'         per class containing predicted probabilities.  Column names match
#'         the factor levels of the response variable.}
#'       \item{\code{output = "class"}}{A \code{data.frame} with a single
#'         column \code{class} (factor) containing the predicted class.}
#'       \item{\code{output = "both"}}{The probability columns and the
#'         \code{class} column combined in one \code{data.frame}.}
#'     }
#'   }
#' }
#'
#' @details
#' Whether a model is treated as classification or regression is decided
#' from the fitted object itself (e.g. \code{rpart$method},
#' \code{randomForest$type}, the \code{family}/\code{objective} stored by
#' \code{fitMod()} for \code{glmnet}/\code{xgboost}), not from the fitting
#' method alone.
#'
#' For classification models the column order of probability outputs is
#' always aligned with the factor levels of the response variable,
#' regardless of which model type is used.  This ensures that
#' \code{predict(fitLogit)} and \code{predict(fitRf)} return columns in
#' the same order.
#'
#' Models that require explicit \code{newdata} even for training-data
#' predictions (e.g. \code{svm}, \code{C5.0}, \code{randomForest}) are
#' handled transparently via an internal helper.  Note that for
#' \code{randomForest} this returns in-sample (not out-of-bag)
#' predictions, consistent with the fitted-values semantics of all other
#' methods.
#'
#' For \code{fitfn = "glmnet"} and \code{"xgboost"}, design matrices for
#' \code{newdata} are rebuilt from the \code{terms} and factor levels of
#' the training data, so new data may contain a subset of the training
#' factor levels.
#'
#' For \code{fitfn = "logit"}, calling \code{predict(object)} returns a
#' two-column probability \code{data.frame} (like all other classifiers).
#' To obtain the linear predictor (log-odds), use
#' \code{predict(object, type = "link")}.
#'
#' @examples
#' # Regression
#' fitLm <- fitMod(Fertility ~ ., swiss)
#' head(predict(fitLm))
#'
#' # Binary classification - probabilities
#' fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
#' head(predict(fitLogit))
#' head(predict(fitLogit, output = "both"))
#'
#' # Multinomial classification
#' if (requireNamespace("nnet", quietly = TRUE)) {
#'   fitMult <- fitMod(ice_cream ~ video + puzzle + female,
#'                     IceCream, fitfn = "multinom")
#'   head(predict(fitMult, output = "both"))
#' }
#'
#' # Cox model - risk scores
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   fitCox <- fitMod(Surv(foltime, folstatus) ~ gender, Whas100,
#'                    fitfn = "coxph")
#'   head(predict(fitCox))
#'
#'   # Parametric survival - expected survival time
#'   fitWei <- fitMod(Surv(foltime, folstatus) ~ gender + age, Whas100,
#'                    fitfn = "weibull")
#'   head(predict(fitWei))
#' }
#'
#' @seealso \code{\link{fitMod}}, \code{\link{print.FitMod}}
#' @family modelling
#' @concept prediction
#' @export
predict.FitMod <- function(object, newdata = NULL,
                           output = c("prob", "class", "both", "where", "leaf"),
                           s = "lambda.1se",
                           type = NULL,
                           ...) {

  output <- match.arg(output)
  fitfn  <- object$fitfn

  # Strip FitMod class to avoid infinite recursion; the slots stored by
  # fitMod() (x_train, terms, xlev, y_levels, classification, ...) remain
  # accessible on obj
  obj <- object
  class(obj) <- class(obj)[class(obj) != "FitMod"]

  # Unwrap the raw model object from the list wrappers (lme4 S4 / xgboost)
  fit <- if (inherits(obj, "FitMod.lme4") || inherits(obj, "FitMod.xgboost"))
    obj$model
  else
    obj

  # --- rpart only: leaf/where output ---
  if (output %in% c("where", "leaf")) {
    if (fitfn != "rpart")
      stop("output = '", output, "' is only available for fitfn = 'rpart'.")
    if (is.null(newdata))
      return(if (output == "where") fit$where
             else rownames(fit$frame)[fit$where])
    return(.predict.leaves(fit, newdata = newdata, type = output))
  }

  # --- cox: risk scores by default, type overrideable ---
  if (fitfn == "coxph") {
    args <- list(fit, type = if (!is.null(type)) type else "risk")
    if (!is.null(newdata)) args$newdata <- newdata
    return(do.call(predict, c(args, list(...))))
  }

  # --- parametric survival (survreg-based, incl. tobit) ---
  # NOTE: survreg objects have no $fitted.values, so a fitted() shortcut
  # would silently return NULL - always go through predict()
  if (fitfn %in% c("weibull", "exponential", "lognormal",
                   "loglogistic", "tobit")) {
    args <- list(fit, type = if (!is.null(type)) type else "response")
    if (!is.null(newdata)) args$newdata <- newdata
    return(do.call(predict, c(args, list(...))))
  }

  # --- logit special case: type = "link" returns the linear predictor ---
  if (fitfn == "logit" && identical(type, "link")) {
    args <- list(fit, type = "link")
    if (!is.null(newdata)) args$newdata <- newdata
    return(do.call(predict, c(args, list(...))))
  }

  # --- regression vs classification: decided per fitted object ---
  if (!.is_classification(obj, fit, fitfn))
    return(.predict_response(fit, obj, fitfn, newdata,
                             s = s, type = type, ...))

  # --- classification ---
  if (!is.null(type))
    warning("'type' is ignored for classification models in predict.FitMod; ",
            "use 'output' to control the return format.", call. = FALSE)

  .pred_prob  <- function() .predict_prob(fit, obj, fitfn, newdata, s = s, ...)
  .pred_class <- function() .predict_class(fit, obj, fitfn, newdata, s = s, ...)

  switch(output,
         prob  = .pred_prob(),
         class = data.frame(class = .pred_class(), check.names = FALSE),
         both  = data.frame(.pred_prob(), class = .pred_class(),
                            check.names = FALSE)
  )
}


# -------------------------------------------------------------------------
# Internal: classification or regression?
# -------------------------------------------------------------------------

# Decided from the fitted object, not from fitfn alone: several methods
# (rpart, randomForest, svm, nnet, glmnet, xgboost) support both tasks.
#' @keywords internal
.is_classification <- function(obj, fit, fitfn) {

  # explicit flag stored by fitMod() for glmnet / xgboost
  if (!is.null(obj$classification))
    return(isTRUE(obj$classification))

  switch(fitfn,

         logit = , multinom = , polr = , lda = , qda = ,
         C5.0 = , naiveBayes = , logitMixed = TRUE,

         lm = , lmrob = , poisson = , quasipoisson = , gamma = ,
         negbin = , zeroinfl = ,
         lmMixed = , poissonMixed = , negbinMixed = , gammaMixed = FALSE,

         rpart        = identical(fit$method, "class"),
         randomForest = identical(fit$type, "classification"),
         svm          = !is.null(fit$levels),
         nnet         = !is.null(fit$lev),

         # legacy objects fitted before the classification flag existed:
         # glmnet is reliably detectable via classnames; for xgboost the
         # old behaviour (classification) is retained - refit with the
         # current fitMod() for correct regression handling
         glmnet  = !is.null(fit$glmnet.fit$classnames),
         xgboost = TRUE,

         stop("Cannot determine task type for fitfn = '", fitfn, "'.")
  )
}


# -------------------------------------------------------------------------
# Internal: regression predictions, uniformly on the response scale
# -------------------------------------------------------------------------

#' @keywords internal
.predict_response <- function(fit, obj, fitfn, newdata, s, type, ...) {

  # glmnet / xgboost: matrix interface
  if (fitfn == "glmnet") {
    nd <- .design_newdata(obj, newdata)
    p  <- predict(fit, newx = nd, s = s, type = "response")
    return(stats::setNames(as.numeric(p), rownames(nd)))
  }
  if (fitfn == "xgboost") {
    nd <- .design_newdata(obj, newdata)
    return(stats::setNames(as.numeric(predict(fit, nd)), rownames(nd)))
  }

  # ML regressions without a 'type = "response"' concept: their default
  # predict already returns the predictions
  if (fitfn %in% c("rpart", "nnet")) {
    args <- list(fit)
    if (!is.null(newdata)) args$newdata <- newdata
    return(as.numeric(do.call(predict, c(args, list(...)))))
  }
  if (fitfn %in% c("randomForest", "svm")) {
    # explicit newdata required; for randomForest this yields in-sample
    # (not OOB) predictions - consistent fitted-values semantics
    args <- list(fit, newdata = .resolve_newdata(fit, newdata))
    return(as.numeric(do.call(predict, c(args, list(...)))))
  }

  # GLM family & friends (lm, glm, glm.nb, lmrob, zeroinfl, merMod):
  # uniform response scale, with and without newdata
  args <- list(fit, type = if (is.null(type)) "response" else type)
  if (!is.null(newdata)) args$newdata <- newdata
  do.call(predict, c(args, list(...)))
}


# -------------------------------------------------------------------------
# Internal: extract probability matrix, always as data.frame
# -------------------------------------------------------------------------

#' @keywords internal
.predict_prob <- function(fit, obj, fitfn, newdata, s, ...) {

  args <- if (is.null(newdata)) list(fit)
          else                  list(fit, newdata = newdata)

  # Lazy evaluation - only computed for models that need it
  args_explicit <- function()
    list(fit, newdata = .resolve_newdata(fit, newdata))

  lvl <- .response_levels(fit)

  # Two-column probability matrix from P(second level)
  .binary_mat <- function(p, lv) {
    p <- as.numeric(p)
    m <- cbind(1 - p, p)
    colnames(m) <- lv
    m
  }

  mat <- switch(fitfn,

                logit = {
                  p <- do.call(predict, c(args, list(type = "response"),
                                          list(...)))
                  .binary_mat(p, if (is.null(lvl)) c("0", "1") else lvl)
                },

                multinom = {
                  p <- do.call(predict, c(args, list(type = "probs"),
                                          list(...)))
                  if (is.null(dim(p))) {
                    # binary multinom returns a vector of P(2nd level)
                    lv <- if (!is.null(fit$lev)) fit$lev
                          else if (!is.null(lvl)) lvl
                          else c("0", "1")
                    .binary_mat(p, lv)
                  } else p
                },

                polr = {
                  do.call(predict, c(args, list(type = "probs"), list(...)))
                },

                rpart = {
                  do.call(predict, c(args, list(type = "prob"), list(...)))
                },

                lda = ,
                qda = {
                  do.call(predict, c(args, list(...)))$posterior
                },

                svm = {
                  p <- do.call(predict, c(args_explicit(),
                                          list(probability = TRUE),
                                          list(...)))
                  attr(p, "probabilities")
                },

                nnet = {
                  p <- do.call(predict, c(args, list(type = "raw"),
                                          list(...)))
                  if (is.null(dim(p)) || ncol(p) == 1L) {
                    lv <- if (!is.null(fit$lev)) fit$lev else c("0", "1")
                    .binary_mat(p, lv)
                  } else p
                },

                naiveBayes = {
                  do.call(predict, c(args, list(type = "prob"), list(...)))
                },

                C5.0 = {
                  do.call(predict, c(args_explicit(), list(type = "prob"),
                                     list(...)))
                },

                randomForest = {
                  do.call(predict, c(args_explicit(), list(type = "prob"),
                                     list(...)))
                },

                glmnet = {
                  nd <- .design_newdata(obj, newdata)
                  p  <- predict(fit, newx = nd, s = s, type = "response")
                  if (length(dim(p)) == 3L)
                    p <- p[, , 1L]
                  if (is.null(dim(p)) || ncol(p) == 1L) {
                    # binomial: single column of P(2nd class)
                    lv <- fit$glmnet.fit$classnames
                    .binary_mat(p, if (is.null(lv)) c("0", "1") else lv)
                  } else p
                },

                xgboost = {
                  nd <- .design_newdata(obj, newdata)
                  p  <- predict(fit, nd)
                  yl <- obj$y_levels
                  if (is.matrix(p)) {
                    colnames(p) <- yl
                    p
                  } else if (!is.null(yl)) {
                    p <- matrix(p, ncol = length(yl), byrow = TRUE)
                    colnames(p) <- yl
                    p
                  } else {
                    .binary_mat(p, c("0", "1"))
                  }
                },

                logitMixed = {
                  p <- do.call(predict, c(args, list(type = "response"),
                                          list(...)))
                  .binary_mat(p, if (is.null(lvl)) c("0", "1") else lvl)
                },

                stop(sprintf("No probability prediction implemented for fitfn = '%s'",
                             fitfn))
  )

  # Keep original level names as column names (documented contract);
  # no sanitizing - downstream code uses check.names = FALSE
  nms <- colnames(mat)
  mat <- as.data.frame(mat)
  if (!is.null(nms))
    names(mat) <- nms
  .normalise_prob_cols(mat, fit, obj)
}


# -------------------------------------------------------------------------
# Internal: extract predicted class, always as factor
# -------------------------------------------------------------------------

#' @keywords internal
.predict_class <- function(fit, obj, fitfn, newdata, s, ...) {

  args <- if (is.null(newdata)) list(fit)
          else                  list(fit, newdata = newdata)

  args_explicit <- function()
    list(fit, newdata = .resolve_newdata(fit, newdata))

  lvl <- .response_levels(fit)

  .binary_class <- function(p, lv)
    factor(ifelse(as.numeric(p) > 0.5, lv[2L], lv[1L]), levels = lv)

  cls <- switch(fitfn,

                logit = {
                  p <- do.call(predict, c(args, list(type = "response"),
                                          list(...)))
                  .binary_class(p, if (is.null(lvl)) c("0", "1") else lvl)
                },

                multinom = ,
                polr     = {
                  do.call(predict, c(args, list(type = "class"), list(...)))
                },

                rpart = {
                  do.call(predict, c(args, list(type = "class"), list(...)))
                },

                lda = ,
                qda = {
                  do.call(predict, c(args, list(...)))$class
                },

                svm = {
                  do.call(predict, c(args_explicit(), list(...)))
                },

                C5.0 = ,
                randomForest = {
                  do.call(predict, c(args_explicit(), list(type = "class"),
                                     list(...)))
                },

                nnet = {
                  p <- do.call(predict, c(args, list(type = "raw"),
                                          list(...)))
                  if (is.null(dim(p)) || ncol(p) == 1L) {
                    lv <- if (!is.null(fit$lev)) fit$lev else c("0", "1")
                    .binary_class(p, lv)
                  } else {
                    # keep the level order of the model, not alphabetical
                    factor(colnames(p)[max.col(p)], levels = colnames(p))
                  }
                },

                naiveBayes = {
                  do.call(predict, c(args, list(type = "class"), list(...)))
                },

                glmnet = {
                  nd <- .design_newdata(obj, newdata)
                  p  <- predict(fit, newx = nd, s = s, type = "class")
                  cl <- as.character(p[, 1L])
                  lv <- fit$glmnet.fit$classnames
                  if (is.null(lv)) factor(cl) else factor(cl, levels = lv)
                },

                xgboost = {
                  nd <- .design_newdata(obj, newdata)
                  p  <- predict(fit, nd)
                  yl <- obj$y_levels
                  if (is.matrix(p)) {
                    factor(yl[max.col(p)], levels = yl)
                  } else if (!is.null(yl)) {
                    p <- matrix(p, ncol = length(yl), byrow = TRUE)
                    factor(yl[max.col(p)], levels = yl)
                  } else {
                    factor(ifelse(p > 0.5, "1", "0"), levels = c("0", "1"))
                  }
                },

                logitMixed = {
                  p <- do.call(predict, c(args, list(type = "response"),
                                          list(...)))
                  .binary_class(p, if (is.null(lvl)) c("0", "1") else lvl)
                },

                stop(sprintf("No class prediction implemented for fitfn = '%s'",
                             fitfn))
  )

  if (is.factor(cls)) cls else as.factor(cls)
}


# -------------------------------------------------------------------------
# Internal: design matrix for glmnet / xgboost predictions
# -------------------------------------------------------------------------

# Rebuilds the design matrix for newdata from the terms and factor levels
# (xlev) of the training data, so that newdata containing only a subset of
# the training factor levels still yields correctly aligned columns.
# The intercept column is only dropped if the model actually has one.
#' @keywords internal
.design_newdata <- function(obj, newdata) {

  if (is.null(newdata)) {
    if (is.null(obj$x_train))
      stop("Cannot recover training design matrix: ",
           "the model may not have been fitted via fitMod().")
    return(obj$x_train)
  }

  if (!is.null(obj$terms) && !is.null(obj$xlev)) {
    tt <- delete.response(obj$terms)
    mf <- model.frame(tt, data = newdata, xlev = obj$xlev)
    x  <- model.matrix(tt, mf)
    if (attr(tt, "intercept") == 1L)
      x <- x[, -1L, drop = FALSE]
    return(x)
  }

  # legacy objects fitted before terms/xlev were stored: old behaviour
  # (no level alignment) - refit with the current fitMod()
  model.matrix(obj$formula[-2L], data = newdata)[, -1L, drop = FALSE]
}


# -------------------------------------------------------------------------
# Internal: extract predicted leaf for an rpart
# -------------------------------------------------------------------------

# Works entirely in "where" space (row indices of the tree frame) and
# converts to node labels at the very end, so both output types share the
# identical reassignment logic for predictions landing on non-leaf nodes.
#' @keywords internal
.predict.leaves <- function(rp, newdata, type = c("where", "leaf")) {

  type <- match.arg(type)

  # yval trick: have predict() return the frame row index of the leaf
  rp$frame$yval <- seq_len(nrow(rp$frame))
  leaves        <- predict(rp, newdata = newdata, type = "vector")

  should.be.leaves <- which(rp$frame$var == "<leaf>")
  bad.leaves       <- leaves[!leaves %in% should.be.leaves]

  if (length(bad.leaves) > 0L) {

    u.bad.leaves <- unique(bad.leaves)
    u.bad.nodes  <- rownames(rp$frame)[u.bad.leaves]
    all.nodes    <- rownames(rp$frame)[should.be.leaves]

    # Find nearest leaf descendant for misdirected observations
    is.descendant <- function(all.leaves, node) {
      if (length(all.leaves) == 0L) return(logical(0L))
      all.leaves <- as.numeric(all.leaves)
      node       <- as.numeric(node)
      result     <- logical(length(all.leaves))
      for (i in seq_along(all.leaves)) {
        leaf <- all.leaves[i]
        while (leaf > node) {
          leaf <- trunc(leaf / 2L)
          if (leaf == node) { result[i] <- TRUE; break }
        }
      }
      result
    }

    where.tbl        <- table(rp$where)
    names(where.tbl) <- rownames(rp$frame)[as.integer(names(where.tbl))]

    for (u in seq_along(u.bad.nodes)) {
      desc.vec <- is.descendant(all.nodes, u.bad.nodes[u])
      me       <- where.tbl[all.nodes][desc.vec]
      winner   <- names(me)[me == max(me)][1L]
      leaves[leaves == u.bad.leaves[u]] <- which(rownames(rp$frame) == winner)
    }
  }

  if (type == "leaf") rownames(rp$frame)[leaves] else leaves
}


# -------------------------------------------------------------------------
# Internal: normalizing
# -------------------------------------------------------------------------

# Ensure newdata is set - some packages (e1071, C50, randomForest) require
# explicit newdata even for training data predictions
#' @keywords internal
.resolve_newdata <- function(object, newdata) {

  if (!is.null(newdata))
    return(newdata)

  # Try model.frame first (works for lm, glm, rpart, ...)
  nd <- tryCatch(
    model.frame(object),
    error = function(e) NULL
  )

  # Fallback for models without standard terms (C5.0, naive_bayes, ...)
  if (is.null(nd)) {
    if (is.null(object$call$data))
      stop("Cannot recover training data: no model.frame and no data= in call")
    env <- environment(formula(object))
    if (is.null(env))
      env <- parent.frame()
    nd <- eval(object$call$data, envir = env)
  }

  # Drop response column
  resp_name <- tryCatch(
    attr(attr(object$terms, "dataClasses"), "names")[1L],
    error = function(e) {
      # Fallback: first variable in formula
      all.vars(formula(object))[1L]
    }
  )

  nd[, setdiff(names(nd), resp_name), drop = FALSE]
}


# Response factor levels of the fitted model, NULL if not recoverable
#' @keywords internal
.response_levels <- function(fit) {
  tryCatch(
    levels(model.response(model.frame(fit))),
    error = function(e) NULL
  )
}


# Normalise column order to match factor levels of the response
#' @keywords internal
.normalise_prob_cols <- function(mat, fit, obj) {

  lvl <- tryCatch({
    if (!is.null(fit$glmnet.fit$classnames))
      fit$glmnet.fit$classnames
    else if (!is.null(obj$y_levels))
      obj$y_levels
    else
      .response_levels(fit)
  }, error = function(e) NULL)

  if (is.null(lvl) || !all(lvl %in% names(mat)))
    return(mat)
  mat[, lvl, drop = FALSE]
}
