
#' Fit a statistical or machine-learning model with automatic method selection
#'
#' A unified interface for fitting a wide range of regression and
#' classification models.  When \code{engine} is omitted the appropriate
#' method is chosen automatically from the type of the response variable.
#' The return value is always an object of class \code{"FitMod"} layered on
#' top of the original model object, so all standard methods
#' (\code{predict}, \code{print}, \code{coef}, \ldots) continue to work.
#'
#' @param formula A two-sided model formula.
#' @param data A data frame containing the variables in \code{formula}.
#' @param engine Character string naming the model to fit.  One of
#'   \code{"lm"}, \code{"logit"}, \code{"poisson"}, \code{"quasipoisson"},
#'   \code{"gamma"}, \code{"beta"}, \code{"negbin"}, \code{"polr"},
#'   \code{"lmrob"},
#'   \code{"tobit"}, \code{"zeroinfl"}, \code{"multinom"}, \code{"nnet"},
#'   \code{"rpart"}, \code{"C5.0"}, \code{"lda"}, \code{"qda"},
#'   \code{"svm"}, \code{"naiveBayes"}, \code{"randomForest"},
#'   \code{"glmnet"}, \code{"xgboost"}, \code{"coxph"},
#'   \code{"weibull"}, \code{"exponential"}, \code{"lognormal"},
#'   \code{"loglogistic"}, \code{"lmMixed"}, \code{"logitMixed"},
#'   \code{"poissonMixed"}, \code{"negbinMixed"}, \code{"gammaMixed"}.
#'   If \code{NULL} (default) the engine is chosen automatically, see
#'   Details.
#' @param subset An optional vector specifying a subset of observations.
#'   Only supported for fitting functions that accept a \code{subset}
#'   argument (and for \code{"glmnet"} and \code{"xgboost"}, where it is
#'   applied when the design matrix is built).
#' @param na.action A function for handling missing values, passed to the
#'   underlying fitting function (or to \code{\link[stats]{model.frame}}
#'   for \code{"glmnet"} and \code{"xgboost"}).  If not supplied, the
#'   default of the respective fitting function applies (usually
#'   \code{\link[stats]{na.omit}}).
#' @param ... Additional arguments passed to the underlying fitting function.
#'
#' @details
#' Automatic method selection uses the following heuristic: a dichotomous
#' response (exactly two distinct values, factor/logical or numeric coded
#' as 0/1) is fitted with \code{"logit"}, an ordered factor with
#' \code{"polr"}, an unordered factor with \code{"multinom"}, a
#' non-negative integer response with \code{"poisson"}, and any other
#' numeric response with \code{"lm"}.  Note that integer storage does not
#' necessarily mean count data -- data import functions often return
#' integer columns for metric variables.  The chosen method is always
#' reported via \code{message()}; supply \code{engine} explicitly to
#' override the heuristic.
#'
#' Beta regression (\code{engine = "beta"}, fitted with
#' \code{\link[betareg]{betareg}}) models a response in the open interval
#' (0, 1), typically a rate or proportion.  It is never selected
#' automatically, since a numeric response in (0, 1) is no evidence
#' against a linear model.  A two-part formula \code{y ~ x | z} adds
#' regressors \code{z} for the precision parameter.  If the response
#' contains the boundary values 0 or 1, \pkg{betareg} (>= 3.2-0) switches
#' to the extended-support beta mixture (\code{dist = "xbetax"}), which
#' additionally requires the packages \pkg{statmod} and \pkg{numDeriv}.
#' The result carries two extra components: \code{waldTable}, the
#' coefficient table of all model parts as a single matrix (rows named as
#' in \code{coef()}, attribute \code{"component"} giving the model part),
#' and \code{drop1}, the likelihood-ratio tests for the terms of the mean
#' and the precision model (the latter prefixed with \code{"(phi)_"}).
#'
#' @return An object of class \code{c("FitMod", <original class>)}.
#'   For \code{xgboost} and \code{lme4} models, a list of class
#'   \code{c("FitMod", "FitMod.xgboost")} or
#'   \code{c("FitMod", "FitMod.lme4")} wrapping the original model
#'   object in \code{$model}.  For \code{"glmnet"} and \code{"xgboost"}
#'   the result additionally stores \code{terms}, \code{xlev} and
#'   \code{x_train}, so that \code{predict()} can rebuild design matrices
#'   for new data with the factor levels of the training data.
#'
#' @examples
#' # Auto-detection: numeric response -> lm
#' fitMod(Sepal.Length ~ ., data = iris)
#'
#' # factor response -> multinom
#' if (requireNamespace("nnet", quietly = TRUE)) {
#'   fitMod(Species ~ ., data = iris)
#' }
#'
#' # Explicit method
#' if (requireNamespace("rpart", quietly = TRUE)) {
#'   fitMod(Species ~ ., data = iris, engine = "rpart")
#' }
#'
#' # Beta regression for a proportion, with a precision model after "|"
#' if (requireNamespace("betareg", quietly = TRUE)) {
#'   data("GasolineYield", package = "betareg")
#'   fitMod(yield ~ gravity + temp | temp, data = GasolineYield,
#'          engine = "beta")
#' }
#'
#' # Mixed models
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   fitMod(Reaction ~ Days + (1 | Subject), lme4::sleepstudy,
#'          engine = "lmMixed")
#' }
#'
#' @family modelling
#' @concept regression
#' @concept classification
#' @export
fitMod <- function(formula, data, engine = NULL, subset, na.action, ...) {
  
  # --- validate inputs ---
  if (!inherits(formula, "formula"))
    stop("'formula' must be a formula object.")
  if (length(formula) != 3L)
    stop("'formula' must be two-sided (response ~ predictors).")
  if (!is.data.frame(data))
    stop("'data' must be a data frame.")
  
  # --- build call ---
  cl <- match.call()
  
  # --- auto-detect fitting function if needed ---
  if (is.null(engine)) {
    resp  <- eval(formula[[2L]], envir = data, enclos = parent.frame())
    engine <- .guess_engine(resp)
    message("fitMod: using engine = '", engine, "'")
  } else {
    engine <- match.arg(engine, names(.engine_registry))
  }
  
  # --- look up registry entry, ensure package is available ---
  entry <- .engine_registry[[engine]]
  .require_pkg(entry$pkg)
  
  # --- glmnet / xgboost: no formula interface, convert to x/y ---
  # subset and na.action are honoured via model.frame() and removed from
  # the call afterwards (the target functions do not accept them)
  design <- NULL
  
  if (engine == "glmnet") {
    design <- .build_design(cl, parent.frame())
    
    if (!("family" %in% names(cl)))
      cl[["family"]] <- .guess_glmnet_family(design$y)
    
    cl[["x"]] <- design$x
    cl[["y"]] <- design$y
    # NOTE: cl is a call, not a list - assigning NULL to an *absent*
    # component via [[<- throws "subscript out of bounds", and subset/
    # na.action are absent unless explicitly supplied (match.call()!)
    for (nm in c("formula", "data", "subset", "na.action"))
      if (nm %in% names(cl)) cl[[nm]] <- NULL
  }
  
  if (engine == "xgboost") {
    design <- .build_design(cl, parent.frame())
    
    if (!("objective" %in% names(cl)))
      cl[["objective"]] <- .guess_xgb_objective(design$y)
    
    cl[["x"]] <- design$x
    cl[["y"]] <- design$y
    for (nm in c("formula", "data", "subset", "na.action"))
      if (nm %in% names(cl)) cl[[nm]] <- NULL
  }
  
  # --- apply registry defaults and strip fitMod-specific args ---
  cl       <- .apply_defaults(cl, entry$defaults)
  cl$engine <- NULL
  
  # Namespaced call head (pkg::fn): the fitting function is found even if
  # its package is not attached, the call stored by the fitter via
  # match.call() is valid as-is, and update()/drop1() on the result work
  # in any environment
  cl[[1L]] <- call("::", as.name(entry$pkg), as.name(entry$fn))
  
  # --- fit model ---
  res <- eval(cl, parent.frame())
  
  # --- xgboost: wrap in list since xgboost objects don't support $<- ---
  if (engine == "xgboost") {
    res <- list(
      model          = res,
      engine          = engine,
      formula        = formula,
      terms          = design$terms,
      xlev           = design$xlev,
      x_train        = design$x,
      y_levels       = if (is.factor(design$y)) levels(design$y) else NULL,
      classification = is.character(cl[["objective"]]) &&
        grepl("^(binary|multi):", cl[["objective"]]),
      call           = match.call()
    )
    class(res) <- c("FitMod", "FitMod.xgboost")
    return(res)
  }
  
  # --- lme4: wrap in list since S4 objects don't support $<- ---
  if (engine %in% c("lmMixed", "logitMixed", "poissonMixed",
                   "negbinMixed", "gammaMixed")) {
    res <- list(
      model = res,
      engine = engine,
      call  = match.call()
    )
    class(res) <- c("FitMod", "FitMod.lme4")
    return(res)
  }
  
  # --- post-process on the natural class, before FitMod is prepended ---
  res <- .postprocess(res, engine)
  
  # --- attach FitMod class and metadata (all other models) ---
  class(res) <- c("FitMod", class(res))
  res$engine  <- engine
  
  # --- store glmnet-specific data for predict ---
  # cv.glmnet's own stored call embeds the full x matrix; replace it with
  # the compact fitMod call (update() then refits via fitMod, by design)
  if (engine == "glmnet") {
    res[["formula"]]        <- formula
    res[["terms"]]          <- design$terms
    res[["xlev"]]           <- design$xlev
    res[["x_train"]]        <- design$x
    res[["classification"]] <- identical(cl[["family"]], "binomial") ||
      identical(cl[["family"]], "multinomial")
    res[["call"]]           <- match.call()
  }
  
  res
}



# == internal helper functions ============================================


# Internal registry: one entry per supported fitting function
# NOTE: the former fix_call field is obsolete -- the namespaced call head
# (pkg::fn) makes the stored calls valid without repair.

.engine_registry <- list(
  
  lm = list(
    pkg      = "stats",
    fn       = "lm",
    defaults = list()
  ),
  
  logit = list(
    pkg      = "stats",
    fn       = "glm",
    defaults = list(family = "binomial")
  ),
  
  poisson = list(
    pkg      = "stats",
    fn       = "glm",
    defaults = list(family = "poisson")
  ),
  
  quasipoisson = list(
    pkg      = "stats",
    fn       = "glm",
    defaults = list(family = "quasipoisson")
  ),
  
  gamma = list(
    pkg      = "stats",
    fn       = "glm",
    defaults = list(family = quote(Gamma(link = "log")))
  ),
  
  # response in (0, 1); "y ~ x | z" adds a precision model.  No defaults
  # needed: betareg() already keeps the model frame and the response
  beta = list(
    pkg      = "betareg",
    fn       = "betareg",
    defaults = list()
  ),

  negbin = list(
    pkg      = "MASS",
    fn       = "glm.nb",
    defaults = list()
  ),

  polr = list(
    pkg      = "MASS",
    fn       = "polr",
    defaults = list(Hess = TRUE, model = TRUE)
  ),
  
  lmrob = list(
    pkg      = "robustbase",
    fn       = "lmrob",
    defaults = list()
  ),
  
  tobit = list(
    pkg      = "AER",
    fn       = "tobit",
    defaults = list()
  ),
  
  zeroinfl = list(
    pkg      = "pscl",
    fn       = "zeroinfl",
    defaults = list()
  ),
  
  multinom = list(
    pkg      = "nnet",
    fn       = "multinom",
    defaults = list(maxit = 500, model = TRUE, trace = FALSE)
  ),
  
  nnet = list(
    pkg      = "nnet",
    fn       = "nnet",
    defaults = list(
      maxit   = 1000,
      trace   = FALSE,
      size    = 10,
      entropy = TRUE,    # cross-entropy loss for classification
      decay   = 0.01     # L2 regularization, helps convergence
    )
  ),
  
  rpart = list(
    pkg      = "rpart",
    fn       = "rpart",
    defaults = list(model = TRUE, y = TRUE)
  ),
  
  randomForest = list(
    pkg      = "randomForest",
    fn       = "randomForest",
    defaults = list()
  ),
  
  C5.0 = list(
    pkg      = "C50",
    fn       = "C5.0",
    defaults = list()
  ),
  
  lda = list(
    pkg      = "MASS",
    fn       = "lda",
    defaults = list()
  ),
  
  qda = list(
    pkg      = "MASS",
    fn       = "qda",
    defaults = list()
  ),
  
  svm = list(
    pkg      = "e1071",
    fn       = "svm",
    defaults = list(probability = TRUE)
  ),
  
  naiveBayes = list(
    pkg      = "naivebayes",
    fn       = "naive_bayes",
    defaults = list()
  ),
  
  glmnet = list(
    pkg      = "glmnet",
    fn       = "cv.glmnet",
    defaults = list(
      alpha  = 1,      # Lasso; user can override to 0 (Ridge) or 0.5 (Elastic Net)
      nfolds = 10
      # family is auto-detected in fitMod() from the response type
    )
  ),
  
  xgboost = list(
    pkg      = "xgboost",
    fn       = "xgboost",
    defaults = list(
      nrounds       = 100L,
      max_depth     = 3L,
      learning_rate = 0.1
    )
  ),
  
  coxph = list(
    pkg      = "survival",
    fn       = "coxph",
    defaults = list(model = TRUE, x = TRUE)
  ),
  
  weibull = list(
    pkg      = "survival",
    fn       = "survreg",
    defaults = list(dist = "weibull")
  ),
  
  exponential = list(
    pkg      = "survival",
    fn       = "survreg",
    defaults = list(dist = "exponential")
  ),
  
  lognormal = list(
    pkg      = "survival",
    fn       = "survreg",
    defaults = list(dist = "lognormal")
  ),
  
  loglogistic = list(
    pkg      = "survival",
    fn       = "survreg",
    defaults = list(dist = "loglogistic")
  ),
  
  lmMixed = list(
    pkg      = "lme4",
    fn       = "lmer",
    defaults = list()
  ),
  
  logitMixed = list(
    pkg      = "lme4",
    fn       = "glmer",
    defaults = list(family = "binomial")
  ),
  
  poissonMixed = list(
    pkg      = "lme4",
    fn       = "glmer",
    defaults = list(family = "poisson")
  ),
  
  negbinMixed = list(
    pkg      = "lme4",
    fn       = "glmer.nb",
    defaults = list()
  ),
  
  gammaMixed = list(
    pkg      = "lme4",
    fn       = "glmer",
    defaults = list(family = quote(Gamma(link = "log")))
  )
  
)


# -------------------------------------------------------------------------
# Auto-detect fitting function from response type
# -------------------------------------------------------------------------

#' @keywords internal
.guess_engine <- function(resp) {
  
  if (all(is.na(resp)))
    stop("Response contains only missing values.")
  
  # dichotomous: exactly two distinct values AND a type where a
  # binomial fit is meaningful (factor/logical/character or 0/1 coded)
  if (isTRUE(isDichotomous(resp, strict = TRUE, na.rm = TRUE)) &&
      (is.factor(resp) || is.logical(resp) || is.character(resp) ||
       all(resp %in% c(0, 1) | is.na(resp))))
    return("logit")
  
  if (inherits(resp, "ordered"))
    return("polr")
  if (is.factor(resp))
    return("multinom")
  if (is.integer(resp))
    return(if (any(resp < 0, na.rm = TRUE)) "lm" else "poisson")
  if (is.numeric(resp))
    return("lm")
  
  stop(
    "Cannot guess fitting function for response of class '",
    paste(class(resp), collapse = "/"), "'. ",
    "Please provide 'engine' explicitly."
  )
}


#' @keywords internal
.guess_glmnet_family <- function(resp) {
  if (isTRUE(isDichotomous(resp, strict = TRUE, na.rm = TRUE)))
    "binomial"
  else if (is.factor(resp))
    "multinomial"
  else if (is.integer(resp) && !any(resp < 0, na.rm = TRUE))
    "poisson"
  else
    "gaussian"
}


#' @keywords internal
.guess_xgb_objective <- function(resp) {
  if (isTRUE(isDichotomous(resp, strict = TRUE, na.rm = TRUE)))
    "binary:logistic"
  else if (is.factor(resp))
    "multi:softprob"
  else if (is.integer(resp) && !any(resp < 0, na.rm = TRUE))
    "count:poisson"
  else
    "reg:squarederror"
}


# -------------------------------------------------------------------------
# Build design matrix x and response y from the matched call
# (used for target functions without a formula interface)
# -------------------------------------------------------------------------

# Standard model.frame idiom (cf. lm): subset and na.action are kept as
# unevaluated expressions and correctly resolved within 'data'.
# The intercept column is only dropped if the formula actually has one.
#' @keywords internal
.build_design <- function(cl, env) {
  
  mfCall <- cl
  keep   <- match(c("formula", "data", "subset", "na.action"),
                  names(mfCall), 0L)
  mfCall <- mfCall[c(1L, keep)]
  mfCall[[1L]] <- quote(stats::model.frame)
  mfCall$drop.unused.levels <- TRUE
  mf <- eval(mfCall, env)
  
  tt <- attr(mf, "terms")
  x  <- model.matrix(tt, mf)
  if (attr(tt, "intercept") == 1L)
    x <- x[, -1L, drop = FALSE]
  
  list(
    x     = x,
    y     = model.response(mf),
    terms = tt,
    xlev  = stats::.getXlevels(tt, mf)
  )
}


# -------------------------------------------------------------------------
# Ensure optional package is available
# -------------------------------------------------------------------------

#' @keywords internal
.require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop("Package '", pkg, "' must be installed for this fitting function.")
}


# -------------------------------------------------------------------------
# Apply registry defaults to call (only if not supplied by the user)
# -------------------------------------------------------------------------

# Membership check instead of is.null(): an explicitly supplied NULL is
# a deliberate user choice and must not be overwritten by a default
# (same principle as modifyListSafe())
#' @keywords internal
.apply_defaults <- function(cl, defaults) {
  for (nm in names(defaults))
    if (!(nm %in% names(cl)))
      cl[[nm]] <- defaults[[nm]]
  cl
}


# -------------------------------------------------------------------------
# Post-processing: steps that extend the result object after fitting
# (called on the natural class, before "FitMod" is prepended)
# -------------------------------------------------------------------------

#' @keywords internal
.postprocess <- function(res, engine) {
  UseMethod(".postprocess")
}

#' @keywords internal
.postprocess.multinom <- function(res, engine) {
  # Wald z-test p-values (2-tailed); lower.tail avoids underflow to
  # exactly 0 for large |z|
  sm <- suppressMessages(summary(res))
  z  <- sm$coefficients / sm$standard.errors
  res[["pval"]]  <- 2 * pnorm(abs(z), lower.tail = FALSE)
  res[["drop1"]] <- .drop1.multinom(res)
  res
}

#' @keywords internal
.postprocess.polr <- function(res, engine) {
  res[["drop1"]] <- .drop1.polr(res)
  res
}

#' @keywords internal
.postprocess.rpart <- function(res, engine) {
  # Record variables actually used in tree splits
  frame  <- res$frame
  leaves <- frame$var == "<leaf>"
  res[["used"]] <- sort(as.character(unique(frame$var[!leaves])))
  res
}

#' @keywords internal
.postprocess.nnet <- function(res, engine) {
  if (identical(res$convergence, 1L))
    warning(
      "nnet() did not converge; consider increasing 'maxit' ",
      "(set trace = TRUE to monitor progress)."
    )
  res
}

#' @keywords internal
.postprocess.betareg <- function(res, engine) {
  res[["waldTable"]] <- .waldTable.betareg(res)
  # the fit itself is valid even if a refit of a reduced model fails:
  # report this instead of aborting fitMod()
  res[["drop1"]] <- tryCatch(
    .drop1.betareg(res),
    error = function(e) {
      warning("fitMod: no term-wise likelihood-ratio tests for this ",
              "beta regression (", conditionMessage(e), ").", call. = FALSE)
      NULL
    }
  )
  res
}

#' @keywords internal
.postprocess.default <- function(res, engine) res



# Likelihood-ratio tests for the terms of a multinomial model.
#
# The reduced models are refitted on the response and the design matrix of
# the full fit, with the columns of the term removed (as .drop1.polr and
# .drop1.betareg do).  They use exactly the observations of the full fit,
# and transformed terms such as log(x) or poly(x, 2) need no special
# treatment: a refit through the formula on model.frame(object) fails for
# them, the frame holds a column "log(x)" but no variable x.
# All other arguments of the original call (maxit, decay, ...) are kept.
#' @keywords internal
.drop1.multinom <- function(object, scope, test = c("Chisq", "none"), ...) {
  
  if (!inherits(object, "multinom"))
    stop("'object' must be of class 'multinom'.")
  
  test <- match.arg(test)
  labs <- attr(object$terms, "term.labels")
  
  if (missing(scope))
    scope <- drop.scope(object)
  else {
    if (!is.character(scope))
      scope <- attr(terms(update.formula(object, scope)), "term.labels")
    if (!all(scope %in% labs))
      stop("'scope' is not a subset of the term labels.")
  }
  
  mf     <- model.frame(object)
  X      <- model.matrix(object)
  asgn   <- attr(X, "assign")
  hasInt <- attr(object$terms, "intercept") == 1L
  wts    <- model.weights(mf)
  off    <- model.offset(mf)
  
  # Data of the refits: the actual objects, not symbols that would have
  # to be looked up again (cf. the weights issue in .drop1.polr)
  refitData <- data.frame(.row = seq_len(nrow(mf)))
  refitData[[".y"]]   <- model.response(mf)
  refitData[[".off"]] <- off
  
  # The remaining arguments of the call are evaluated in an isolated
  # environment on top of the formula environment, which also provides
  # the fitting function for calls with an unqualified head
  env <- environment(formula(object))
  if (is.null(env))
    env <- parent.frame()
  
  evalEnv <- new.env(parent = env)
  assign("multinom", getFromNamespace("multinom", "nnet"), envir = evalEnv)
  
  refit <- function(drop) {
    # the intercept column is supplied by the formula
    keep <- asgn != 0L & !drop
    refitData[[".X"]] <- X[, keep, drop = FALSE]
    
    rhs <- c(if (any(keep)) ".X", if (!is.null(off)) "offset(.off)")
    if (length(rhs) == 0L)
      rhs <- "1"
    
    call <- update(object, evaluate = FALSE)
    call$formula   <- stats::reformulate(rhs, response = ".y",
                                         intercept = hasInt)
    call$data      <- refitData
    call$weights   <- wts
    call$subset    <- NULL
    call$na.action <- NULL
    call$contrasts <- NULL
    call$trace     <- FALSE
    
    eval(call, envir = evalEnv)
  }
  
  # Result matrix
  has_chisq <- test == "Chisq"
  ans <- matrix(
    NA_real_,
    nrow     = length(scope) + 1L,
    ncol     = if (has_chisq) 4L else 2L,
    dimnames = list(c("<none>", scope),
                    c("Df", "AIC", if (has_chisq) c("LR stat.", "p-value")))
  )
  ans[1L, "Df"]  <- object$edf
  ans[1L, "AIC"] <- object$AIC
  
  for (i in seq_along(scope)) {
    
    drop <- asgn == match(scope[i], labs)
    
    # a model without any parameter cannot be fitted
    if (!hasInt && !any(asgn != 0L & !drop)) next
    
    nfit <- refit(drop)
    
    ans[i + 1L, "Df"] <- nfit$edf
    
    if (isTRUE(nfit$edf == object$edf)) {
      # Singular term: model unchanged, leave AIC and test cols as NA
      # (mirrors behaviour of drop1.lm for singular terms)
      next
    }
    
    ans[i + 1L, "AIC"] <- nfit$AIC
    
    if (has_chisq) {
      lr <- nfit$deviance - object$deviance
      ans[i + 1L, c("LR stat.", "p-value")] <-
        c(lr, pchisq(lr, df = object$edf - nfit$edf, lower.tail = FALSE))
    }
  }
  
  as.data.frame(ans)
}


# -------------------------------------------------------------------------
# Beta regression: coefficient table and term-wise tests
# -------------------------------------------------------------------------

# coef(summary()) of a betareg object is a list with one matrix per model
# part, not a matrix.  The parts are stacked into a single matrix whose
# rows are named as in coef(); the attribute "component" keeps the part.
# The list names depend on the distribution: "mean"/"precision" for the
# classic beta, "mu"/"phi"/"nu" for the extended-support variants.
#' @keywords internal
.waldTable.betareg <- function(object) {
  
  tabs <- coef(summary(object))
  part <- unname(c(mean = "mean", mu = "mean", precision = "precision",
                   phi = "precision", nu = "nu")[names(tabs)])
  
  # a constant precision on the identity link (the default for a
  # one-part formula) is already named "(phi)"
  for (i in which(part == "precision")) {
    rn <- rownames(tabs[[i]])
    rownames(tabs[[i]]) <- ifelse(rn == "(phi)", rn, paste0("(phi)_", rn))
  }
  
  res <- do.call(rbind, unname(tabs))
  attr(res, "component") <- rep(part, vapply(tabs, nrow, integer(1L)))
  res
}


# Refit a beta regression on other design matrices for the mean (x) and
# the precision model (z), everything else as in the full fit: response,
# weights, offsets, links, estimator and distribution.  Used for the
# reduced models of .drop1.betareg and for the null model of pseudoRSq().
# Nothing is evaluated again in the formula environment, and exactly the
# observations of the full fit are used.  The result is the plain list of
# betareg.fit(), with components loglik and converged.
#' @keywords internal
.refit_betareg <- function(object, x, z) {
  
  n <- NROW(x)
  y <- object[["y"]]
  if (is.null(y))
    y <- model.response(model.frame(object))
  
  control       <- object[["control"]]
  control$start <- NULL      # start values of the full model do not fit
  
  # link and offset are accessed by position, their names differ between
  # the distributions (see .waldTable.betareg)
  offset <- lapply(
    list(mu = object[["offset"]][[1L]], phi = object[["offset"]][[2L]]),
    function(o) if (is.null(o)) rep.int(0, n) else o
  )
  
  suppressWarnings(betareg::betareg.fit(
    x = x, y = y, z = z,
    weights  = object[["weights"]],
    offset   = offset,
    link     = object[["link"]][[1L]],
    link.phi = object[["link"]][[2L]],
    type     = object[["type"]],
    control  = control,
    dist     = object[["dist"]],
    # nu is estimated for "xbetax" and only fixed for "xbeta"
    nu       = if (identical(object[["dist"]], "xbeta")) object[["nu"]]
  ))
}


# Likelihood-ratio tests for the terms of the mean and of the precision
# model (rows of the latter prefixed with "(phi)_", as in coef()).
# drop1() itself fails on betareg objects, there is no extractAIC method.
#
# The reduced models are refitted with .refit_betareg() on the design
# matrices of the full fit with the columns of the term removed, as
# drop1.glm does with glm.fit(), so subset, weights, missing values and
# transformed terms need no special treatment.
#
# Layout as in .drop1.multinom: Df is the number of parameters of the
# respective model, rows without a usable refit stay NA.
#' @keywords internal
.drop1.betareg <- function(object, test = c("Chisq", "none")) {
  
  test  <- match.arg(test)
  parts <- c(mean = "mean", precision = "precision")
  
  tl    <- lapply(parts, function(p)
    attr(terms(object, model = p), "term.labels"))
  mm    <- lapply(parts, function(p) model.matrix(object, model = p))
  scope <- lapply(parts, function(p) drop.scope(terms(object, model = p)))
  
  ll0  <- logLik(object)
  edf0 <- attr(ll0, "df")
  ll0  <- as.numeric(ll0)
  
  has_chisq <- test == "Chisq"
  labels    <- c(scope$mean, paste0("(phi)_", scope$precision,
                                    recycle0 = TRUE))
  ans <- matrix(
    NA_real_,
    nrow     = length(labels) + 1L,
    ncol     = if (has_chisq) 4L else 2L,
    dimnames = list(c("<none>", labels),
                    c("Df", "AIC", if (has_chisq) c("LR stat.", "p-value")))
  )
  ans[1L, c("Df", "AIC")] <- c(edf0, -2 * ll0 + 2 * edf0)
  
  failed <- character(0L)
  i      <- 1L
  
  for (p in parts) for (tt in scope[[p]]) {
    
    i    <- i + 1L
    drop <- attr(mm[[p]], "assign") == match(tt, tl[[p]])
    red  <- mm
    red[[p]] <- mm[[p]][, !drop, drop = FALSE]
    
    # a model part without any parameter cannot be fitted
    if (ncol(red[[p]]) == 0L) next
    
    edf <- edf0 - sum(drop)
    ans[i, "Df"] <- edf
    
    nfit <- .refit_betareg(object, red$mean, red$precision)
    if (!isTRUE(nfit$converged)) {
      failed <- c(failed, rownames(ans)[i])
      next
    }
    
    ans[i, "AIC"] <- -2 * nfit$loglik + 2 * edf
    
    if (has_chisq) {
      lr <- 2 * (ll0 - nfit$loglik)
      ans[i, c("LR stat.", "p-value")] <-
        c(lr, pchisq(lr, df = sum(drop), lower.tail = FALSE))
    }
  }
  
  if (length(failed))
    warning("fitMod: the reduced beta regression did not converge for ",
            paste(failed, collapse = ", "), ".", call. = FALSE)
  
  as.data.frame(ans)
}
