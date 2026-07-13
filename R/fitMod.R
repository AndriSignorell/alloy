
#' Fit a statistical or machine-learning model with automatic method selection
#'
#' A unified interface for fitting a wide range of regression and
#' classification models.  When \code{fitfn} is omitted the appropriate
#' method is chosen automatically from the type of the response variable.
#' The return value is always an object of class \code{"FitMod"} layered on
#' top of the original model object, so all standard methods
#' (\code{predict}, \code{print}, \code{coef}, \ldots) continue to work.
#'
#' @param formula A two-sided model formula.
#' @param data A data frame containing the variables in \code{formula}.
#' @param ... Additional arguments passed to the underlying fitting function.
#' @param subset An optional vector specifying a subset of observations.
#'   Only supported for fitting functions that accept a \code{subset}
#'   argument (and for \code{"glmnet"} and \code{"xgboost"}, where it is
#'   applied when the design matrix is built).
#' @param na.action A function for handling missing values, passed to the
#'   underlying fitting function (or to \code{\link[stats]{model.frame}}
#'   for \code{"glmnet"} and \code{"xgboost"}).  If not supplied, the
#'   default of the respective fitting function applies (usually
#'   \code{\link[stats]{na.omit}}).
#' @param fitfn Character string naming the fitting method.  One of
#'   \code{"lm"}, \code{"logit"}, \code{"poisson"}, \code{"quasipoisson"},
#'   \code{"gamma"}, \code{"negbin"}, \code{"polr"}, \code{"lmrob"},
#'   \code{"tobit"}, \code{"zeroinfl"}, \code{"multinom"}, \code{"nnet"},
#'   \code{"rpart"}, \code{"C5.0"}, \code{"lda"}, \code{"qda"},
#'   \code{"svm"}, \code{"naiveBayes"}, \code{"randomForest"},
#'   \code{"glmnet"}, \code{"xgboost"}, \code{"coxph"},
#'   \code{"weibull"}, \code{"exponential"}, \code{"lognormal"},
#'   \code{"loglogistic"}, \code{"lmMixed"}, \code{"logitMixed"},
#'   \code{"poissonMixed"}, \code{"negbinMixed"}, \code{"gammaMixed"}.
#'   If \code{NULL} (default) the method is chosen automatically.
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
#' reported via \code{message()}; supply \code{fitfn} explicitly to
#' override the heuristic.
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
#'   fitMod(Species ~ ., data = iris, fitfn = "rpart")
#' }
#'
#' # Mixed models
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   fitMod(Reaction ~ Days + (1 | Subject), lme4::sleepstudy,
#'          fitfn = "lmMixed")
#' }
#'
#' @family modelling
#' @concept regression
#' @concept classification
#' @export
fitMod <- function(formula, data, ..., subset, na.action, fitfn = NULL) {
  
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
  if (is.null(fitfn)) {
    resp  <- eval(formula[[2L]], envir = data, enclos = parent.frame())
    fitfn <- .guess_fitfn(resp)
    message("fitMod: using fitfn = '", fitfn, "'")
  } else {
    fitfn <- match.arg(fitfn, names(.fitfn_registry))
  }
  
  # --- look up registry entry, ensure package is available ---
  entry <- .fitfn_registry[[fitfn]]
  .require_pkg(entry$pkg)
  
  # --- glmnet / xgboost: no formula interface, convert to x/y ---
  # subset and na.action are honoured via model.frame() and removed from
  # the call afterwards (the target functions do not accept them)
  design <- NULL
  
  if (fitfn == "glmnet") {
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
  
  if (fitfn == "xgboost") {
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
  cl$fitfn <- NULL
  
  # Namespaced call head (pkg::fn): the fitting function is found even if
  # its package is not attached, the call stored by the fitter via
  # match.call() is valid as-is, and update()/drop1() on the result work
  # in any environment
  cl[[1L]] <- call("::", as.name(entry$pkg), as.name(entry$fn))
  
  # --- fit model ---
  res <- eval(cl, parent.frame())
  
  # --- xgboost: wrap in list since xgboost objects don't support $<- ---
  if (fitfn == "xgboost") {
    res <- list(
      model          = res,
      fitfn          = fitfn,
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
  if (fitfn %in% c("lmMixed", "logitMixed", "poissonMixed",
                   "negbinMixed", "gammaMixed")) {
    res <- list(
      model = res,
      fitfn = fitfn,
      call  = match.call()
    )
    class(res) <- c("FitMod", "FitMod.lme4")
    return(res)
  }
  
  # --- post-process on the natural class, before FitMod is prepended ---
  res <- .postprocess(res, fitfn)
  
  # --- attach FitMod class and metadata (all other models) ---
  class(res) <- c("FitMod", class(res))
  res$fitfn  <- fitfn
  
  # --- store glmnet-specific data for predict ---
  # cv.glmnet's own stored call embeds the full x matrix; replace it with
  # the compact fitMod call (update() then refits via fitMod, by design)
  if (fitfn == "glmnet") {
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

.fitfn_registry <- list(
  
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
.guess_fitfn <- function(resp) {
  
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
    "Please provide 'fitfn' explicitly."
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
.postprocess <- function(res, fitfn) {
  UseMethod(".postprocess")
}

#' @keywords internal
.postprocess.multinom <- function(res, fitfn) {
  # Wald z-test p-values (2-tailed); lower.tail avoids underflow to
  # exactly 0 for large |z|
  sm <- suppressMessages(summary(res))
  z  <- sm$coefficients / sm$standard.errors
  res[["pval"]]  <- 2 * pnorm(abs(z), lower.tail = FALSE)
  res[["drop1"]] <- .drop1.multinom(res)
  res
}

#' @keywords internal
.postprocess.polr <- function(res, fitfn) {
  res[["drop1"]] <- .drop1.polr(res)
  res[["ci"]]    <- confint.default(res)
  res
}

#' @keywords internal
.postprocess.rpart <- function(res, fitfn) {
  # Record variables actually used in tree splits
  frame  <- res$frame
  leaves <- frame$var == "<leaf>"
  res[["used"]] <- sort(as.character(unique(frame$var[!leaves])))
  res
}

#' @keywords internal
.postprocess.nnet <- function(res, fitfn) {
  if (identical(res$convergence, 1L))
    warning(
      "nnet() did not converge; consider increasing 'maxit' ",
      "(set trace = TRUE to monitor progress)."
    )
  res
}

#' @keywords internal
.postprocess.default <- function(res, fitfn) res



#' @keywords internal
.drop1.multinom <- function(object, scope, test = c("Chisq", "none"), ...) {
  
  if (!inherits(object, "multinom"))
    stop("'object' must be of class 'multinom'.")
  
  test <- match.arg(test)
  
  if (missing(scope))
    scope <- drop.scope(object)
  else {
    if (!is.character(scope))
      scope <- attr(terms(update.formula(object, scope)), "term.labels")
    if (!all(scope %in% attr(object$terms, "term.labels")))
      stop("'scope' is not a subset of the term labels.")
  }
  
  # Isolated evaluation environment on top of the formula environment:
  # provides the fitting function and the training data for the refits
  # without writing into the user's workspace and without consuming
  # RNG state for temp names (reproducibility after set.seed()).
  # model.frame(object) is already stripped to exactly the rows used
  # during fitting.
  env <- environment(formula(object))
  if (is.null(env))
    env <- parent.frame()
  
  evalEnv <- new.env(parent = env)
  assign("multinom", getFromNamespace("multinom", "nnet"), envir = evalEnv)
  assign(".drop1_data", model.frame(object), envir = evalEnv)
  
  ns <- length(scope)
  
  # Result matrix
  has_chisq <- test == "Chisq"
  col_names <- if (has_chisq) c("Df", "AIC", "LR stat.", "p-value")
  else           c("Df", "AIC")
  
  ans <- matrix(
    NA_real_,
    nrow     = ns + 1L,
    ncol     = length(col_names),
    dimnames = list(c("<none>", scope), col_names)
  )
  ans[1L, "Df"]  <- object$edf
  ans[1L, "AIC"] <- object$AIC
  
  # Extract LR stat and p-value column names from anova output robustly
  .anova_cols <- function(av) {
    nms <- names(av)
    list(
      stat = nms[length(nms) - 1L],
      pval = nms[length(nms)]
    )
  }
  
  for (i in seq_len(ns)) {
    tt   <- scope[i]
    call <- update(object, as.formula(paste("~ . -", tt)), evaluate = FALSE)
    call$data <- as.name(".drop1_data")
    
    nfit <- eval(call, envir = evalEnv)
    
    ans[i + 1L, "Df"] <- nfit$edf
    
    if (isTRUE(nfit$edf == object$edf)) {
      # Singular term: model unchanged, leave AIC and test cols as NA
      # (mirrors behaviour of drop1.lm for singular terms)
      next
    }
    
    ans[i + 1L, "AIC"] <- nfit$AIC
    
    if (has_chisq) {
      av   <- anova(object, nfit)
      cols <- .anova_cols(av)
      ans[i + 1L, "LR stat."] <- av[2L, cols$stat]
      ans[i + 1L, "p-value"]  <- av[2L, cols$pval]
    }
  }
  
  as.data.frame(ans)
}

