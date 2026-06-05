


#' Fit a statistical or machine-learning model with automatic method selection
#'
#' A unified interface for fitting a wide range of regression and
#' classification models.  When \code{fitfn} is omitted the appropriate
#' method is chosen automatically from the type of the response variable.
#' The return value is always an object of class \code{"FitMod"} layered on
#' top of the original model object, so all standard methods
#' (\code{predict}, \code{print}, \code{coef}, \ldots) continue to work.
#'
#' @param formula A model formula.
#' @param data A data frame containing the variables in \code{formula}.
#' @param ... Additional arguments passed to the underlying fitting function.
#' @param subset An optional vector specifying a subset of observations.
#' @param na.action A function for handling missing values.
#'   Default is \code{\link[stats]{na.pass}}.
#' @param fitfn Character string naming the fitting method.  One of
#'   \code{"lm"}, \code{"logit"}, \code{"poisson"}, \code{"quasipoisson"},
#'   \code{"gamma"}, \code{"negbin"}, \code{"polr"}, \code{"lmrob"},
#'   \code{"tobit"}, \code{"zeroinfl"}, \code{"multinom"}, \code{"nnet"},
#'   \code{"rpart"}, \code{"C5.0"}, \code{"lda"}, \code{"qda"},
#'   \code{"svm"}, \code{"naive_bayes"}, \code{"randomForest"},
#'   \code{"glmnet"}, \code{"xgboost"}, \code{"coxph"},
#'   \code{"weibull"}, \code{"exponential"}, \code{"lognormal"},
#'   \code{"loglogistic"}, \code{"lmMixed"}, \code{"logitMixed"},
#'   \code{"poissonMixed"}, \code{"negbinMixed"}, \code{"gammaMixed"}.
#'   If \code{NULL} (default) the method is chosen automatically.
#'
#' @return An object of class \code{c("FitMod", <original class>)}.
#'   For \code{xgboost} and \code{lme4} models, a list of class
#'   \code{c("FitMod", "FitMod.xgboost")} or
#'   \code{c("FitMod", "FitMod.lme4")} wrapping the original model
#'   object in \code{$model}.
#'
#' @examples
#' # Auto-detection
#' fitMod(Species ~ ., data = iris)        # -> multinom
#' fitMod(Sepal.Length ~ ., data = iris)   # -> lm
#'
#' # Explicit
#' fitMod(Species ~ ., data = iris, fitfn = "rpart")
#'
#' # Mixed models
#' fitMod(Reaction ~ Days + (1|Subject), lme4::sleepstudy, fitfn = "lmMixed")
#'
#' @export
fitMod <- function(formula, data, ..., subset, na.action = na.pass,
                   fitfn = NULL) {
  
  # --- validate inputs ---
  if (!inherits(formula, "formula"))
    stop("'formula' must be a formula object.")
  if (!is.data.frame(data))
    stop("'data' must be a data frame.")
  
  # --- build call ---
  cl <- match.call()
  
  # --- auto-detect fitting function if needed ---
  if (is.null(fitfn)) {
    resp  <- eval(formula[[2]], envir = data, enclos = parent.frame())
    fitfn <- .guess_fitfn(resp)
    message("fitMod: using fitfn = '", fitfn, "'")
  } else {
    if (!fitfn %in% names(.fitfn_registry))
      stop(
        "Unknown fitfn '", fitfn, "'. ",
        "Choose one of: ",
        paste(names(.fitfn_registry), collapse = ", "), "."
      )
  }
  
  # --- look up registry entry ---
  entry <- .fitfn_registry[[fitfn]]
  
  # --- glmnet: auto-detect family and convert formula to x/y ---
  if (fitfn == "glmnet") {
    resp <- eval(formula[[2L]], envir = data, enclos = parent.frame())
    
    if (is.null(cl[["family"]]))
      cl[["family"]] <- if (isDichotomous(resp))           "binomial"
    else if (inherits(resp, "factor")) "multinomial"
    else if (inherits(resp, "integer")) "poisson"
    else                               "gaussian"
    
    # cv.glmnet has no formula interface - convert manually
    mf              <- model.frame(formula, data = data)
    x_train         <- model.matrix(formula[-2L], data = mf)[, -1L, drop = FALSE]
    cl[["y"]]       <- model.response(mf)
    cl[["x"]]       <- x_train
    cl[["formula"]] <- NULL
    cl[["data"]]    <- NULL
  }
  
  # --- xgboost: auto-detect objective and convert formula to x/y ---
  if (fitfn == "xgboost") {
    resp <- eval(formula[[2L]], envir = data, enclos = parent.frame())
    
    mf      <- model.frame(formula, data = data)
    y_raw   <- model.response(mf)
    x_train <- model.matrix(formula[-2L], data = mf)[, -1L, drop = FALSE]
    
    cl[["x"]]       <- x_train
    cl[["y"]]       <- y_raw
    cl[["formula"]] <- NULL
    cl[["data"]]    <- NULL
    
    cl[["objective"]] <- cl[["objective"]] %||%
      if (isDichotomous(resp))           "binary:logistic"
    else if (inherits(resp, "factor")) "multi:softprob"
    else if (inherits(resp, "integer")) "count:poisson"
    else                               "reg:squarederror"
    
    x_train_xgb  <- x_train
    y_levels_xgb <- if (is.factor(y_raw)) levels(y_raw) else NULL
  }
  
  # --- ensure package is available ---
  .require_pkg(entry$pkg)
  
  # --- resolve fitting function ---
  fun <- if (is.null(entry$pkg)) {
    get(entry$fn, envir = parent.env(environment()), inherits = TRUE)
  } else {
    get(entry$fn, asNamespace(entry$pkg), inherits = FALSE)
  }
  
  # --- apply registry defaults and strip fitMod-specific args ---
  cl       <- .apply_defaults(cl, entry$defaults)
  cl$fitfn <- NULL
  cl[[1L]] <- fun
  
  # --- fit model ---
  res <- eval(cl, parent.frame())
  
  # --- xgboost: wrap in list since xgboost objects don't support $<- ---
  if (fitfn == "xgboost") {
    res <- list(
      model    = res,
      fitfn    = fitfn,
      formula  = formula,
      x_train  = x_train_xgb,
      y_levels = y_levels_xgb,
      call     = match.call()
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
  
  # --- attach FitMod class and metadata (all other models) ---
  class(res)    <- c("FitMod", class(res))
  res$fitfn     <- fitfn
  res$call[[1]] <- as.name(entry$fix_call)
  
  # --- store glmnet-specific data for predict ---
  if (fitfn == "glmnet") {
    res[["formula"]] <- formula
    res[["x_train"]] <- x_train
  }
  
  # --- post-process ---
  res <- .postprocess(res, fitfn)
  
  res
}




# == internal helper functions ============================================



# Internal registry: one entry per supported fitting function

.fitfn_registry <- list(
  
  lm = list(
    pkg      = "stats",
    fn       = "lm",
    defaults = list(),
    fix_call = "lm"
  ),
  
  logit = list(
    pkg      = "stats",
    fn       = "glm",
    defaults = list(family = "binomial"),
    fix_call = "glm"
  ),
  
  poisson = list(
    pkg      = "stats",
    fn       = "glm",
    defaults = list(family = "poisson"),
    fix_call = "glm"
  ),
  
  quasipoisson = list(
    pkg      = "stats",
    fn       = "glm",
    defaults = list(family = "quasipoisson"),
    fix_call = "glm"
  ),
  
  gamma = list(
    pkg      = "stats",
    fn       = "glm",
    defaults = list(family = quote(Gamma(link = "log"))),
    fix_call = "glm"
  ),
  
  negbin = list(
    pkg      = "MASS",
    fn       = "glm.nb",
    defaults = list(),
    fix_call = "glm.nb"
  ),
  
  polr = list(
    pkg      = "MASS",
    fn       = "polr",
    defaults = list(Hess = TRUE, model = TRUE),
    fix_call = "polr"
  ),
  
  lmrob = list(
    pkg      = "robustbase",
    fn       = "lmrob",
    defaults = list(),
    fix_call = "lmrob"
  ),
  
  tobit = list(
    pkg      = NULL,        # internal function
    fn       = "tobit",
    defaults = list(),
    fix_call = "tobit"
  ),
  
  zeroinfl = list(
    pkg      = "pscl",
    fn       = "zeroinfl",
    defaults = list(),
    fix_call = "zeroinfl"
  ),
  
  multinom = list(
    pkg      = "nnet",
    fn       = "multinom",
    defaults = list(maxit = 500, model = TRUE, trace = FALSE),
    fix_call = "multinom"
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
    ),
    fix_call = "nnet"
  ),
  
  rpart = list(
    pkg      = "rpart",
    fn       = "rpart",
    defaults = list(model = TRUE, y = TRUE),
    fix_call = "rpart"
  ),
  
  randomForest = list(
    pkg      = "randomForest",
    fn       = "randomForest",
    defaults = list(na.action = na.omit),
    fix_call = "randomForest"
  ),
  
  C5.0 = list(
    pkg      = "C50",
    fn       = "C5.0",
    defaults = list(),
    fix_call = "C5.0"
  ),
  
  lda = list(
    pkg      = "MASS",
    fn       = "lda",
    defaults = list(),
    fix_call = "lda"
  ),
  
  qda = list(
    pkg      = "MASS",
    fn       = "qda",
    defaults = list(),
    fix_call = "qda"
  ),
  
  svm = list(
    pkg      = "e1071",
    fn       = "svm",
    defaults = list(probability = TRUE),
    fix_call = "svm"
  ),
  
  naiveBayes = list(
    pkg      = "naivebayes",
    fn       = "naive_bayes",
    defaults = list(),
    fix_call = "naive_bayes"
  ),
  
  glmnet = list(
    pkg      = "glmnet",
    fn       = "cv.glmnet",
    defaults = list(
      alpha   = 1,       # Lasso; user can override to 0 (Ridge) or 0.5 (Elastic Net)
      nfolds  = 10,
      family  = "multinomial"  # auto-detect would be better - see below
    ),
    fix_call = "cv.glmnet"
  ),
  
  xgboost = list(
    pkg      = "xgboost",
    fn       = "xgboost",
    defaults = list(
      nrounds       = 100L,
      max_depth     = 3L,
      learning_rate = 0.1
    ),
    fix_call = "xgboost"
  ),
  
  coxph = list(
    pkg      = "survival",
    fn       = "coxph",
    defaults = list(model = TRUE, x = TRUE),
    fix_call = "coxph"
  ),
  
  weibull = list(
    pkg      = "survival",
    fn       = "survreg",
    defaults = list(dist = "weibull"),
    fix_call = "survreg"
  ),
  
  exponential = list(
    pkg      = "survival",
    fn       = "survreg",
    defaults = list(dist = "exponential"),
    fix_call = "survreg"
  ),
  
  lognormal = list(
    pkg      = "survival",
    fn       = "survreg",
    defaults = list(dist = "lognormal"),
    fix_call = "survreg"
  ),
  
  loglogistic = list(
    pkg      = "survival",
    fn       = "survreg",
    defaults = list(dist = "loglogistic"),
    fix_call = "survreg"
  ),
  
  lmMixed = list(
    pkg      = "lme4",
    fn       = "lmer",
    defaults = list(),
    fix_call = "lmer"
  ),
  
  logitMixed = list(
    pkg      = "lme4",
    fn       = "glmer",
    defaults = list(family = "binomial"),
    fix_call = "glmer"
  ),
  
  poissonMixed = list(
    pkg      = "lme4",
    fn       = "glmer",
    defaults = list(family = "poisson"),
    fix_call = "glmer"
  ),
  
  negbinMixed = list(
    pkg      = "lme4",
    fn       = "glmer.nb",
    defaults = list(),
    fix_call = "glmer.nb"
  ),
  
  gammaMixed = list(
    pkg      = "lme4",
    fn       = "glmer",
    defaults = list(family = quote(Gamma(link = "log"))),
    fix_call = "glmer"
  )
  
  )


# -------------------------------------------------------------------------
# Auto-detect fitting function from response type
# -------------------------------------------------------------------------

#' @keywords internal
.guess_fitfn <- function(resp) {
  if (isDichotomous(resp))
    return("logit")
  if (inherits(resp, "ordered"))
    return("polr")
  if (inherits(resp, "factor"))
    return("multinom")
  if (inherits(resp, "integer"))
    return("poisson")
  if (inherits(resp, "numeric"))
    return("lm")
  stop(
    "Cannot guess fitting function for response of class '",
    paste(class(resp), collapse = "/"), "'. ",
    "Please provide 'fitfn' explicitly."
  )
}


# -------------------------------------------------------------------------
# Ensure optional package is available
# -------------------------------------------------------------------------

#' @keywords internal
.require_pkg <- function(pkg) {
  if (!is.null(pkg) && !requireNamespace(pkg, quietly = TRUE))
    stop("Package '", pkg, "' must be installed for this fitting function.")
}


# -------------------------------------------------------------------------
# Apply registry defaults to call (only if not already set by user)
# -------------------------------------------------------------------------

#' @keywords internal
.apply_defaults <- function(cl, defaults) {
  for (nm in names(defaults))
    if (is.null(cl[[nm]]))
      cl[[nm]] <- defaults[[nm]]
  cl
}


# -------------------------------------------------------------------------
# Post-processing: steps that extend the result object after fitting
# -------------------------------------------------------------------------

#' @keywords internal
.postprocess <- function(res, fitfn) {
  UseMethod(".postprocess")
}

#' @keywords internal
.postprocess.multinom <- function(res, fitfn) {
  # Wald z-test p-values (2-tailed)
  sm <- suppressMessages(summary(res))
  z  <- sm$coefficients / sm$standard.errors
  res[["pval"]]  <- (1 - pnorm(abs(z), 0, 1)) * 2
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
    stop("object must be of class 'multinom'")
  
  test <- match.arg(test)
  
  # Make multinom available in the evaluation environment -
  # required during R CMD check where nnet is not on the search path
  multinom <- getFromNamespace("multinom", "nnet")
  env <- environment(formula(object))
  assign("multinom", multinom, envir = env)
  
  if (missing(scope))
    scope <- drop.scope(object)
  else {
    if (!is.character(scope))
      scope <- attr(terms(update.formula(object, scope)), "term.labels")
    if (!all(scope %in% attr(object$terms, "term.labels")))
      stop("scope is not a subset of term labels")
  }
  
  # Resolve environment; fall back to caller if formula has no attached env
  env <- environment(formula(object))
  if (is.null(env))
    env <- parent.frame()
  
  # Use model.frame: works without explicit data=, already stripped to
  # exactly the rows used during fitting
  orig_data <- model.frame(object)
  
  # Collision-safe temp name (random 32-bit hex)
  tmp_name <- sprintf(".drop1_data_%08x", sample.int(2147483647L, 1L))
  assign(tmp_name, orig_data, envir = env)
  on.exit(
    if (exists(tmp_name, envir = env, inherits = FALSE))
      rm(list = tmp_name, envir = env),
    add = TRUE
  )
  
  ns  <- length(scope)
  
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
    call$data <- as.name(tmp_name)
    
    nfit <- eval(call, envir = env)
    
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



