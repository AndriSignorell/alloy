#' k-Fold Cross-Validation of a Fitted Model
#'
#' Estimates the out-of-sample performance of a fitted model by k-fold
#' cross-validation. The data are split into `k` folds, the model is refitted
#' on `k - 1` of them with the specification of `fit` and evaluated on the
#' fold left out, until every fold has served once as test set. The procedure
#' can be repeated with new random splits.
#'
#' @param fit a model fitted with [fitMod()], or a plain [lm()] or [glm()]
#'   (binomial, Poisson, quasi-Poisson or Gamma family), which is refitted
#'   through [fitMod()] with the matching engine
#' @param data the data frame to split. `NULL` (default) evaluates the `data`
#'   argument of the original call in the calling environment.
#' @param k the number of folds, between 2 and the number of observations
#'   (leave-one-out). Default 10.
#' @param repeats the number of times the cross-validation is repeated with a
#'   new random split. Default 1.
#' @param folds an optional vector of length `nrow(data)` with predefined fold
#'   memberships, e.g. for grouped cross-validation where all observations of
#'   a cluster must fall into the same fold. Overrides `k`, `repeats` and
#'   `strata`.
#' @param strata `TRUE` (default) stratifies the folds by the response for
#'   classification models and does not stratify regression models; `FALSE`
#'   never stratifies. A vector of length `nrow(data)`, or a list of such
#'   vectors, gives the strata explicitly, see [splitTrainTest()].
#' @param cutoff the probability threshold above which a binary prediction
#'   counts as positive, used for accuracy, sensitivity and specificity.
#'   Default 0.5.
#' @param keepModels logical, whether to return the models fitted on the
#'   training folds. Default `FALSE`.
#'
#' @details
#' Each training fold is refitted from the call stored in `fit` with only the
#' data replaced, so every argument of the original fit (engine, family,
#' weights, tuning parameters) is carried over. A `subset` of the original
#' call is applied to `data` once, before the split. Observations with missing
#' values in any model variable are removed beforehand as well, so that all
#' folds are of comparable size and the same observations enter the apparent
#' and the cross-validated estimates. Prior weights of the fit are used in
#' refitting, but the performance measures are unweighted.
#'
#' The performance measures depend on the task:
#'
#' \describe{
#'   \item{binary classification}{`auc` (c-statistic), `brier` and
#'     `brierScaled` (Brier score and its scaled version, see
#'     [DescToolsX::brierScore()]), `logLoss` (mean negative log-likelihood),
#'     `calIntercept` and `calSlope` (calibration-in-the-large and
#'     calibration slope, see [plotCalibration()]), and `accuracy`,
#'     `sensitivity` and `specificity` at `cutoff`. The second level of the
#'     response is the positive class.}
#'   \item{multiclass classification}{`accuracy`, `kappa`, `brier` (summed
#'     over the classes) and `logLoss`.}
#'   \item{regression}{`rmse`, `mae`, `rSq` (\eqn{1 - SSE/SST} on the test
#'     fold) and `calSlope` (slope of the observed on the predicted values).}
#' }
#'
#' The measures are reported in three versions. *Apparent* is the performance
#' of `fit` on the data it was fitted to and therefore optimistic. *CV* is the
#' mean over all folds (and repeats), with its standard deviation and range
#' across the folds. *Pooled* computes each measure once on all out-of-fold
#' predictions of a repetition, averaged over the repetitions; it is the more
#' stable estimate for small folds and the only one available for
#' leave-one-out, where a single observation has no AUC or calibration slope.
#' *Optimism* is the difference Apparent - CV: positive for measures where
#' higher is better (`auc`, `rSq`), negative for losses (`brier`, `logLoss`,
#' `rmse`). For a logistic model fitted by maximum likelihood the apparent
#' calibration intercept and slope are 0 and 1 by construction; a
#' cross-validated slope clearly below 1 is the signature of overfitting.
#'
#' Warnings raised while fitting the folds (e.g. fitted probabilities of 0 or
#' 1) are collected and reported once, with the number of folds concerned. A
#' fold whose model cannot be fitted or cannot predict its test data (e.g.
#' because of a factor level absent from the training folds) is reported and
#' left out; the function only fails if no fold succeeds.
#'
#' The split is drawn from R's global random number generator; call
#' [set.seed()] before the function for reproducible results. Survival models
#' are not supported.
#'
#' @return An object of class `"CrossVal"`, a list with the components
#' \describe{
#'   \item{summary}{a data frame with one row per measure and the columns
#'     `apparent`, `cv`, `sd`, `min`, `max`, `pooled` and `optimism`.}
#'   \item{perFold}{a data frame with one row per fold: `rep`, `fold`,
#'     `nTrain`, `nTest` and the measures.}
#'   \item{pred}{the out-of-fold predictions as a data frame with `rep`,
#'     `fold`, `row` (row in `data`), `obs` and `pred` (the probability of
#'     the positive class, the predicted value, or for multiclass models the
#'     predicted class followed by one probability column per class).}
#'   \item{foldId}{an integer matrix of fold memberships, one column per
#'     repetition, rows matching the observations used.}
#'   \item{coefs}{a matrix of the coefficients per fold, if [coef()] returns
#'     a vector of the same length for every fold, else `NULL`.}
#'   \item{models}{the models of the training folds if `keepModels = TRUE`,
#'     else `NULL`.}
#'   \item{warnings, errors}{data frames of the messages collected, with the
#'     number of folds concerned.}
#' }
#' and the settings `task`, `engine`, `formula`, `n`, `nDropped`, `k`,
#' `repeats`, `stratified`, `cutoff`, `levels` and `call`.
#'
#' @examples
#' fit <- fitMod(admit ~ gre + gpa + rank, Admit, engine = "logit")
#'
#' set.seed(1)
#' cv <- crossVal(fit)
#' cv
#'
#' # the measures of every fold, and the variability of the coefficients
#' head(cv$perFold)
#' apply(cv$coefs, 2, sd)
#'
#' # repeated cross-validation, keeping the fold models
#' set.seed(1)
#' cv <- crossVal(fit, k = 5, repeats = 10, keepModels = TRUE)
#' round(cv$summary, 3)
#'
#' # grouped cross-validation: all rows of a cluster in the same fold
#' d <- Admit
#' d$id <- rep(1:100, each = 4)
#' ids <- unique(d$id)
#' grp <- splitTrainTest(ids, p = setNames(rep(1, 5), 1:5), output = "group")
#' cv <- crossVal(fit, data = d, folds = grp[match(d$id, ids)])
#'
#' # regression
#' crossVal(fitMod(Fertility ~ ., swiss, engine = "lm"), k = 5)
#'
#' @family data.split
#' @concept model-evaluation
#'
#' @export
crossVal <- function(fit, data = NULL, k = 10, repeats = 1, folds = NULL,
                     strata = TRUE, cutoff = 0.5, keepModels = FALSE) {

  cl <- match.call()
  env <- parent.frame()

  # --- the model: refit call, formula, engine, task ------------------------
  refit <- .cvRefitCall(fit)
  fo <- tryCatch(stats::formula(fit),
                 error = function(e) eval(refit$formula, env))
  engine <- .cvEngine(fit)
  task <- .cvTask(fit, engine)

  # --- the data: as in the original call, subset applied, complete cases ---
  if (is.null(data)) {
    if (is.null(refit$data))
      stop("'data' is not stored in the call of 'fit'; supply it explicitly.")
    data <- eval(refit$data, env)
  }
  if (!is.data.frame(data))
    stop("'data' must be a data frame.")

  nOrig <- nrow(data)
  keep <- .cvKeepRows(refit, fo, data, env)
  refit$subset <- NULL
  data <- data[keep, , drop = FALSE]
  n <- nrow(data)

  foEnv <- environment(fo)
  obs <- eval(fo[[2L]], data, if (is.null(foEnv)) env else foEnv)

  # a plain lm/glm is refitted once through fitMod(), so that predictions
  # come on the same scale as those of the fold models
  evalEnv <- new.env(parent = env)
  if (!inherits(fit, "FitMod")) {
    full <- refit
    full$data <- as.name(".cvTrain")
    assign(".cvTrain", data, envir = evalEnv)
    fit <- suppressMessages(eval(full, evalEnv))
  }

  # --- apparent performance of fit on the same observations ----------------
  pa <- .cvQuietly(predict(fit, newdata = data))
  if (inherits(pa$value, "cvError"))
    stop("'fit' cannot predict 'data': ", pa$value)

  lev <- if (task == "regression") NULL else colnames(as.matrix(pa$value))
  if (task != "regression") {
    obs <- factor(as.character(obs), levels = lev)
    if (anyNA(obs))
      stop("the response has values not among the predicted classes ",
           paste(sQuote(lev), collapse = ", "), ".")
    task <- if (length(lev) == 2L) "binary" else "multiclass"
  }

  apparent <- .cvMetrics(obs, .cvPredValue(pa$value, task, lev), task, cutoff)

  # --- fold memberships ----------------------------------------------------
  foldId <- .cvFoldId(n, k, repeats, folds, strata, keep, nOrig, obs, task)
  repeats <- ncol(foldId)
  k <- length(unique(foldId[, 1L]))

  # --- refit and evaluate fold by fold -------------------------------------
  refit$data <- as.name(".cvTrain")

  runs <- vector("list", repeats * k)
  i <- 0L

  for (r in seq_len(repeats)) {
    ids <- sort(unique(foldId[, r]))
    for (f in ids) {
      i <- i + 1L
      test <- foldId[, r] == f
      runs[[i]] <- .cvRunFold(refit, evalEnv, data, test, obs, task, lev,
                              cutoff, keepModels)
      runs[[i]]$rep <- r
      runs[[i]]$fold <- f
      if (!is.null(runs[[i]]$pred)) {
        runs[[i]]$pred$rep <- r
        runs[[i]]$pred$fold <- f
        runs[[i]]$pred$row <- keep[runs[[i]]$pred$row]
      }
    }
  }
  runs <- runs[seq_len(i)]

  failed <- vapply(runs, function(z) !is.null(z$error), logical(1L))
  errors <- .cvTally(lapply(runs, `[[`, "error"))
  warns <- .cvTally(c(list(pa$warnings), lapply(runs, `[[`, "warnings")))

  if (all(failed))
    stop("no fold could be fitted and evaluated: ", errors$message[1L])
  if (any(failed))
    warning(gettextf("%d of %d folds failed and were left out:\n%s",
                     sum(failed), length(runs), .cvFormatTally(errors)),
            call. = FALSE, domain = NA)
  if (nrow(warns) > 0L)
    warning(gettextf("warnings in fitting the folds:\n%s",
                     .cvFormatTally(warns)),
            call. = FALSE, domain = NA)

  ok <- runs[!failed]

  # --- assemble ------------------------------------------------------------
  perFold <- do.call(rbind, lapply(ok, function(z)
    data.frame(rep = z$rep, fold = z$fold, nTrain = n - z$nTest,
               nTest = z$nTest, t(z$metrics), check.names = FALSE)))
  rownames(perFold) <- NULL

  pred <- do.call(rbind, lapply(ok, `[[`, "pred"))
  rownames(pred) <- NULL

  metricNames <- names(apparent)
  m <- as.matrix(perFold[metricNames])

  pooled <- colMeans(do.call(rbind, lapply(split(pred, pred$rep), function(z)
    .cvMetrics(z$obs, .cvPooledValue(z, task, lev), task, cutoff))))

  cvMean <- colMeans(m, na.rm = TRUE)
  summary <- data.frame(
    apparent = apparent,
    cv       = cvMean,
    sd       = apply(m, 2L, stats::sd, na.rm = TRUE),
    min      = .cvRange(m, min),
    max      = .cvRange(m, max),
    pooled   = pooled,
    optimism = apparent - cvMean,
    row.names = metricNames
  )
  summary[is.na(summary)] <- NA

  models <- if (keepModels)
    stats::setNames(lapply(ok, `[[`, "model"),
                    .cvRunNames(perFold$rep, perFold$fold, repeats))

  coefs <- .cvCoefMatrix(lapply(ok, `[[`, "coef"),
                         .cvRunNames(perFold$rep, perFold$fold, repeats))

  dimnames(foldId) <- list(keep, paste0("rep", seq_len(repeats)))

  structure(list(
    summary    = summary,
    perFold    = perFold,
    pred       = pred,
    foldId     = foldId,
    coefs      = coefs,
    models     = models,
    warnings   = warns,
    errors     = errors,
    task       = task,
    engine     = engine,
    formula    = fo,
    n          = n,
    nDropped   = nOrig - n,
    k          = k,
    repeats    = repeats,
    stratified = attr(foldId, "stratified"),
    cutoff     = if (task == "binary") cutoff else NULL,
    levels     = lev,
    call       = cl
  ), class = "CrossVal")
}


#' @rdname crossVal
#' @param x an object of class `"CrossVal"`
#' @param digits the number of digits to print
#' @param ... further arguments, ignored
#' @export
print.CrossVal <- function(x, digits = 3, ...) {

  design <- if (x$repeats > 1L)
    gettextf("%d-fold, %d repeats", x$k, x$repeats)
  else
    gettextf("%d-fold", x$k)
  if (isTRUE(x$stratified)) design <- paste0(design, ", stratified")

  cat("\nCross-validation (", design, ")\n\n", sep = "")
  cat("Model:  ", deparse1(x$formula), "  [", x$engine, "]\n", sep = "")
  cat("Obs:    ", x$n,
      if (x$nDropped > 0L) gettextf(" (%d removed)", x$nDropped), sep = "")
  if (x$task == "binary")
    cat(gettextf(", positive class %s, cutoff %g",
                 dQuote(x$levels[2L], FALSE), x$cutoff))
  cat("\n\n")

  s <- x$summary
  names(s) <- c("Apparent", "CV", "SD", "Min", "Max", "Pooled", "Optimism")
  print(format(round(as.matrix(s), digits), nsmall = digits), quote = FALSE,
        right = TRUE)

  if (nrow(x$errors) > 0L)
    cat(gettextf("\n%d fold(s) failed, see $errors\n", sum(x$errors$folds)))
  cat("\n")

  invisible(x)
}



# == internal helper functions ============================================


# The call that refits the model, always through fitMod(): the original
# call for wrapped models (glmnet, xgboost, lme4 store the fitMod call),
# else the arguments of the underlying fitter plus the engine. The head is
# namespaced, so alloy need not be attached.
.cvRefitCall <- function(fit) {

  cl <- stats::getCall(fit)
  if (is.null(cl))
    stop("'fit' does not store its call and cannot be refitted.")

  args <- as.list(cl)[-1L]
  if (!.cvIsFitModCall(cl))
    args$engine <- .cvEngine(fit)

  as.call(c(list(quote(alloy::fitMod)), args))
}


.cvIsFitModCall <- function(cl) {
  head <- cl[[1L]]
  identical(head, as.name("fitMod")) ||
    (is.call(head) && identical(head[[3L]], as.name("fitMod")))
}


# engine of a FitMod object, or the engine that refits a plain lm/glm
.cvEngine <- function(fit) {

  if (inherits(fit, "FitMod"))
    return(fit$engine)

  if (identical(class(fit), "lm"))
    return("lm")

  if (inherits(fit, "glm")) {
    engine <- switch(fit$family$family,
                     binomial = "logit", poisson = "poisson",
                     quasipoisson = "quasipoisson", Gamma = "gamma", NULL)
    if (!is.null(engine))
      return(engine)
  }

  stop("'fit' must be a model fitted with fitMod(), an lm or a binomial, ",
       "Poisson, quasi-Poisson or Gamma glm.")
}


.cvTask <- function(fit, engine) {

  if (engine %in% c("coxph", "weibull", "exponential", "lognormal",
                    "loglogistic", "tobit"))
    stop("crossVal() does not support survival models.")

  if (!inherits(fit, "FitMod"))
    return(if (engine == "logit") "classification" else "regression")

  inner <- if (inherits(fit, c("FitMod.lme4", "FitMod.xgboost")))
    fit$model else fit

  if (.is_classification(fit, inner, engine)) "classification"
  else "regression"
}


# Rows of data entering the cross-validation: the subset of the original
# call, then complete cases of the model variables
.cvKeepRows <- function(refit, fo, data, env) {

  keep <- rep(TRUE, nrow(data))

  if (!is.null(refit$subset)) {
    s <- eval(refit$subset, data, env)
    if (is.logical(s))
      keep <- keep & !is.na(s) & s
    else
      keep <- seq_len(nrow(data)) %in% s
  }

  vars <- all.vars(fo)
  vars <- if ("." %in% vars) names(data) else intersect(vars, names(data))
  if (!is.null(refit$weights))
    vars <- union(vars, intersect(all.vars(refit$weights), names(data)))

  keep <- keep & stats::complete.cases(data[vars])

  if (!any(keep))
    stop("no complete observations left in 'data'.")

  which(keep)
}


# Fold memberships as an n x repeats integer matrix
.cvFoldId <- function(n, k, repeats, folds, strata, keep, nOrig, obs, task) {

  if (!is.null(folds)) {
    if (length(folds) != nOrig)
      stop(gettextf("'folds' must be of length %d, not %d.",
                    nOrig, length(folds)))
    if (repeats > 1L)
      warning("'repeats' is ignored when 'folds' is given.", call. = FALSE)
    id <- as.integer(factor(folds[keep]))
    if (anyNA(id) || length(unique(id)) < 2L)
      stop("'folds' must define at least two folds without missing values.")
    return(structure(matrix(id), stratified = FALSE))
  }

  if (!is.numeric(k) || length(k) != 1L || k < 2 || k > n || k != round(k))
    stop(gettextf("'k' must be an integer between 2 and %d.", n))
  if (!is.numeric(repeats) || length(repeats) != 1L || repeats < 1 ||
      repeats != round(repeats))
    stop("'repeats' must be a positive integer.")

  s <- if (isTRUE(strata)) {
    if (task == "regression") NULL else obs
  } else if (isFALSE(strata)) {
    NULL
  } else {
    nStrata <- if (is.data.frame(strata)) nrow(strata)
               else if (is.list(strata)) length(strata[[1L]])
               else length(strata)
    if (nStrata != nOrig)
      stop(gettextf("'strata' must be of length %d, not %d.",
                    nOrig, nStrata))
    if (is.list(strata)) lapply(strata, `[`, keep) else strata[keep]
  }

  p <- stats::setNames(rep(1, k), seq_len(k))

  id <- vapply(seq_len(repeats), function(r)
    as.integer(splitTrainTest(seq_len(n), p = p, strata = s,
                              output = "group")),
    integer(n))

  structure(matrix(id, nrow = n), stratified = !is.null(s))
}


# Refit on the training folds, predict the test fold, compute the measures
.cvRunFold <- function(refit, evalEnv, data, test, obs, task, lev, cutoff,
                       keepModel) {

  assign(".cvTrain", data[!test, , drop = FALSE], envir = evalEnv)
  on.exit(rm(".cvTrain", envir = evalEnv))

  res <- .cvQuietly({
    model <- eval(refit, evalEnv)
    p <- predict(model, newdata = data[test, , drop = FALSE])
    list(model = model, value = .cvPredValue(p, task, lev))
  })

  out <- list(nTest = sum(test), warnings = res$warnings, error = NULL)

  if (inherits(res$value, "cvError")) {
    out$error <- unclass(res$value)
    return(out)
  }

  value <- res$value$value
  out$metrics <- .cvQuietly(.cvMetrics(obs[test], value, task, cutoff))$value
  out$pred <- .cvPredFrame(which(test), obs[test], value, task, lev)
  out$coef <- tryCatch(stats::coef(res$value$model), error = function(e) NULL)
  if (keepModel) out$model <- res$value$model

  out
}


# Predictions in the form the measures need: P(positive) for binary,
# the probability matrix for multiclass, the numeric prediction otherwise
.cvPredValue <- function(p, task, lev) {
  if (task == "regression")
    return(as.numeric(unlist(p)))
  p <- as.matrix(p)[, lev, drop = FALSE]
  if (task == "binary") unname(p[, 2L]) else unname(p)
}


.cvPredFrame <- function(row, obs, value, task, lev) {
  if (task != "multiclass")
    return(data.frame(rep = NA_integer_, fold = NA_integer_, row = row,
                      obs = obs, pred = value))
  cls <- factor(lev[max.col(value, ties.method = "first")], levels = lev)
  data.frame(rep = NA_integer_, fold = NA_integer_, row = row, obs = obs,
             pred = cls, stats::setNames(as.data.frame(value), lev),
             check.names = FALSE)
}


.cvPooledValue <- function(pred, task, lev) {
  if (task == "multiclass") as.matrix(pred[lev]) else pred$pred
}


.cvMetrics <- function(obs, pred, task, cutoff) {
  switch(task,
         binary     = .cvMetricsBinary(obs, pred, cutoff),
         multiclass = .cvMetricsMulti(obs, pred),
         regression = .cvMetricsRegr(obs, pred))
}


.cvMetricsBinary <- function(obs, p, cutoff) {

  y <- as.integer(obs) - 1L
  pos <- p >= cutoff
  pc <- pmin(pmax(p, 1e-15), 1 - 1e-15)

  # AUC, scaled Brier score and calibration need both classes; a fold
  # holding a single class (always so in leave-one-out) has none of them
  both <- length(unique(y)) == 2L
  cal <- if (both)
    .cvSafe(unlist(.calibrationCoefs(p, y)), c(NA_real_, NA_real_))
  else c(NA_real_, NA_real_)

  c(auc          = if (both) .cvSafe(cStat(p, y)) else NA_real_,
    brier        = brierScore(p, y),
    brierScaled  = if (both) .cvSafe(brierScore(p, y, scaled = TRUE))
                   else NA_real_,
    logLoss      = -mean(y * log(pc) + (1 - y) * log(1 - pc)),
    calIntercept = cal[[1L]],
    calSlope     = cal[[2L]],
    accuracy     = mean(pos == (y == 1L)),
    sensitivity  = if (any(y == 1L)) mean(pos[y == 1L]) else NA_real_,
    specificity  = if (any(y == 0L)) mean(!pos[y == 0L]) else NA_real_)
}


.cvMetricsMulti <- function(obs, p) {

  lev <- levels(obs)
  ind <- outer(as.integer(obs), seq_along(lev), `==`)
  cls <- factor(lev[max.col(p, ties.method = "first")], levels = lev)
  cm <- .cvSafe(suppressMessages(confusion(cls, obs)), NULL)

  c(accuracy = mean(cls == obs),
    kappa    = if (is.null(cm)) NA_real_ else cm$kappa,
    brier    = mean(rowSums((p - ind)^2)),
    logLoss  = -mean(log(pmax(p[ind], 1e-15))))
}


.cvMetricsRegr <- function(obs, pred) {

  res <- obs - pred
  sst <- sum((obs - mean(obs))^2)

  c(rmse     = sqrt(mean(res^2)),
    mae      = mean(abs(res)),
    rSq      = if (sst > 0) 1 - sum(res^2) / sst else NA_real_,
    calSlope = .cvSafe(unname(stats::coef(stats::lm(obs ~ pred))[2L])))
}


# Evaluate expr, collecting warnings and swallowing messages; an error is
# returned as a string of class "cvError" instead of being thrown
.cvQuietly <- function(expr) {
  warns <- character()
  value <- withCallingHandlers(
    tryCatch(expr, error = function(e)
      structure(conditionMessage(e), class = "cvError")),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) invokeRestart("muffleMessage")
  )
  list(value = value, warnings = unique(warns))
}


.cvSafe <- function(expr, default = NA_real_) {
  res <- tryCatch(suppressWarnings(expr), error = function(e) default)
  if (length(res) == 0L) default else res
}


# Distinct messages and the number of folds (list elements) raising them
.cvTally <- function(msgs) {
  msgs <- unlist(lapply(msgs, unique))
  if (length(msgs) == 0L)
    return(data.frame(message = character(), folds = integer()))
  tab <- table(factor(msgs, levels = unique(msgs)))
  data.frame(message = names(tab), folds = as.integer(tab),
             stringsAsFactors = FALSE)
}


.cvFormatTally <- function(tally) {
  paste0("  [", tally$folds, "x] ", tally$message, collapse = "\n")
}


.cvRange <- function(m, fun) {
  apply(m, 2L, function(z) if (all(is.na(z))) NA_real_ else fun(z, na.rm = TRUE))
}


.cvRunNames <- function(rep, fold, repeats) {
  if (repeats > 1L) paste0("rep", rep, ".fold", fold) else paste0("fold", fold)
}


.cvCoefMatrix <- function(coefs, names) {
  ok <- !vapply(coefs, is.null, logical(1L)) &
    vapply(coefs, function(z) is.numeric(z) && is.null(dim(z)), logical(1L))
  if (!all(ok) || length(unique(lengths(coefs))) != 1L)
    return(NULL)
  do.call(rbind, stats::setNames(coefs, names))
}
