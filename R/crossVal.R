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
#'   through [fitMod()] with the matching engine. Binary models need a
#'   Bernoulli response (one 0/1, logical or two-level factor value per row);
#'   aggregated binomial data (`cbind(successes, failures)` or proportions
#'   with trial weights) are not supported.
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
#' @param cutoff the probability threshold at or above which a binary
#'   prediction counts as positive, used for accuracy, sensitivity and
#'   specificity: a number in \eqn{[0, 1]} (default 0.5), or
#'   `"youden"` or `"closest.topleft"` for the optimal cut-point of
#'   [bestCut()], determined anew on every training fold (see Details).
#' @param keepModels logical, whether to return the models fitted on the
#'   training folds. Default `FALSE`.
#'
#' @details
#' **Observations.** The rows entering the cross-validation are determined
#' with the semantics of [model.frame()]: the `subset` of the original call is
#' resolved with R's indexing rules (logical, positive, negative or row-name
#' indices), and rows are dropped where any model term, including transformed
#' ones such as `log(x)` or `sqrt(x)`, the response or the weights evaluate to
#' `NA` or `NaN`. This is done once before the split, so that the apparent and
#' the cross-validated estimates rest on the same observations and the folds
#' are of comparable size. A subset that selects a row more than once is
#' rejected, as the copies could otherwise end up in training and test fold.
#' Variables are evaluated on the whole data only to find these rows; every
#' fold model recomputes its terms (e.g. `poly()` or `scale()`) from its own
#' training data. Terms that cannot be evaluated on `data` are an error, as
#' they would be for the original fit.
#'
#' The response must be computed row by row (`log(y)` is fine), since it is
#' compared with predictions of models whose response was computed on their
#' training folds only. A response that depends on other observations is
#' rejected if it calls a known data-dependent function such as `scale()`,
#' `mean()` or `rank()`, or if its values recomputed on the odd rows, the
#' even rows or the first half of the rows differ from those computed on
#' all rows. The second check is a heuristic that may miss
#' a user-defined transformation which happens to agree on that subset;
#' such transformations are the user's responsibility. Computing the
#' transformation in `data` beforehand gives all folds a common scale, but
#' its estimation (e.g. of the mean and SD used by `scale()`) is then not
#' part of the cross-validation.
#'
#' **Refitting.** Each training fold is refitted from the call stored in
#' `fit` with the data replaced, so the engine, family, link and tuning
#' parameters are carried over. Row-wise arguments are aligned with the
#' folds: prior weights, whether a column of `data` or a vector outside it,
#' are evaluated once and split together with the data. Prior weights are
#' used in refitting, but the performance measures are unweighted. An
#' `offset` argument is not supported (specify it in the formula as
#' `offset()`), nor is a formula variable of observation length that does not
#' belong to `data`.
#'
#' **Measures.** They depend on the task:
#'
#' \describe{
#'   \item{binary classification}{`auc` (c-statistic), `brier` and
#'     `brierScaled` (Brier score and its scaled version, see
#'     [DescToolsX::brierScore()]), `logLoss` (mean negative log-likelihood),
#'     `calIntercept` (calibration-in-the-large: the intercept of a logistic
#'     regression of the outcome with the logit of the prediction as offset,
#'     i.e. with the slope fixed at 1), `calSlope` (the slope of the logistic
#'     regression of the outcome on the logit of the prediction). The two
#'     fits are separate. The intercept is `NA` if its fit does not
#'     converge; the slope is `NA` if the logits of the two classes do not
#'     overlap (complete or quasi-complete separation), if the logits are
#'     constant, or if its fit does not converge. Each case is reported in
#'     the warnings. The remaining measures are `accuracy`, `sensitivity`
#'     and `specificity` at `cutoff`. The second
#'     level of the response is the positive class.}
#'   \item{multiclass classification}{`accuracy`, `kappa`, `brier` (summed
#'     over the classes) and `logLoss`.}
#'   \item{regression}{`rmse`, `mae`, `rSq` (\eqn{1 - SSE/SST} on the test
#'     fold) and `calSlope` (slope of the observed on the predicted values).}
#' }
#'
#' **Data-driven cutoff.** With `cutoff = "youden"` (or
#' `"closest.topleft"`) the cut-point is part of the model and is
#' cross-validated with it: every fold model determines its cut-point with
#' [bestCut()] on its own training data and classifies the test fold with it,
#' so the cut-point never sees the observations it classifies. The apparent
#' measures use the cut-point of `fit` on all data, as
#' `bestCut(roc(fit))` returns it. A cut-point chosen on all data and then
#' passed as a number would make the cross-validated accuracy, sensitivity
#' and specificity optimistic. The cut-points are reported in `perFold` and
#' `pred`; the pooled measures classify every prediction with the cut-point
#' of its own fold. Ties of the criterion are resolved by the first
#' cut-point that \pkg{pROC} reports.
#'
#' **Apparent, CV, pooled.** *Apparent* is the performance of `fit` on the
#' data it was fitted to and therefore optimistic. *CV* is the unweighted mean
#' of the fold results over all folds (and repeats): every fold counts
#' equally, whatever its size. `sd`, `min` and `max` describe the spread of
#' the fold results; `sd` is not a standard error of the CV estimate. *Pooled*
#' computes each measure once on all out-of-fold predictions of a repetition
#' and averages over the repetitions. This is a different estimand from the
#' fold mean, also for losses such as the RMSE, and the more stable choice for
#' small folds. With leave-one-out, measures that need both classes or
#' several observations (`auc`, `brierScaled`, the calibration measures,
#' `rSq`, `calSlope`, `kappa`) exist only in their pooled version.
#'
#' *Optimism* is the difference Apparent - CV: positive for measures where
#' higher is better (`auc`, `rSq`, `accuracy`), negative for losses (`brier`,
#' `logLoss`, `rmse`, `mae`). For the calibration measures it is merely the
#' difference, without a better direction. For an unpenalised logistic model
#' with an intercept, fitted by maximum likelihood without weights, the
#' apparent calibration intercept and slope are 0 and 1 by construction; a
#' cross-validated slope clearly below 1 is the signature of overfitting.
#'
#' Cross-validation assesses the given model specification. A variable
#' selection or tuning done beforehand on the full data is not part of the
#' refit and therefore not validated.
#'
#' **Failures.** Warnings raised while fitting the folds or computing the
#' measures (e.g. fitted probabilities of 0 or 1) are collected and reported
#' once, with the number of times each was raised (counting the folds, the
#' apparent and the pooled evaluations). A fold whose model cannot be fitted, cannot predict
#' its test data (e.g. because of a factor level absent from the training
#' folds), returns predictions of the wrong shape or invalid values, or whose
#' measures cannot be computed, is kept in `perFold` with `ok = FALSE` and
#' its error, and is left out of the CV estimates; the function only fails if
#' no fold succeeds. The pooled estimates use only repetitions in which every
#' observation was predicted, as an incomplete repetition would describe a
#' different population; without such a repetition they are `NA`.
#'
#' For binary models, [plotCalibration()] and [roc()] accept the result
#' and show the out-of-fold predictions, see [rocTest()] for comparing the
#' cross-validated ROC curves of two models.
#'
#' The split is drawn from R's global random number generator; call
#' [set.seed()] before the function for reproducible results. Fit a model
#' whose fitting draws random numbers itself (e.g. a random forest) before
#' setting the seed: written as a call inside `crossVal()`, it is evaluated
#' before the split and shifts the random stream, so that the folds no
#' longer match those of another model cross-validated with the same seed. Survival models
#' are not supported.
#'
#' For mixed models, observations of clusters absent from the training folds
#' (always so in grouped cross-validation by cluster) are predicted at
#' population level, with the random effects set to zero.
#'
#' The models kept with `keepModels = TRUE` support `predict()`, `coef()` and
#' inspection. Their call refers to a temporary training set, so [update()]
#' needs the data supplied again.
#'
#' @return An object of class `"CrossVal"`, a list with the components
#' \describe{
#'   \item{summary}{a data frame with one row per measure and the columns
#'     `apparent`, `cv`, `sd`, `min`, `max`, `nFolds` (the number of folds
#'     with a value for the measure), `pooled` and `optimism`.}
#'   \item{perFold}{a data frame with one row per planned fold: `rep`,
#'     `fold`, `nTrain`, `nTest`, `ok`, `error`, for binary models `cutoff`,
#'     and the measures (`NA` for failed folds).}
#'   \item{reps}{a data frame with one row per repetition: `rep`,
#'     `nPredicted` and `coverage`, the share of observations with an
#'     out-of-fold prediction.}
#'   \item{pred}{the out-of-fold predictions as a data frame with `rep`,
#'     `fold`, `row` (row in `data`), `obs` and `pred` (the probability of
#'     the positive class, the predicted value, or for multiclass models the
#'     predicted class), for binary models followed by the `cutoff` it was
#'     classified with.}
#'   \item{prob}{for multiclass models a matrix of the predicted class
#'     probabilities, one column per class, its rows matching `pred`; else
#'     `NULL`.}
#'   \item{foldId}{an integer matrix of fold memberships, one column per
#'     repetition, rows matching the observations used.}
#'   \item{coefs}{a matrix of the coefficients per successful fold, matched
#'     by name and `NA` where a fold lacks a coefficient, if [coef()] returns
#'     a named numeric vector for every fold (unnamed vectors of equal length
#'     are bound by position), else `NULL`.}
#'   \item{models}{the models of the successful training folds if
#'     `keepModels = TRUE`, else `NULL`.}
#'   \item{warnings, errors}{data frames of the distinct messages and their
#'     `count`: for `errors` the number of failed folds, for `warnings` the
#'     number of evaluations (folds, apparent, pooled repetitions) that raised
#'     the message.}
#' }
#' and the settings `task`, `engine`, `formula`, `n`, `nDropped`, `k`,
#' `repeats`, `stratified`, `cutoff` (the number or method given),
#' `cutoffApparent` (the cut-point of the apparent measures), `levels` and
#' `call`.
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
#' apply(cv$coefs, 2, sd, na.rm = TRUE)
#'
#' # the Youden cut-point, determined on every training fold
#' set.seed(1)
#' cvY <- crossVal(fit, cutoff = "youden")
#' cvY$cutoffApparent
#' summary(cvY$perFold$cutoff)
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
  .cvCheckArgs(repeats, cutoff, keepModels)

  # --- the model: engine, task, refit call, formula ------------------------
  engine <- .cvEngine(fit)
  task <- .cvTask(fit, engine)
  refit <- .fitModCall(fit, engine = engine)
  fo <- tryCatch(stats::formula(fit),
                 error = function(e) eval(refit$formula, env))
  foEnv <- environment(fo)
  if (is.null(foEnv)) foEnv <- env

  if (!is.null(refit$offset))
    stop("an 'offset' argument is not supported; ",
         "specify the offset in the formula with offset().")

  # --- the data: rows as model.frame() would use them ----------------------
  if (is.null(data)) {
    if (is.null(refit$data))
      stop("'data' is not stored in the call of 'fit'; supply it explicitly.")
    data <- eval(refit$data, env)
  }
  if (!is.data.frame(data))
    stop("'data' must be a data frame.")

  nOrig <- nrow(data)
  rows <- .cvKeepRows(refit, fo, data, foEnv)
  keep <- rows$keep
  w <- if (!is.null(rows$weights)) rows$weights[keep]
  refit$subset <- NULL
  refit$weights <- NULL

  data <- data[keep, , drop = FALSE]
  n <- nrow(data)
  obs <- .cvResponse(fo, data, foEnv)

  # a plain lm/glm is refitted once through fitMod(), so that predictions
  # come on the same scale as those of the fold models
  evalEnv <- new.env(parent = env)
  if (!inherits(fit, "FitMod"))
    fit <- suppressMessages(.cvRefit(refit, data, w, evalEnv))

  # --- apparent performance of fit on the same observations ----------------
  pa <- .cvQuietly(.cvPredict(fit, data))
  if (inherits(pa$value, "cvError"))
    stop("'fit' cannot predict 'data': ", pa$value)

  lev <- NULL
  if (task != "regression") {
    lev <- colnames(as.matrix(pa$value))
    if (is.null(lev))
      stop("the predictions of 'fit' carry no class names.")
    if (length(lev) == 2L && is.numeric(obs) && any(obs != round(obs)))
      stop("a binary response must have one class per observation; ",
           "proportions with trial weights are not supported.")
    obs <- factor(as.character(obs), levels = lev)
    if (anyNA(obs))
      stop("the response has values not among the predicted classes ",
           paste(sQuote(lev), collapse = ", "), ".")
    task <- if (length(lev) == 2L) "binary" else "multiclass"
  }

  pApp <- .cvPredValue(pa$value, task, lev, n)
  cutApp <- if (task == "binary") .cvFindCutoff(pApp, obs, cutoff)
  am <- .cvQuietly(.cvMetrics(obs, pApp, task, cutApp))
  if (inherits(am$value, "cvError"))
    stop("the measures of 'fit' cannot be computed: ", am$value)
  apparent <- am$value
  metricNames <- names(apparent)
  naMetrics <- stats::setNames(rep(NA_real_, length(metricNames)),
                               metricNames)

  # --- fold memberships ----------------------------------------------------
  foldId <- .cvFoldId(n, k, repeats, folds, strata, keep, nOrig, obs, task)
  repeats <- ncol(foldId)
  k <- max(foldId)

  # --- refit and evaluate fold by fold -------------------------------------
  grid <- expand.grid(fold = seq_len(k), rep = seq_len(repeats))
  runs <- lapply(seq_len(nrow(grid)), function(i) {
    test <- foldId[, grid$rep[i]] == grid$fold[i]
    .cvRunFold(refit, evalEnv, data, w, test, obs, task, lev, cutoff,
               keepModels)
  })

  ok <- vapply(runs, function(z) is.null(z$error), logical(1L))
  errors <- .cvTally(lapply(runs, `[[`, "error"))

  if (!any(ok))
    stop("no fold could be fitted and evaluated: ", errors$message[1L])
  if (!all(ok))
    warning(gettextf("%d of %d folds failed and were left out:\n%s",
                     sum(!ok), length(runs), .cvFormatTally(errors)),
            call. = FALSE, domain = NA)
  # --- per fold ------------------------------------------------------------
  m <- t(vapply(runs, function(z)
    if (is.null(z$error)) z$metrics else naMetrics,
    numeric(length(metricNames))))
  m <- matrix(m, ncol = length(metricNames),
              dimnames = list(NULL, metricNames))

  perFold <- data.frame(
    rep    = grid$rep,
    fold   = grid$fold,
    nTrain = vapply(runs, `[[`, integer(1L), "nTrain"),
    nTest  = vapply(runs, `[[`, integer(1L), "nTest"),
    ok     = ok,
    error  = vapply(runs, function(z)
      if (is.null(z$error)) NA_character_ else z$error, character(1L)),
    check.names = FALSE)
  if (task == "binary")
    perFold$cutoff <- vapply(runs, function(z)
      if (is.null(z$cutoff)) NA_real_ else z$cutoff, numeric(1L))
  perFold <- cbind(perFold, m)

  # --- out-of-fold predictions ---------------------------------------------
  okRuns <- runs[ok]
  okRep <- grid$rep[ok]
  okFold <- grid$fold[ok]
  testRows <- lapply(okRuns, `[[`, "test")
  values <- lapply(okRuns, `[[`, "value")
  idx <- unlist(testRows)
  nTest <- lengths(testRows)

  prob <- if (task == "multiclass")
    `dimnames<-`(do.call(rbind, values), list(NULL, lev))

  pred <- data.frame(
    rep  = rep(okRep, nTest),
    fold = rep(okFold, nTest),
    row  = keep[idx],
    obs  = obs[idx],
    pred = if (task == "multiclass") .cvClass(prob, lev) else unlist(values))
  if (task == "binary")
    pred$cutoff <- rep(vapply(okRuns, `[[`, numeric(1L), "cutoff"), nTest)

  # --- pooled: complete repetitions only -----------------------------------
  nPred <- tabulate(pred$rep, repeats)
  reps <- data.frame(rep = seq_len(repeats), nPredicted = nPred,
                     coverage = nPred / n)
  complete <- which(nPred == n)

  pooledRep <- lapply(complete, function(r) {
    i <- pred$rep == r
    v <- if (task == "multiclass") prob[i, , drop = FALSE] else pred$pred[i]
    .cvQuietly(.cvMetrics(pred$obs[i], v, task, pred$cutoff[i]))
  })
  pooled <- if (length(pooledRep))
    colMeans(do.call(rbind, lapply(pooledRep, function(z)
      if (inherits(z$value, "cvError")) naMetrics else z$value)))
  else
    naMetrics

  warns <- .cvTally(c(list(pa$warnings, am$warnings),
                      lapply(runs, `[[`, "warnings"),
                      lapply(pooledRep, `[[`, "warnings")))
  if (nrow(warns) > 0L)
    warning(gettextf("warnings during the cross-validation:\n%s",
                     .cvFormatTally(warns)),
            call. = FALSE, domain = NA)

  if (length(complete) < repeats)
    warning(if (length(complete) == 0L)
      "no repetition predicted every observation; pooled measures are NA."
    else
      gettextf("pooled measures rest on the %d of %d repetitions that predicted every observation.",
               length(complete), repeats),
    call. = FALSE)

  # --- summary -------------------------------------------------------------
  cvMean <- .cvColApply(m, mean)
  summary <- data.frame(
    apparent = apparent,
    cv       = cvMean,
    sd       = .cvColApply(m, stats::sd),
    min      = .cvColApply(m, min),
    max      = .cvColApply(m, max),
    nFolds   = colSums(!is.na(m)),
    pooled   = pooled,
    optimism = apparent - cvMean,
    row.names = metricNames
  )

  runNames <- .cvRunNames(okRep, okFold, repeats)
  models <- if (keepModels)
    stats::setNames(lapply(okRuns, `[[`, "model"), runNames)
  coefs <- .cvCoefMatrix(lapply(okRuns, `[[`, "coef"), runNames)

  dimnames(foldId) <- list(keep, paste0("rep", seq_len(repeats)))

  structure(list(
    summary    = summary,
    perFold    = perFold,
    reps       = reps,
    pred       = pred,
    prob       = prob,
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
    cutoffApparent = cutApp,
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
    cat(gettextf(", positive class %s, cutoff %s",
                 dQuote(x$levels[2L], FALSE),
                 if (is.character(x$cutoff))
                   gettextf("%s per training fold (%s on all data)", x$cutoff,
                            format(x$cutoffApparent, digits = 3))
                 else format(x$cutoff)))
  cat("\n\n")

  s <- x$summary
  s$nFolds <- NULL
  names(s) <- c("Apparent", "CV", "SD", "Min", "Max", "Pooled", "Optimism")
  print(format(round(as.matrix(s), digits), nsmall = digits), quote = FALSE,
        right = TRUE)

  nFail <- sum(!x$perFold$ok)
  if (nFail > 0L)
    cat(gettextf("\n%d of %d folds failed, see $perFold and $errors\n",
                 nFail, nrow(x$perFold)))
  if (any(x$reps$coverage < 1))
    cat(gettextf("%d of %d repetitions incomplete, see $reps\n",
                 sum(x$reps$coverage < 1), nrow(x$reps)))
  cat("\n")

  invisible(x)
}



# == internal helper functions ============================================


# Out-of-fold predictions of a binary model, one per observation: with
# repeats, the mean of its predictions over the repetitions (a single model
# never made it, but every repetition held the observation out). Used by
# plotCalibration() and roc(). Observations never predicted (failed folds)
# are left out with a warning.
#' @keywords internal
.cvOutOfFold <- function(x) {

  if (!identical(x$task, "binary"))
    stop("this needs the cross-validation of a binary classification model, ",
         "not of a ", x$task, " one.", call. = FALSE)

  pr <- x$pred
  row <- sort(unique(pr$row))
  p <- as.vector(tapply(pr$pred, factor(pr$row, levels = row), mean))
  obs <- pr$obs[match(row, pr$row)]

  if (length(row) < x$n)
    warning(gettextf("%d of %d observations have no out-of-fold prediction (failed folds) and are left out.",
                     x$n - length(row), x$n), call. = FALSE, domain = NA)

  list(p = p, y = as.integer(obs) - 1L, row = row, levels = x$levels)
}



.cvIsCount <- function(x, min = 1) {
  is.numeric(x) && length(x) == 1L && is.finite(x) && x >= min &&
    x == round(x)
}


.cvCheckArgs <- function(repeats, cutoff, keepModels) {
  if (!.cvIsCount(repeats))
    stop("'repeats' must be a positive integer.")
  if (!(is.numeric(cutoff) && length(cutoff) == 1L && is.finite(cutoff) &&
        cutoff >= 0 && cutoff <= 1) &&
      !(is.character(cutoff) && length(cutoff) == 1L &&
        cutoff %in% c("youden", "closest.topleft")))
    stop("'cutoff' must be a single number in [0, 1], \"youden\" or ",
         "\"closest.topleft\".")
  if (!(is.logical(keepModels) && length(keepModels) == 1L &&
        !is.na(keepModels)))
    stop("'keepModels' must be TRUE or FALSE.")
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


# Rows of data entering the cross-validation, with model.frame() semantics:
# the subset resolved by R indexing, then rows where no model term, the
# response or the weights is NA/NaN. Returns the row indices and the prior
# weights evaluated on the full data.
.cvKeepRows <- function(refit, fo, data, foEnv) {

  n <- nrow(data)

  # formula variables of observation length must come from data, else they
  # could not be split with it
  for (v in setdiff(all.vars(fo), c(names(data), "."))) {
    obj <- get0(v, envir = foEnv, inherits = TRUE)
    if (!is.null(obj) && !is.function(obj) && n > 1L && NROW(obj) == n)
      stop(gettextf("the formula of 'fit' uses %s from outside 'data'; add it as a column of 'data'.",
                    sQuote(v)), domain = NA)
  }

  idx <- seq_len(n)
  if (!is.null(refit$subset)) {
    s <- eval(refit$subset, data, foEnv)
    names(idx) <- rownames(data)
    idx <- unname(idx[s])
    idx <- idx[!is.na(idx)]
    if (anyDuplicated(idx))
      stop("the 'subset' of 'fit' selects observations more than once, ",
           "which cross-validation cannot split.")
  }

  w <- NULL
  if (!is.null(refit$weights)) {
    w <- eval(refit$weights, data, foEnv)
    if (!is.numeric(w) || length(w) != n)
      stop(gettextf("the weights of 'fit' must be a numeric vector of length %d.",
                    n), domain = NA)
  }

  # an error here would also have stopped the original fit on these data
  mf <- tryCatch(
    suppressWarnings(stats::model.frame(.cvNoBars(fo), data = data,
                                        na.action = stats::na.pass)),
    error = function(e)
      stop("the model terms cannot be evaluated on 'data': ",
           conditionMessage(e), call. = FALSE))
  cc <- stats::complete.cases(mf)
  if (!is.null(w)) cc <- cc & !is.na(w)

  keep <- idx[cc[idx]]
  if (length(keep) == 0L)
    stop("no complete observations left in 'data'.")

  list(keep = keep, weights = w)
}


# random-effect bars (a | g) turned into (a + g), so that model.frame()
# sees all variables of a mixed model formula
.cvNoBars <- function(fo) {
  sub <- function(e) {
    if (!is.call(e)) return(e)
    if (identical(e[[1L]], as.name("|")) || identical(e[[1L]], as.name("||")))
      e[[1L]] <- as.name("+")
    as.call(lapply(as.list(e), sub))
  }
  fo[[length(fo)]] <- sub(fo[[length(fo)]])
  fo
}


# functions whose value for one row depends on the other rows
.cvDataDependent <- c(
  "scale", "mean", "median", "sd", "var", "mad", "IQR", "sum", "prod",
  "min", "max", "range", "rank", "quantile", "ecdf", "cumsum", "cumprod",
  "cummax", "cummin", "diff", "rev", "sort", "order", "length", "nrow",
  "NROW", "ave", "prop.table", "proportions", "table", "tabulate", "poly",
  "lag", "lead")


# names of all functions called in an expression
.cvCallNames <- function(e) {
  if (!is.call(e)) return(character())
  head <- e[[1L]]
  nm <- if (is.name(head)) as.character(head)
        else if (is.call(head) && as.character(head[[1L]]) %in% c("::", ":::"))
          as.character(head[[3L]])
  unique(c(nm, unlist(lapply(as.list(e)[-1L], .cvCallNames)),
           if (is.call(head)) .cvCallNames(head)))
}


# the response, checked to be a single column computed row by row
.cvResponse <- function(fo, data, foEnv) {

  obs <- eval(fo[[2L]], data, foEnv)

  if (NCOL(obs) != 1L)
    stop("crossVal() needs a response with one value per observation; ",
         "matrix responses such as cbind(successes, failures) are not ",
         "supported.")

  # the fold models evaluate the response on their training rows, so it
  # must not depend on the other rows: known data-dependent functions are
  # rejected, and as a heuristic for the rest the response is recomputed on
  # the odd rows, the even rows and the first half, and compared
  lhs <- fo[[2L]]
  bad <- intersect(.cvCallNames(lhs), .cvDataDependent)
  n <- nrow(data)
  if (length(bad) == 0L && n >= 2L) {
    for (i in list(seq(1L, n, by = 2L), seq(2L, n, by = 2L),
                   seq_len(n %/% 2L))) {
      part <- eval(lhs, data[i, , drop = FALSE], foEnv)
      same <- if (is.numeric(obs))
        isTRUE(all.equal(as.numeric(obs)[i], as.numeric(part)))
      else
        identical(as.character(unlist(obs))[i], as.character(unlist(part)))
      if (!same) {
        bad <- "?"
        break
      }
    }
  }
  if (length(bad))
    stop(gettextf("the response %s depends on other observations%s; compute it in 'data' beforehand (its estimation is then not cross-validated).",
                  sQuote(deparse1(lhs)),
                  if (identical(bad, "?")) "" else
                    paste0(" through ", paste0(bad, "()", collapse = ", "))),
         domain = NA)

  if (is.matrix(obs) || is.data.frame(obs)) obs <- obs[, 1L]
  # predict.FitMod() names the classes of a logical response "0" and "1"
  if (is.logical(obs)) as.integer(obs) else obs
}


# Fold memberships as an n x repeats integer matrix with ids 1..k
.cvFoldId <- function(n, k, repeats, folds, strata, keep, nOrig, obs, task) {

  if (!is.null(folds)) {
    if (length(folds) != nOrig)
      stop(gettextf("'folds' must be of length %d, not %d.",
                    nOrig, length(folds)))
    if (repeats > 1L)
      warning("'repeats' is ignored when 'folds' is given.", call. = FALSE)
    if (anyNA(folds[keep]))
      stop("'folds' must not have missing values.")
    id <- as.integer(factor(folds[keep]))
    if (max(id) < 2L)
      stop("'folds' must define at least two folds.")
    return(structure(matrix(id), stratified = FALSE))
  }

  if (!.cvIsCount(k, 2) || k > n)
    stop(gettextf("'k' must be an integer between 2 and %d.", n))

  # stratification is void for leave-one-out
  s <- if (k == n) {
    NULL
  } else if (isTRUE(strata)) {
    if (task == "regression") NULL else obs
  } else if (isFALSE(strata)) {
    NULL
  } else {
    sl <- if (is.list(strata)) as.list(strata) else list(strata)
    len <- vapply(sl, NROW, integer(1L))
    if (any(len != nOrig))
      stop(gettextf("every stratification vector must be of length %d.",
                    nOrig))
    if (is.list(strata)) lapply(sl, `[`, keep) else strata[keep]
  }

  p <- stats::setNames(rep(1, k), seq_len(k))

  id <- vapply(seq_len(repeats), function(r)
    as.integer(splitTrainTest(seq_len(n), p = p, strata = s,
                              output = "group")),
    integer(n))
  # splitTrainTest() stops if a fold would remain empty
  structure(matrix(id, nrow = n), stratified = !is.null(s))
}


# Evaluate the refit call on the training data; the weights are embedded as
# values, so they need not be found by the model's own lookup
.cvRefit <- function(refit, train, w, evalEnv) {
  refit$data <- as.name(".cvTrain")
  if (!is.null(w)) refit$weights <- w
  assign(".cvTrain", train, envir = evalEnv)
  on.exit(rm(".cvTrain", envir = evalEnv))
  eval(refit, evalEnv)
}


# Refit on the training folds, predict the test fold, compute the measures;
# any error on the way marks the fold as failed
.cvRunFold <- function(refit, evalEnv, data, w, test, obs, task, lev, cutoff,
                       keepModel) {

  train <- !test

  res <- .cvQuietly({
    model <- .cvRefit(refit, data[train, , drop = FALSE], w[train], evalEnv)
    value <- .cvPredValue(.cvPredict(model, data[test, , drop = FALSE]),
                          task, lev, sum(test))
    # a data-driven cut-point is found on the training fold only
    cut <- if (task == "binary")
      .cvFindCutoff(if (is.character(cutoff))
                      .cvPredValue(.cvPredict(model, data[train, , drop = FALSE]),
                                   task, lev, sum(train)),
                    obs[train], cutoff)
    list(model = model, value = value, cutoff = cut,
         metrics = .cvMetrics(obs[test], value, task, cut))
  })

  out <- list(nTrain = sum(train), nTest = sum(test),
              warnings = res$warnings, error = NULL)

  if (inherits(res$value, "cvError")) {
    out$error <- unclass(res$value)
    return(out)
  }

  out$test <- which(test)
  out$value <- res$value$value
  out$metrics <- res$value$metrics
  out$cutoff <- res$value$cutoff
  out$coef <- tryCatch(stats::coef(res$value$model), error = function(e) NULL)
  if (keepModel) out$model <- res$value$model

  out
}


# The cut-point for a binary model: the number given, or the optimum of
# bestCut() on the predictions p for the response obs (a two-level factor).
# Levels and direction are fixed, as in roc(), so that pROC cannot turn the
# curve; of tied optima the first is taken.
.cvFindCutoff <- function(p, obs, cutoff) {

  if (is.numeric(cutoff))
    return(cutoff)

  .require_pkg("pROC")
  r <- pROC::roc(response = as.integer(obs) - 1L, predictor = p,
                 levels = c(0, 1), direction = "<", quiet = TRUE)
  bc <- bestCut(r, method = cutoff)
  unname(if (is.matrix(bc)) bc["threshold", 1L] else bc[["threshold"]])
}


# Predictions of a fold model; clusters of a mixed model that are absent
# from the training folds are predicted at population level
.cvPredict <- function(model, newdata) {
  if (inherits(model, "FitMod.lme4"))
    stats::predict(model, newdata = newdata, allow.new.levels = TRUE)
  else
    stats::predict(model, newdata = newdata)
}


# Predictions in the form the measures need -- P(positive) for binary, the
# probability matrix for multiclass, the numeric prediction otherwise --
# checked for shape and valid values
.cvPredValue <- function(p, task, lev, n) {

  if (task == "regression") {
    if (NCOL(p) != 1L)
      stop(gettextf("the prediction has %d columns instead of 1.", NCOL(p)))
    p <- as.numeric(if (is.list(p)) p[[1L]] else p)
    if (length(p) != n)
      stop(gettextf("%d predictions for %d observations.", length(p), n))
    if (!all(is.finite(p)))
      stop("the predictions contain NA, NaN or infinite values.")
    return(p)
  }

  p <- as.matrix(p)
  if (nrow(p) != n)
    stop(gettextf("%d predictions for %d observations.", nrow(p), n))
  miss <- setdiff(lev, colnames(p))
  if (length(miss))
    stop("the prediction has no probability for class ",
         paste(sQuote(miss), collapse = ", "), ".")
  p <- unname(p[, lev, drop = FALSE])
  storage.mode(p) <- "double"
  if (!all(is.finite(p)) || any(p < 0 | p > 1))
    stop("the predicted probabilities are not all within [0, 1].")
  if (any(abs(rowSums(p) - 1) > 1e-6))
    stop("the predicted class probabilities do not sum to 1.")

  if (task == "binary") p[, 2L] else p
}


.cvClass <- function(p, lev) {
  factor(lev[max.col(p, ties.method = "first")], levels = lev)
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
  cal <- if (both) unlist(.calibrationCoefs(pc, y)) else c(NA_real_, NA_real_)

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
  cls <- .cvClass(p, lev)
  cm <- .cvSafe(suppressMessages(confusion(cls, obs)), NULL)

  c(accuracy = mean(cls == obs),
    kappa    = if (is.null(cm)) NA_real_ else unname(cm$kappa)[1L],
    brier    = mean(rowSums((p - ind)^2)),
    logLoss  = -mean(log(pmax(p[ind], 1e-15))))
}


.cvMetricsRegr <- function(obs, pred) {

  res <- obs - pred
  sst <- sum((obs - mean(obs))^2)

  c(rmse     = sqrt(mean(res^2)),
    mae      = mean(abs(res)),
    rSq      = if (sst > 0) 1 - sum(res^2) / sst else NA_real_,
    calSlope = if (length(obs) > 1L)
      .cvSafe(unname(stats::coef(stats::lm(obs ~ pred))[2L]))
    else NA_real_)
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


# Distinct messages and the number of evaluations (list elements) raising them
.cvTally <- function(msgs) {
  msgs <- unlist(lapply(msgs, unique))
  if (length(msgs) == 0L)
    return(data.frame(message = character(), count = integer()))
  tab <- table(factor(msgs, levels = unique(msgs)))
  data.frame(message = names(tab), count = as.integer(tab),
             stringsAsFactors = FALSE)
}


.cvFormatTally <- function(tally) {
  paste0("  [", tally$count, "x] ", tally$message, collapse = "\n")
}


# column-wise statistic over the non-missing values, NA if there are none
# (or, for sd, fewer than two)
.cvColApply <- function(m, fun) {
  apply(m, 2L, function(z) {
    z <- z[!is.na(z)]
    if (length(z) == 0L) NA_real_ else fun(z)
  })
}


.cvRunNames <- function(rep, fold, repeats) {
  if (repeats > 1L) paste0("rep", rep, ".fold", fold) else paste0("fold", fold)
}


# coefficients of the folds as a matrix, matched by name; NULL unless every
# fold returns a plain numeric vector, and if names are missing for some
# folds only (vectors without any names are bound by position if of equal
# length)
.cvCoefMatrix <- function(coefs, runNames) {

  ok <- vapply(coefs, function(z)
    is.numeric(z) && is.null(dim(z)) && length(z) > 0L, logical(1L))
  if (length(coefs) == 0L || !all(ok))
    return(NULL)

  named <- vapply(coefs, function(z) !is.null(names(z)), logical(1L))

  if (!any(named)) {
    if (length(unique(lengths(coefs))) != 1L)
      return(NULL)
    return(do.call(rbind, stats::setNames(coefs, runNames)))
  }
  if (!all(named))
    return(NULL)

  all <- unique(unlist(lapply(coefs, names)))
  matrix(unlist(lapply(coefs, function(z) unname(z[all]))),
         ncol = length(all), byrow = TRUE, dimnames = list(runNames, all))
}
