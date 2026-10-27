
# tests for crossVal() ---------------------------------------------------------

fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, engine = "logit")


test_that("the result has the documented structure", {

  set.seed(1)
  cv <- crossVal(fitLogit, k = 5)

  expect_s3_class(cv, "CrossVal")
  expect_identical(cv$task, "binary")
  expect_identical(cv$k, 5L)
  expect_identical(cv$repeats, 1L)
  expect_true(cv$stratified)

  measures <- c("auc", "brier", "brierScaled", "logLoss", "calIntercept",
                "calSlope", "accuracy", "sensitivity", "specificity")
  expect_identical(rownames(cv$summary), measures)
  expect_named(cv$summary, c("apparent", "cv", "sd", "min", "max", "nFolds",
                             "pooled", "optimism"))
  expect_named(cv$perFold, c("rep", "fold", "nTrain", "nTest", "ok", "error",
                             "cutoff", measures))
  expect_true(all(cv$perFold$cutoff == 0.5))
  expect_identical(nrow(cv$perFold), 5L)
  expect_true(all(cv$perFold$ok))
  expect_identical(sum(cv$perFold$nTest), 400L)
  expect_true(all(cv$perFold$nTrain + cv$perFold$nTest == 400L))

  # every observation predicted exactly once, out of fold
  expect_identical(sort(cv$pred$row), 1:400)
  expect_named(cv$pred, c("rep", "fold", "row", "obs", "pred", "cutoff"))
  expect_null(cv$prob)
  expect_identical(cv$cutoffApparent, 0.5)
  expect_identical(cv$reps$coverage, 1)
  expect_null(cv$models)
  expect_identical(dim(cv$coefs), c(5L, 6L))
  expect_named(cv$warnings, c("message", "count"))
  expect_named(cv$errors, c("message", "count"))

  expect_output(print(cv), "Cross-validation \\(5-fold, stratified\\)")

})


test_that("the measures agree with a hand-made cross-validation", {

  set.seed(7)
  cv <- crossVal(fitLogit, k = 4, keepModels = TRUE)

  id <- cv$foldId[, 1L]
  for (f in 1:4) {
    m <- glm(admit ~ gre + gpa + rank, binomial, Admit[id != f, ])
    p <- predict(m, Admit[id == f, ], type = "response")
    y <- Admit$admit[id == f]

    expect_equal(unname(coef(cv$models[[f]])), unname(coef(m)))
    expect_equal(cv$perFold$brier[f], mean((p - y)^2))
    expect_equal(cv$perFold$accuracy[f], mean((p >= 0.5) == (y == 1)))
    expect_equal(cv$perFold$logLoss[f],
                 -mean(y * log(p) + (1 - y) * log(1 - p)))
  }

  expect_equal(cv$summary["brier", "cv"], mean(cv$perFold$brier))
  expect_equal(cv$summary["brier", "pooled"],
               mean((cv$pred$pred - (as.integer(cv$pred$obs) - 1L))^2))

})


test_that("a data-driven cut-point is found on every training fold", {

  skip_if_not_installed("pROC")

  set.seed(7)
  cv <- crossVal(fitLogit, k = 4, cutoff = "youden", keepModels = TRUE)

  # apparent: the cut-point of bestCut(roc(fit)) on all data
  expect_equal(cv$cutoffApparent,
               unname(bestCut(roc(fitLogit, quiet = TRUE))["threshold"]))
  p <- predict(fitLogit)[, 2L]
  expect_equal(cv$summary["accuracy", "apparent"],
               mean((p >= cv$cutoffApparent) == (Admit$admit == 1)))

  id <- cv$foldId[, 1L]
  for (f in 1:4) {
    m <- cv$models[[f]]
    tr <- id != f
    pTr <- predict(m, newdata = Admit[tr, ])[, 2L]
    cut <- unname(bestCut(roc(pTr, ref = Admit$admit[tr], levels = c(0, 1),
                              direction = "<", quiet = TRUE))["threshold"])
    pTe <- predict(m, newdata = Admit[!tr, ])[, 2L]
    yTe <- Admit$admit[!tr]

    expect_equal(cv$perFold$cutoff[f], cut)
    expect_equal(cv$perFold$accuracy[f], mean((pTe >= cut) == (yTe == 1)))
    expect_equal(cv$perFold$sensitivity[f], mean(pTe[yTe == 1] >= cut))
  }

  # the cut-points differ between folds, and the pooled accuracy classifies
  # each prediction with the cut-point of its own fold
  expect_gt(length(unique(cv$perFold$cutoff)), 1L)
  y <- as.integer(cv$pred$obs) - 1L
  expect_equal(cv$summary["accuracy", "pooled"],
               mean((cv$pred$pred >= cv$pred$cutoff) == (y == 1L)))

  expect_identical(cv$cutoff, "youden")
  expect_output(print(cv), "youden per training fold")

  set.seed(7)
  expect_identical(crossVal(fitLogit, k = 4,
                            cutoff = "closest.topleft")$cutoff,
                   "closest.topleft")
})


test_that("apparent calibration of a logit model is 0 / 1 by construction", {

  set.seed(1)
  s <- crossVal(fitLogit, k = 5)$summary

  expect_equal(s["calIntercept", "apparent"], 0, tolerance = 1e-6)
  expect_equal(s["calSlope", "apparent"], 1, tolerance = 1e-6)
  expect_equal(s$optimism, s$apparent - s$cv)

})


test_that("folds are stratified by the response", {

  set.seed(1)
  cv <- crossVal(fitLogit, k = 4)
  tab <- table(cv$foldId[, 1L], Admit$admit)

  # largest remainder: at most one apart per class
  expect_true(all(apply(tab, 2L, function(z) diff(range(z))) <= 1L))

  set.seed(1)
  expect_false(crossVal(fitLogit, k = 4, strata = FALSE)$stratified)

})


test_that("repeats and keepModels", {

  set.seed(1)
  cv <- crossVal(fitLogit, k = 3, repeats = 4, keepModels = TRUE)

  expect_identical(cv$repeats, 4L)
  expect_identical(dim(cv$foldId), c(400L, 4L))
  expect_identical(nrow(cv$perFold), 12L)
  expect_identical(nrow(cv$pred), 1600L)
  expect_identical(cv$reps$coverage, rep(1, 4))
  expect_length(cv$models, 12L)
  expect_identical(names(cv$models)[1:2], c("rep1.fold1", "rep1.fold2"))
  expect_s3_class(cv$models[[1L]], "FitMod")

})


test_that("the RNG decides the split and nothing else", {

  set.seed(11)
  a <- crossVal(fitLogit, k = 5)
  set.seed(11)
  b <- crossVal(fitLogit, k = 5)

  expect_identical(a$foldId, b$foldId)
  expect_equal(a$summary, b$summary)

})


test_that("predefined folds give grouped cross-validation", {

  d <- Admit
  d$id <- rep(1:100, each = 4)
  set.seed(1)
  grp <- sample(rep(1:5, 20))[d$id]

  cv <- crossVal(fitLogit, data = d, folds = grp)

  expect_identical(cv$k, 5L)
  expect_false(cv$stratified)
  # no cluster is split across folds
  expect_true(all(tapply(cv$foldId[, 1L], d$id,
                         function(z) length(unique(z))) == 1L))

  expect_error(crossVal(fitLogit, folds = 1:3), "'folds' must be of length")

})


test_that("a plain glm or lm is accepted", {

  g <- glm(admit ~ gre + gpa + rank, binomial, Admit)

  set.seed(3)
  a <- crossVal(g, k = 5)
  set.seed(3)
  b <- crossVal(fitLogit, k = 5)

  expect_identical(a$engine, "logit")
  expect_equal(a$summary, b$summary)

  set.seed(3)
  expect_identical(crossVal(lm(Fertility ~ ., swiss), k = 5)$engine, "lm")

  expect_error(crossVal(glm(Fertility ~ ., gaussian, swiss)),
               "must be a model fitted with fitMod")

})


test_that("subset and missing values are removed before the split", {

  d <- Admit
  d$gre[c(3, 7)] <- NA
  fit <- fitMod(admit ~ gre + gpa + rank, d, engine = "logit",
                subset = gpa > 2.5)

  set.seed(1)
  cv <- crossVal(fit, k = 5)

  n <- sum(d$gpa > 2.5 & !is.na(d$gre))
  expect_identical(cv$n, n)
  expect_identical(cv$nDropped, 400L - n)
  expect_false(any(c(3L, 7L) %in% cv$pred$row))
  expect_true(all(d$gpa[cv$pred$row] > 2.5))

})


test_that("external weights, a negative subset and NA rows stay aligned", {

  d <- swiss
  d$Agriculture[5L] <- NA
  w <- seq(0.5, 2, length.out = nrow(d))
  fit <- lm(Fertility ~ Agriculture + Education, d, weights = w,
            subset = -c(1, 2))
  folds <- rep(1:3, length.out = nrow(d))

  cv <- crossVal(fit, folds = folds, keepModels = TRUE)

  keep <- setdiff(3:47, 5L)
  expect_identical(cv$n, length(keep))
  expect_identical(sort(cv$pred$row), keep)

  for (f in 1:3) {
    tr <- keep[folds[keep] != f]
    te <- keep[folds[keep] == f]
    m <- lm(Fertility ~ Agriculture + Education, d[tr, ], weights = w[tr])

    expect_identical(cv$perFold$nTrain[f], length(tr))
    expect_equal(unname(cv$coefs[f, names(coef(m))]), unname(coef(m)))
    expect_identical(cv$pred$row[cv$pred$fold == f], te)
    expect_equal(cv$pred$pred[cv$pred$fold == f],
                 unname(predict(m, d[te, ])))
  }

  # the subset must not be read as "keep rows 1 and 2"
  expect_false(any(1:2 %in% cv$pred$row))

})


test_that("leave-one-out reports pooled measures only where defined", {

  cv <- crossVal(fitLogit, k = nrow(Admit))
  s <- cv$summary

  expect_identical(cv$k, 400L)
  expect_false(cv$stratified)
  expect_true(is.na(s["auc", "cv"]))
  expect_identical(s["auc", "nFolds"], 0)
  expect_false(is.na(s["auc", "pooled"]))
  expect_false(is.na(s["brier", "cv"]))

})


test_that("failing folds are kept, flagged and left out of the estimates", {

  d <- Admit
  d$rank <- as.character(d$rank)
  d$rank[1L] <- "5"
  d$rank <- factor(d$rank)
  fit <- fitMod(admit ~ gre + rank, d, engine = "logit")

  set.seed(3)
  w <- capture_warnings(cv <- crossVal(fit, k = 5))
  expect_match(w, "1 of 5 folds failed", all = FALSE)
  expect_match(w, "pooled measures are NA", all = FALSE)

  measures <- rownames(cv$summary)
  expect_identical(nrow(cv$perFold), 5L)
  expect_identical(sum(cv$perFold$ok), 4L)
  expect_match(cv$perFold$error[!cv$perFold$ok], "new levels")
  expect_true(all(is.na(cv$perFold[!cv$perFold$ok, measures])))
  expect_true(cv$reps$coverage < 1)
  expect_true(all(is.na(cv$summary$pooled)))
  expect_true(all(cv$summary$nFolds <= 4))
  expect_match(cv$errors$message, "new levels")
  expect_identical(cv$errors$count, 1L)

})


test_that("regression: pooled RMSE per repetition, then averaged", {

  # n = 47, k = 5: folds of unequal size
  set.seed(1)
  cv <- crossVal(fitMod(Fertility ~ ., swiss, engine = "lm"), k = 5,
                 repeats = 3)

  expect_identical(cv$task, "regression")
  expect_identical(rownames(cv$summary), c("rmse", "mae", "rSq", "calSlope"))
  expect_false(cv$stratified)
  expect_equal(cv$summary["rmse", "apparent"],
               sqrt(mean(residuals(lm(Fertility ~ ., swiss))^2)))
  expect_gt(length(unique(cv$perFold$nTest)), 1L)

  res <- cv$pred$obs - cv$pred$pred
  rmseRep <- tapply(res, cv$pred$rep, function(r) sqrt(mean(r^2)))
  expect_equal(cv$summary["rmse", "pooled"], mean(rmseRep))
  # not the RMSE over all repetitions at once
  expect_false(isTRUE(all.equal(cv$summary["rmse", "pooled"],
                                sqrt(mean(res^2)), tolerance = 1e-12)))
  # CV: unweighted mean over the folds
  expect_equal(cv$summary["rmse", "cv"], mean(cv$perFold$rmse))

  foldRmse <- tapply(res, list(cv$pred$rep, cv$pred$fold),
                     function(r) sqrt(mean(r^2)))
  expect_equal(cv$perFold$rmse, as.vector(t(foldRmse)))

})


test_that("multiclass probabilities are kept apart from the metadata", {

  # class names that collide with the columns of $pred
  d <- iris
  d$cls <- factor(c("pred", "rep", "row")[as.integer(d$Species)])
  d$Species <- NULL

  set.seed(1)
  cv <- suppressWarnings(
    crossVal(fitMod(cls ~ Sepal.Length + Sepal.Width, d, engine = "multinom"),
             k = 5))

  lev <- levels(d$cls)
  expect_identical(cv$task, "multiclass")
  expect_identical(rownames(cv$summary),
                   c("accuracy", "kappa", "brier", "logLoss"))
  expect_true(cv$stratified)
  expect_named(cv$pred, c("rep", "fold", "row", "obs", "pred"))
  expect_identical(colnames(cv$prob), lev)
  expect_identical(nrow(cv$prob), nrow(cv$pred))
  expect_true(is.integer(cv$pred$row))
  expect_equal(unname(rowSums(cv$prob)), rep(1, nrow(cv$prob)))

  # predicted class from the probabilities of the same row
  expect_identical(as.character(cv$pred$pred),
                   lev[max.col(cv$prob, ties.method = "first")])

  # pooled measures recomputed independently
  ind <- outer(as.integer(cv$pred$obs), seq_along(lev), `==`)
  expect_equal(cv$summary["brier", "pooled"],
               mean(rowSums((cv$prob - ind)^2)))
  expect_equal(cv$summary["logLoss", "pooled"],
               -mean(log(pmax(cv$prob[ind], 1e-15))))
  expect_equal(cv$summary["accuracy", "pooled"],
               mean(cv$pred$pred == cv$pred$obs))

  expect_true(all(cv$perFold$ok))
  expect_identical(cv$reps$coverage, 1)
  expect_identical(sort(cv$pred$row), seq_len(nrow(d)))
  expect_identical(
    as.character(cv$pred$obs),
    as.character(d$cls[cv$pred$row])
  )
  
})


test_that("calibration: separate fits, NA slope on separation", {

  cal <- function(p, y)
    unlist(alloy:::.calibrationCoefs(p, y), use.names = FALSE)

  # overlapping classes: as two separate glm() fits
  set.seed(1)
  p <- runif(200, 0.05, 0.95)
  y <- rbinom(200, 1, plogis(0.3 + 0.7 * qlogis(p)))
  citl <- unname(coef(glm(y ~ 1, binomial, offset = qlogis(p))))
  slope <- unname(coef(glm(y ~ qlogis(p), binomial))[2L])
  expect_equal(cal(p, y), c(citl, slope), tolerance = 1e-6)

  y4 <- c(0, 0, 1, 1)
  citlOf <- function(p) unname(coef(glm(y4 ~ 1, binomial, offset = qlogis(p))))

  # complete, quasi-complete and reversed separation: slope NA, intercept
  # still estimated
  for (p in list(c(0.2, 0.3, 0.7, 0.8), c(0.2, 0.5, 0.5, 0.8),
                 c(0.8, 0.7, 0.3, 0.2))) {
    expect_warning(r <- cal(p, y4), "separate the classes")
    expect_true(is.na(r[2L]))
    expect_equal(r[1L], citlOf(p), tolerance = 1e-6)
  }

  # constant predictions: slope not identifiable
  expect_warning(r <- cal(rep(0.4, 6), c(0, 1, 0, 1, 1, 0)),
                 "not identifiable")
  expect_true(is.na(r[2L]))
  expect_equal(r[1L], qlogis(0.5) - qlogis(0.4))

})


test_that("fold coefficients are matched by name", {

  cm <- alloy:::.cvCoefMatrix

  z <- cm(list(c(a = 1, b = 2), c(b = 3, c = 4)), c("fold1", "fold2"))
  expect_identical(colnames(z), c("a", "b", "c"))
  expect_identical(rownames(z), c("fold1", "fold2"))
  expect_identical(unname(z[1, ]), c(1, 2, NA_real_))
  expect_identical(unname(z[2, ]), c(NA_real_, 3, 4))

  # another order of the same names
  z <- cm(list(c(a = 1, b = 2), c(b = 3, a = 4)), c("f1", "f2"))
  expect_identical(unname(z[2, ]), c(4, 3))

  # a single fold (the parameter used to shadow names())
  expect_identical(dim(cm(list(c(a = 1, b = 2)), "fold1")), c(1L, 2L))

  # named and unnamed mixed, matrices, unequal unnamed lengths: NULL
  expect_null(cm(list(c(a = 1, b = 2), c(3, 4)), c("f1", "f2")))
  expect_null(cm(list(matrix(1:4, 2), c(a = 1)), c("f1", "f2")))
  expect_null(cm(list(c(1, 2), c(3, 4, 5)), c("f1", "f2")))

})


test_that("data-dependent responses are rejected", {

  set.seed(1)
  expect_identical(
    crossVal(lm(log(Fertility) ~ Education, swiss), k = 5)$n, 47L)

  expect_error(crossVal(lm(scale(Fertility) ~ Education, swiss)),
               "through scale\\(\\)")
  expect_error(crossVal(lm(I(Fertility - mean(Fertility)) ~ Education, swiss)),
               "through mean\\(\\)")
  expect_error(
    crossVal(lm(I(Fertility - base::mean(Fertility)) ~ Education, swiss)),
    "through mean\\(\\)")

  # the former gap: the mean of the odd rows equals the overall mean
  dd <- data.frame(y = c(1, 1, 3, 3), x = 1:4)
  expect_error(alloy:::.cvResponse(I(y - mean(y)) ~ x, dd, environment()),
               "depends on other observations")

  # user-defined transformation, caught by the subset heuristic
  ctr <- function(v) v - v[1L]
  dd <- data.frame(y = c(1, 2, 1, 3, 5, 1), x = 1:6)
  expect_error(alloy:::.cvResponse(ctr(y) ~ x, dd, environment()),
               "depends on other observations")

})


test_that("survival models and bad arguments are rejected", {

  fitCox <- fitMod(Surv(foltime, folstatus) ~ gender, Whas100,
                   engine = "coxph")
  expect_error(crossVal(fitCox), "does not support survival")

  expect_error(crossVal(fitLogit, k = 1), "'k' must be an integer")
  expect_error(crossVal(fitLogit, k = 2.5), "'k' must be an integer")
  expect_error(crossVal(fitLogit, k = NA), "'k' must be an integer")
  expect_error(crossVal(fitLogit, k = Inf), "'k' must be an integer")
  expect_error(crossVal(fitLogit, k = 401), "'k' must be an integer")
  expect_error(crossVal(fitLogit, repeats = 0), "'repeats' must be")
  expect_error(crossVal(fitLogit, repeats = NA), "'repeats' must be")
  expect_error(crossVal(fitLogit, repeats = Inf), "'repeats' must be")
  expect_error(crossVal(fitLogit, repeats = 1:2), "'repeats' must be")

  for (co in list(-0.1, 1.1, NA, Inf, c(0.3, 0.5), "0.5", "Youden",
                 c("youden", "closest.topleft")))
    expect_error(crossVal(fitLogit, cutoff = co), "'cutoff' must be")

  for (km in list(NA, c(TRUE, FALSE), "yes"))
    expect_error(crossVal(fitLogit, keepModels = km), "'keepModels' must be")

  expect_error(crossVal(fitLogit, strata = Admit$rank[-1L]),
               "stratification vector must be of length 400")
  expect_error(crossVal(fitLogit,
                        strata = list(Admit$rank, Admit$rank[-1L])),
               "stratification vector must be of length 400")

  expect_error(crossVal(glm(cbind(admit, 1 - admit) ~ gre, binomial, Admit)),
               "matrix responses")
  expect_error(crossVal(lm(Fertility ~ Education, swiss, offset = Catholic)),
               "'offset' argument is not supported")

})
