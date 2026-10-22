
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
  expect_named(cv$summary, c("apparent", "cv", "sd", "min", "max",
                             "pooled", "optimism"))
  expect_identical(nrow(cv$perFold), 5L)
  expect_identical(sum(cv$perFold$nTest), 400L)
  expect_true(all(cv$perFold$nTrain + cv$perFold$nTest == 400L))

  # every observation predicted exactly once, out of fold
  expect_identical(sort(cv$pred$row), 1:400)
  expect_null(cv$models)
  expect_identical(dim(cv$coefs), c(5L, 6L))

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


test_that("leave-one-out reports pooled measures only where defined", {

  cv <- crossVal(fitLogit, k = nrow(Admit))
  s <- cv$summary

  expect_identical(cv$k, 400L)
  expect_true(is.na(s["auc", "cv"]))
  expect_false(is.na(s["auc", "pooled"]))
  expect_false(is.na(s["brier", "cv"]))

})


test_that("failing folds are reported and left out", {

  d <- Admit
  d$rank <- as.character(d$rank)
  d$rank[1L] <- "5"
  d$rank <- factor(d$rank)
  fit <- fitMod(admit ~ gre + rank, d, engine = "logit")

  set.seed(3)
  expect_warning(cv <- crossVal(fit, k = 5), "1 of 5 folds failed")
  expect_identical(nrow(cv$perFold), 4L)
  expect_match(cv$errors$message, "new levels")

})


test_that("regression and multiclass models", {

  set.seed(1)
  cv <- crossVal(fitMod(Fertility ~ ., swiss, engine = "lm"), k = 5)
  expect_identical(cv$task, "regression")
  expect_identical(rownames(cv$summary), c("rmse", "mae", "rSq", "calSlope"))
  expect_false(cv$stratified)
  expect_equal(cv$summary["rmse", "apparent"],
               sqrt(mean(residuals(lm(Fertility ~ ., swiss))^2)))

  set.seed(1)
  cv <- crossVal(fitMod(Species ~ ., iris, engine = "multinom"), k = 5)
  expect_identical(cv$task, "multiclass")
  expect_identical(rownames(cv$summary),
                   c("accuracy", "kappa", "brier", "logLoss"))
  expect_named(cv$pred, c("rep", "fold", "row", "obs", "pred",
                          levels(iris$Species)))
  expect_true(cv$stratified)

})


test_that("survival models and bad arguments are rejected", {

  fitCox <- fitMod(Surv(foltime, folstatus) ~ gender, Whas100,
                   engine = "coxph")
  expect_error(crossVal(fitCox), "does not support survival")

  expect_error(crossVal(fitLogit, k = 1), "'k' must be an integer")
  expect_error(crossVal(fitLogit, k = 2.5), "'k' must be an integer")
  expect_error(crossVal(fitLogit, repeats = 0), "'repeats' must be")

})
