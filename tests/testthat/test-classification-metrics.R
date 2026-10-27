
# tests for the classification metrics of fitted models --------------------------

fit <- fitMod(admit ~ gre + gpa + rank, Admit, engine = "logit")
glmFit <- glm(admit ~ gre + gpa + rank, binomial, Admit)


# -- confusion() ---------------------------------------------------------------

test_that("a logit model gives the same result as the glm method", {

  g <- glm(admit ~ gre + gpa + rank, binomial, Admit)

  a <- confusion(fit)
  b <- confusion(g)

  expect_s3_class(a, "Confusion")
  expect_identical(a$pos, "1")
  expect_equal(unclass(a$table), unclass(b$table), ignore_attr = TRUE)
  expect_equal(a$byclass, b$byclass)

  expect_equal(confusion(fit, cutoff = 0.3)$byclass,
               confusion(g, cutoff = 0.3)$byclass)

})


test_that("the cutoff and the positive class are honoured", {

  p <- predict(fit)[["1"]]

  cm <- confusion(fit, cutoff = 0.3)
  expect_identical(sum(cm$table["1", ]), sum(p > 0.3))

  cm <- confusion(fit, pos = "0")
  expect_identical(cm$pos, "0")
  expect_identical(rownames(cm$table)[1L], "0")

})


test_that("new data are classified and compared with their response", {

  set.seed(1)
  d <- splitTrainTest(Admit, p = 0.3)
  m <- update(fit, data = d$train)

  cm <- confusion(m, newdata = d$test)
  expect_identical(cm$n, nrow(d$test))

  p <- predict(m, newdata = d$test)[["1"]]
  expect_identical(cm$diag,
                   sum((p > 0.5) == (d$test$admit == 1L)))

})


test_that("multiclass and other engines work, regression does not", {

  cm <- confusion(fitMod(Species ~ ., iris, engine = "multinom"))
  expect_identical(dim(cm$table), c(3L, 3L))
  expect_null(cm$pos)

  skip_if_not_installed("rpart")
  tree <- fitMod(Species ~ ., iris, engine = "rpart")
  expect_equal(confusion(tree)$acc,
               mean(predict(tree, output = "class")$class == iris$Species))

  expect_error(confusion(fitMod(Fertility ~ ., swiss, engine = "lm")),
               "needs a classification model")

})


test_that("Firth models are covered", {

  skip_if_not_installed("brglm2")

  f <- fitMod(admit ~ gre + gpa + rank, Admit, engine = "logitFirth")
  p <- predict(f)[["1"]]
  expect_identical(confusion(f)$diag,
                   sum((p > 0.5) == (Admit$admit == 1L)))

})


# -- cStat() -------------------------------------------------------------------

test_that("a logit model gives the same C-statistic as the glm method", {

  g <- glm(admit ~ gre + gpa + rank, binomial, Admit)
  expect_equal(cStat(fit), cStat(g))

  # the AUC of the fitted probabilities
  p <- fitted(g)
  y <- Admit$admit
  auc <- mean(outer(p[y == 1], p[y == 0], ">") +
                0.5 * outer(p[y == 1], p[y == 0], "=="))
  expect_equal(cStat(fit), auc)

})


test_that("arguments reach DescToolsX::cStat()", {

  set.seed(1)
  ci <- cStat(fit, conf.level = 0.95, R = 200)
  expect_named(ci, c("est", "lci", "uci"))
  expect_equal(ci[["est"]], cStat(fit))
  expect_true(ci[["lci"]] < ci[["est"]] && ci[["est"]] < ci[["uci"]])

})


test_that("new data are evaluated against their response", {

  set.seed(1)
  d <- splitTrainTest(Admit, p = 0.3)
  m <- update(fit, data = d$train)

  p <- predict(m, newdata = d$test)[["1"]]
  expect_equal(cStat(m, newdata = d$test), cStat(p, ref = d$test$admit))

})


test_that("the positive class follows the factor levels of the response", {

  d <- Admit
  d$adm <- factor(ifelse(d$admit == 1, "yes", "no"), levels = c("no", "yes"))
  f <- fitMod(adm ~ gre + gpa + rank, d, engine = "logit")
  expect_equal(cStat(f), cStat(fit))

})


test_that("Firth, other engines, and non-binary models", {

  skip_if_not_installed("brglm2")
  ff <- fitMod(admit ~ gre + gpa + rank, Admit, engine = "logitFirth")
  expect_equal(cStat(ff), cStat(predict(ff)[["1"]], ref = Admit$admit))

  expect_error(cStat(fitMod(Species ~ ., iris, engine = "multinom")),
               "needs a binary model")
  expect_error(cStat(fitMod(Fertility ~ ., swiss, engine = "lm")),
               "needs a classification model")

})


# -- brierScore(), averagePrecision(), logLoss() --------------------------------

test_that("the FitMod methods agree with the glm methods and the vectors", {

  p <- fitted(glmFit)
  y <- Admit$admit

  expect_equal(brierScore(fit), brierScore(p, ref = y))
  expect_equal(brierScore(fit), brierScore(glmFit))
  expect_equal(brierScore(fit, scaled = TRUE),
               brierScore(p, ref = y, scaled = TRUE))

  expect_equal(averagePrecision(fit), averagePrecision(p, ref = y))
  expect_equal(averagePrecision(fit), averagePrecision(glmFit))

  expect_equal(logLoss(fit), logLoss(p, ref = y))
  expect_equal(logLoss(fit), logLoss(glmFit))

})


test_that("the FitMod methods evaluate new data", {

  set.seed(1)
  d <- splitTrainTest(Admit, p = 0.3)
  m <- update(fit, data = d$train)
  p <- predict(m, newdata = d$test)[["1"]]

  expect_equal(brierScore(m, newdata = d$test), brierScore(p, ref = d$test$admit))
  expect_equal(averagePrecision(m, newdata = d$test),
               averagePrecision(p, ref = d$test$admit))
  expect_equal(logLoss(m, newdata = d$test), logLoss(p, ref = d$test$admit))

})


test_that("the binary metrics refuse multiclass and regression models", {

  multi <- fitMod(Species ~ ., iris, engine = "multinom")
  for (f in list(brierScore, averagePrecision, logLoss))
    expect_error(f(multi), "needs a binary model")

  expect_error(brierScore(glm(mpg ~ wt, data = mtcars)),
               "needs a binomial model")

})


# -- methods moved from DescToolsX --------------------------------------------

test_that("confusion.glm agrees with explicit cutoff predictions", {
  dat <- data.frame(z = 1:12, y = c(0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1))
  fit <- stats::glm(y ~ z, data = dat, family = stats::binomial())
  for (cutoff in c(0.4, 0.5, 0.6)) {
    pred <- ifelse(stats::predict(fit, type = "response") > cutoff, "1", "0")
    expect_identical(confusion(fit, cutoff = cutoff), confusion(pred, dat$y, pos = "1"))
  }
  dat$y <- factor(dat$y, labels = c("no", "yes"))
  fit <- stats::glm(y ~ z, data = dat, family = stats::binomial())
  pred <- ifelse(stats::predict(fit, type = "response") > 0.5, "yes", "no")
  expect_identical(confusion(fit, pos = "no"), confusion(pred, dat$y, pos = "no"))
  fit$model <- NULL
  expect_error(confusion(fit), "model frame")
  fit <- stats::glm(Sepal.Length ~ Sepal.Width, data = iris)
  expect_error(confusion(fit), "binary response")
})

test_that("confusion.rpart agrees with fitted classification predictions", {
  skip_if_not_installed("rpart")
  fit <- rpart::rpart(Species ~ ., data = iris, method = "class", y = TRUE)
  expect_identical(confusion(fit), confusion(stats::predict(fit, type = "class"), iris$Species))
})

test_that("confusion.multinom requires a model frame and uses fitted classes", {
  skip_if_not_installed("nnet")
  fit <- nnet::multinom(Species ~ ., data = iris, model = TRUE,
                        trace = FALSE, maxit = 300)
  expect_identical(confusion(fit), confusion(stats::predict(fit, type = "class"), iris$Species))
  fit$model <- NULL
  expect_error(confusion(fit), "model frame")
})

test_that("confusion.randomForest uses out-of-bag predictions", {
  skip_if_not_installed("randomForest")
  set.seed(6401)
  fit <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50)
  expect_identical(confusion(fit), confusion(fit$predicted, fit$y))
})

test_that("the glm method uses fitted probabilities and the response", {
  fit <- glm(vs ~ mpg, data = mtcars, family = binomial)
  expect_equal(cStat(fit), cStat(fitted(fit), ref = mtcars$vs))
  # without the stored response the model frame is used
  fit0 <- glm(vs ~ mpg, data = mtcars, family = binomial, y = FALSE)
  expect_equal(cStat(fit0), cStat(fit))
})

test_that("the glm method passes sides on", {
  fit <- glm(vs ~ mpg, data = mtcars, family = binomial)
  set.seed(2)
  res <- cStat(fit, conf.level = 0.95, sides = "right", R = 300)
  expect_equal(unname(res[["lci"]]), 0)
})

test_that("the glm method takes fitted probabilities and the response", {
  fit <- glm(vs ~ mpg, data = mtcars, family = binomial)
  expect_equal(brierScore(fit), brierScore(fitted(fit), mtcars$vs))
  fit0 <- glm(vs ~ mpg, data = mtcars, family = binomial, y = FALSE)
  expect_equal(brierScore(fit0), brierScore(fit))
})

test_that("averagePrecision works with glm objects", {
  dat <- data.frame(
    # overlapping classes: with complete separation glm() warns that the
    # fitted probabilities are numerically 0 or 1
    y = c(0, 0, 1, 0, 1, 1),
    x = c(-2, -1, 0, 0.5, 1, 2)
  )
  fit <- glm(y ~ x, data = dat, family = binomial)

  expect_equal(averagePrecision(fit),
               averagePrecision(predict(fit, type = "response"), fit$y),
               tolerance = 1e-12)
})

test_that("logLoss accepts a fitted glm", {

  m <- glm(am ~ hp + wt, data = mtcars, family = binomial)

  expect_equal(
    logLoss(m),
    logLoss(predict(m, type = "response"), m$y)
  )

  # for ungrouped Bernoulli data the saturated log-likelihood is zero,
  # hence deviance == 2 * n * logLoss -- an independent reference
  expect_equal(logLoss(m), m$deviance / (2 * nrow(mtcars)))
})

test_that("logLoss handles na.exclude in fitted glm", {

  # regression test: predict() pads its result via napredict() under
  # na.exclude, $fitted.values does not and stays aligned with $y
  d <- mtcars
  d$hp[c(2, 5)] <- NA

  m <- glm(
    am ~ hp + wt,
    data = d,
    family = binomial,
    na.action = na.exclude
  )

  expect_length(m$y, nrow(mtcars) - 2L)

  expect_equal(
    logLoss(m),
    m$deviance / (2 * length(m$y))
  )
})

test_that("logLoss rejects unsupported models", {

  # an lm has no method: the default method wants the outcomes
  expect_error(
    logLoss(lm(mpg ~ wt, data = mtcars)),
    "'ref' must be provided",
    fixed = TRUE
  )

  expect_error(
    logLoss(glm(mpg ~ wt, data = mtcars)),
    "got \"gaussian\"",
    fixed = TRUE
  )

  # quasibinomial has no full likelihood, hence no log loss
  expect_error(
    logLoss(glm(am ~ wt, data = mtcars, family = quasibinomial)),
    "got \"quasibinomial\"",
    fixed = TRUE
  )

  grp <- data.frame(succ = c(3, 7), fail = c(7, 3), x = c(0, 1))

  expect_error(
    logLoss(glm(cbind(succ, fail) ~ x, data = grp, family = binomial)),
    "prior weights",
    fixed = TRUE
  )
})
