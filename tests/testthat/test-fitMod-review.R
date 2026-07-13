# ==========================================================================
# test-fitMod-review.R
#
# Regression tests from the July 2026 code review of fitMod(),
# predict.FitMod() and .drop1.polr().
#
# Convention: tests marked [BUG] document issues found in the review.
# They fail on the pre-review code base and turn green once the
# corresponding fix is applied.
# ==========================================================================


# --- helper data ----------------------------------------------------------

set.seed(42)

d_num <- data.frame(
  y  = rnorm(100),
  x1 = rnorm(100),
  x2 = factor(sample(c("a", "b", "c"), 100, replace = TRUE))
)

# binary response with signal (so that lambda.min / lambda.1se separate)
d_bin   <- d_num
d_bin$y <- factor(ifelse(rbinom(100, 1, plogis(2 * d_num$x1)) == 1,
                         "yes", "no"),
                  levels = c("no", "yes"))

# count response (integer -> auto-detect poisson)
d_cnt   <- d_num
d_cnt$y <- rpois(100, lambda = exp(0.3 * d_num$x1 + 0.5))


# ==========================================================================
# fitMod: auto-detection & validation
# ==========================================================================

test_that("auto-detection picks fitfn from the response type", {
  expect_message(m <- fitMod(y ~ x1, d_num), "lm")
  expect_s3_class(m, "FitMod")
  expect_identical(m$fitfn, "lm")

  expect_message(m <- fitMod(y ~ x1, d_bin), "logit")
  expect_identical(m$fitfn, "logit")

  expect_message(m <- fitMod(y ~ x1, d_cnt), "poisson")
  expect_identical(m$fitfn, "poisson")

  skip_if_not_installed("nnet")
  expect_message(m <- fitMod(Species ~ ., iris), "multinom")
  expect_identical(m$fitfn, "multinom")
})

test_that("unknown fitfn gives an informative error", {
  # fitMod() uses match.arg() against the registry names
  expect_error(fitMod(y ~ x1, d_num, fitfn = "gurkensalat"),
               "should be one of")
})

test_that("[BUG] NAs in the response do not break auto-detection", {
  # isDichotomous() returns NA on anyNA() by default -> if (NA) crashes.
  # Fix: na.rm = TRUE in fitMod.
  d <- d_num
  d$y[1] <- NA
  expect_no_error(suppressMessages(fitMod(y ~ x1, d)))
})

test_that("[BUG] one-sided formulas are rejected cleanly", {
  # formula[[2]] of ~x is the RHS -> auto-detection would guess on the
  # predictors. Fix: check length(formula) == 3L.
  expect_error(suppressMessages(fitMod(~ x1, d_num)),
               regexp = "formula|Response|response")
})

test_that("naiveBayes registry key works (docs used to say 'naive_bayes')", {
  skip_if_not_installed("naivebayes")
  expect_no_error(fitMod(y ~ x1 + x2, d_bin, fitfn = "naiveBayes"))
})


# ==========================================================================
# fitMod: glmnet / xgboost special paths
# ==========================================================================

test_that("[BUG] glmnet/xgboost: plain call without subset/na.action works", {
  # cl is a call, not a list: assigning NULL to an *absent* component via
  # [[<- throws "subscript out of bounds", and subset/na.action are absent
  # in any ordinary call (match.call() only captures supplied arguments).
  # This is the exact scenario of the vignette examples.
  skip_if_not_installed("glmnet")
  expect_no_error(
    suppressMessages(fitMod(y ~ x1 + x2, d_bin, fitfn = "glmnet"))
  )
})

test_that("[BUG] glmnet: formula without intercept keeps all predictors", {
  skip_if_not_installed("glmnet")
  # x2 has 3 levels -> without intercept: 3 dummies + x1 = 4 columns.
  # The unconditional [, -1L] drop used to discard the first dummy.
  m <- suppressMessages(fitMod(y ~ x1 + x2 - 1, d_num, fitfn = "glmnet"))
  expect_identical(ncol(m$x_train), 4L)
})

test_that("[BUG] glmnet: subset is honoured", {
  skip_if_not_installed("glmnet")
  m <- suppressMessages(
    fitMod(y ~ x1 + x2, d_num, fitfn = "glmnet", subset = 1:50)
  )
  expect_identical(nrow(m$x_train), 50L)
})


# ==========================================================================
# predict.FitMod
# ==========================================================================

test_that("[BUG] GLM: same scale with and without newdata", {
  # without newdata: fitted() (response scale)
  # with newdata:    predict.glm default (link scale)  -> inconsistent
  m  <- fitMod(y ~ x1, d_cnt, fitfn = "poisson")
  p0 <- predict(m)
  p1 <- predict(m, newdata = d_cnt)
  expect_equal(unname(p0), unname(p1), tolerance = 1e-8)
})

test_that("[BUG] logit: prob columns carry the factor levels", {
  # The docs promise "column names match the factor levels", but the
  # prob branch used to hardcode "0"/"1".
  m <- fitMod(y ~ x1, d_bin, fitfn = "logit")
  p <- predict(m)
  expect_identical(colnames(p), levels(d_bin$y))
})

test_that("classification probs: rows sum to 1, columns = levels", {
  skip_if_not_installed("nnet")
  m <- suppressMessages(fitMod(Species ~ ., iris))
  p <- predict(m)
  expect_identical(colnames(p), levels(iris$Species))
  expect_equal(unname(rowSums(p)), rep(1, nrow(iris)), tolerance = 1e-6)
})

test_that("[BUG] binary multinom returns 2 prob columns", {
  # predict(multinom, type = "probs") returns a vector for 2 classes
  skip_if_not_installed("nnet")
  m <- fitMod(y ~ x1, d_bin, fitfn = "multinom")
  p <- predict(m)
  expect_identical(ncol(p), 2L)
  expect_identical(colnames(p), levels(d_bin$y))
})

test_that("[BUG] output = 'where' outside rpart does not return NULL silently", {
  m <- fitMod(y ~ x1, d_bin, fitfn = "logit")
  expect_error(predict(m, output = "where"))
})

test_that("[BUG] rpart regression tree is predictable", {
  # is_reg used to depend on fitfn alone -> anova trees ended up in the
  # classification branch (type = "prob" -> error)
  skip_if_not_installed("rpart")
  m <- fitMod(y ~ x1 + x2, d_num, fitfn = "rpart")
  expect_no_error(p <- predict(m))
  expect_true(is.numeric(unlist(p)))
})

test_that("[BUG] xgboost regression returns no pseudo-probabilities", {
  skip_if_not_installed("xgboost")
  m <- suppressMessages(fitMod(y ~ x1 + x2, d_num, fitfn = "xgboost"))
  p <- predict(m)
  expect_false(identical(colnames(p), c("0", "1")))
})

test_that("[BUG] glmnet: newdata with a subset of the factor levels", {
  # model.matrix() on newdata alone -> missing levels = missing dummy
  # columns -> column mismatch. Fix: store terms + xlev at fit time.
  skip_if_not_installed("glmnet")
  m  <- suppressMessages(fitMod(y ~ x1 + x2, d_bin, fitfn = "glmnet"))
  nd <- d_bin[d_bin$x2 == "a", ][1:5, ]
  nd$x2 <- droplevels(nd$x2)
  expect_no_error(p <- predict(m, newdata = nd))
  expect_identical(NROW(p), 5L)
})

test_that("[BUG] glmnet: 's' is passed through", {
  skip_if_not_installed("glmnet")
  m <- suppressMessages(fitMod(y ~ x1 + x2, d_bin, fitfn = "glmnet"))

  # reference directly via predict.cv.glmnet with lambda.min
  obj <- m
  class(obj) <- setdiff(class(obj), "FitMod")
  ref <- predict(obj, newx = m$x_train, s = "lambda.min",
                 type = "response")

  p_min <- predict(m, s = "lambda.min")
  expect_equal(unname(p_min[[2L]]), unname(as.numeric(ref)),
               tolerance = 1e-8)
})

test_that("[BUG] tobit: predict() does not return NULL", {
  # survreg objects have no $fitted.values -> fitted() returns NULL;
  # tobit used to sit in the is_reg branch with a fitted() shortcut
  skip_if_not_installed("AER")
  d   <- d_num
  d$y <- pmax(d$y, 0)
  m <- fitMod(y ~ x1 + x2, d, fitfn = "tobit")
  expect_false(is.null(predict(m)))
  expect_identical(length(predict(m)), nrow(d))
})


# ==========================================================================
# update() / call repair
# ==========================================================================

test_that("[BUG] update() works without the fitting package attached", {
  # The un-namespaced fix_call head ('glm.nb') used to require MASS to be
  # attached. Fix: namespaced call head MASS::glm.nb.
  skip_if_not_installed("MASS")
  # negbin needs *overdispersed* counts: on pure Poisson data (d_cnt)
  # theta diverges and theta.ml() warns 'iteration limit reached'.
  # Local seed: earlier tests consume RNG state (multinom start weights,
  # glmnet CV folds), depending on which suggested packages are installed.
  set.seed(4711)
  d_nb   <- d_num
  d_nb$y <- rnbinom(100, mu = exp(0.3 * d_num$x1 + 0.5), size = 1.5)
  expect_no_warning(m <- fitMod(y ~ x1 + x2, d_nb, fitfn = "negbin"))
  expect_no_error(update(m, . ~ . - x2))
})


# ==========================================================================
# .drop1.polr
# ==========================================================================

test_that("[BUG] .drop1.polr respects the method of the full model", {
  # Two bugs in one: (1) the reduced fits used to be refitted with the
  # default method = "logistic" regardless of the full model; (2) passing
  # 'weights' as a plain symbol made polr's model.frame() resolve it in
  # environment(formula), falling through to the *function* stats::weights
  # -> every refit failed and tryCatch silently produced all-NA tables.
  skip_if_not_installed("MASS")
  d   <- d_num
  d$y <- cut(d$x1 + rnorm(100), 3,
             labels = c("low", "mid", "high"),
             ordered_result = TRUE)

  m <- fitMod(y ~ x1 + x2, d, fitfn = "polr", method = "probit")

  # reference: LR test consistently with probit
  full <- MASS::polr(y ~ x1 + x2, data = d, method = "probit", Hess = TRUE)
  red  <- MASS::polr(y ~ x2,      data = d, method = "probit", Hess = TRUE)
  lr_ref <- deviance(red) - deviance(full)

  expect_equal(m$drop1["x1", "LR Chisq"], lr_ref, tolerance = 1e-6)
})

test_that("[BUG] .drop1.polr does not silently discard offsets", {
  # Offsets are NOT part of the model matrix; the reduced fits would run
  # without the offset -> invalid LR comparison. After the fix: stop().
  skip_if_not_installed("MASS")
  d   <- d_num
  d$y <- cut(d$x1 + rnorm(100), 3, ordered_result = TRUE)
  d$o <- runif(100)
  expect_error(
    fitMod(y ~ x1 + x2 + offset(o), d, fitfn = "polr"),
    regexp = "offset"
  )
})
