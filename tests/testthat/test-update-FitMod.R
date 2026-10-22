
# tests for update.FitMod() ----------------------------------------------------

fit <- fitMod(admit ~ gre + gpa + rank, Admit, engine = "logit")


test_that("update() refits through fitMod() and keeps the class", {

  up <- update(fit, . ~ . - gre)

  expect_s3_class(up, c("FitMod", "glm"))
  expect_identical(up$engine, "logit")
  expect_identical(attr(terms(up), "term.labels"), c("gpa", "rank"))
  expect_equal(coef(up),
               coef(glm(admit ~ gpa + rank, binomial, Admit)))

  # without changes the model is reproduced
  expect_equal(coef(update(fit)), coef(fit))

  # the call can be returned instead
  cl <- update(fit, . ~ . - gre, evaluate = FALSE)
  expect_identical(cl[[1L]], quote(alloy::fitMod))
  expect_identical(cl$engine, "logit")

})


test_that("arguments in ... replace or extend the original call", {

  d <- Admit
  d$w <- rep(c(1, 2), 200)

  up <- update(fit, data = d, weights = w)
  expect_equal(coef(up),
               coef(suppressWarnings(glm(admit ~ gre + gpa + rank,
                                         binomial, d, weights = w))))

  up <- update(fit, subset = gpa > 3)
  expect_identical(nobs(up), sum(Admit$gpa > 3))

})


test_that("the engine can be changed, its registry defaults follow", {

  skip_if_not_installed("brglm2")

  firth <- update(fit, engine = "logitFirth")
  expect_s3_class(firth, c("FitMod", "logitFirth", "brglmFit"))
  expect_identical(firth$engine, "logitFirth")

  back <- update(firth, engine = "logit")
  expect_false(inherits(back, "brglmFit"))
  expect_false(inherits(back, "logitFirth"))
  expect_equal(coef(back), coef(fit))

  # the subclass survives a formula update
  up <- update(firth, . ~ . - gre)
  expect_s3_class(up, "logitFirth")

})


test_that("other engines keep their class as well", {

  up <- update(fitMod(Fertility ~ ., swiss, engine = "lm"), . ~ . - Examination)
  expect_s3_class(up, c("FitMod", "lm"))
  expect_identical(up$engine, "lm")

  skip_if_not_installed("lme4")
  m <- fitMod(Reaction ~ Days + (1 | Subject), lme4::sleepstudy,
              engine = "lmMixed")
  up <- update(m, . ~ . + (0 + Days | Subject))
  expect_s3_class(up, "FitMod.lme4")
  expect_length(lme4::VarCorr(up$model), 2L)

})
