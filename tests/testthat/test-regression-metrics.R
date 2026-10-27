
# tests for the prediction errors of fitted regression models --------------------

test_that("the FitMod methods agree with the lm methods and the vectors", {

  fit <- fitMod(mpg ~ hp + wt, mtcars, engine = "lm")
  l   <- lm(mpg ~ hp + wt, mtcars)
  p   <- fitted(l)
  y   <- mtcars$mpg

  expect_equal(mae(fit),   mean(abs(y - p)))
  expect_equal(mse(fit),   mean((y - p)^2))
  expect_equal(rmse(fit),  sqrt(mean(residuals(l)^2)))
  expect_equal(mape(fit),  mean(abs((y - p) / y)))
  expect_equal(smape(fit), smape(unname(p), ref = y))

  for (f in list(mae, mse, rmse, mape, smape))
    expect_equal(f(fit), f(l))

})


test_that("mape divides by the observed, not by the predicted values", {

  fit <- fitMod(mpg ~ hp + wt, mtcars, engine = "lm")
  p <- unname(fitted(fit))
  expect_equal(mape(fit), mean(abs((mtcars$mpg - p) / mtcars$mpg)))
  expect_false(isTRUE(all.equal(mape(fit),
                                mean(abs((mtcars$mpg - p) / p)))))

})


test_that("the FitMod methods evaluate new data and other engines", {

  fit <- fitMod(mpg ~ hp + wt, mtcars, engine = "lm")
  set.seed(1)
  d <- splitTrainTest(mtcars, p = 0.3)
  m <- update(fit, data = d$train)
  p <- predict(m, newdata = d$test)

  expect_equal(rmse(m, newdata = d$test), sqrt(mean((d$test$mpg - p)^2)))

  pois <- fitMod(art ~ fem + ment, BioChemists, engine = "poisson")
  expect_equal(mae(pois), mean(abs(BioChemists$art - fitted(pois))))

  expect_error(rmse(fitMod(admit ~ gre, Admit, engine = "logit")),
               "needs a regression model")

})


test_that("missing values are left out consistently (na.exclude)", {

  d <- mtcars
  d$hp[c(3, 8)] <- NA
  l <- lm(mpg ~ hp + wt, d, na.action = na.exclude)
  ok <- !is.na(d$hp)
  expect_equal(rmse(l), sqrt(mean(residuals(l)[ok]^2)))

})


# -- methods moved from DescToolsX --------------------------------------------

test_that("mae.lm returns a non-negative numeric for a linear model", {
  fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  result <- mae(fit)
  expect_gte(result, 0)
  expect_length(result, 1)
})

test_that("mse.lm returns a non-negative numeric", {
  fit <- lm(mpg ~ hp, data = mtcars)
  res <- mse(fit)
  expect_gte(res, 0); expect_length(res, 1)
})

test_that("rmse.lm returns a non-negative numeric", {
  fit <- lm(mpg ~ hp, data = mtcars)
  res <- rmse(fit)
  expect_gte(res, 0); expect_length(res, 1)
})

test_that("mape.lm returns a non-negative numeric for a linear model", {
  fit <- lm(mpg ~ hp, data = mtcars)
  res <- mape(fit)
  expect_gte(res, 0)
  expect_length(res, 1)
})

test_that("smape.lm returns a non-negative numeric", {
  fit <- lm(mpg ~ hp, data = mtcars)
  res <- smape(fit)
  expect_gte(res, 0); expect_length(res, 1)
})

test_that("the lm methods refuse a non-numeric response", {
  
  fit <- lm(mpg ~ hp, data = mtcars)
  expect_equal(rmse(fit),
               sqrt(mean(residuals(fit)^2)))
  
  # factor response without (quasi-)separation; I(mpg > 20) ~ hp gave
  # fitted probabilities numerically 0 or 1
  gfit <- glm(factor(vs) ~ mpg, data = mtcars, family = binomial)
  # a factor or matrix response reached the arithmetic unchecked before
  expect_error(rmse(gfit), "numeric vector")
  expect_error(smape(gfit), "numeric vector")
})
