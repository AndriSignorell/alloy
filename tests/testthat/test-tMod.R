
# tests for tMod() with fitMod() and lme4 objects --------------------------------

test_that("FitMod objects give the same table as the underlying fit", {

  f1 <- fitMod(am ~ wt, data = mtcars, engine = "logit")
  f2 <- fitMod(am ~ wt + hp, data = mtcars, engine = "logit")
  g1 <- glm(am ~ wt, data = mtcars, family = binomial)
  g2 <- glm(am ~ wt + hp, data = mtcars, family = binomial)

  a <- suppressWarnings(tMod(f1, f2))
  b <- suppressWarnings(tMod(g1, g2))

  expect_s3_class(a, "TMod")
  expect_identical(colnames(a[[1]]), c("coef", "f1", "f2"))
  expect_equal(unname(a[[1]][-1]), unname(b[[1]][-1]))
  expect_equal(unname(a[[2]][-1]), unname(b[[2]][-1]))
  expect_equal(unname(a$mall), unname(b$mall))

  # mixed with a plain model, and the summary method called directly
  expect_silent(tMod(fitMod(mpg ~ wt, mtcars, engine = "lm"),
                     lm(mpg ~ wt + hp, mtcars)))
  expect_equal(tModSummary(f1, useProfile = FALSE),
               tModSummary(g1, useProfile = FALSE))

})


test_that("lme4 models are summarised, also when fitted with fitMod()", {

  skip_if_not_installed("lme4")

  l1 <- fitMod(Reaction ~ Days + (1 | Subject), lme4::sleepstudy,
               engine = "lmMixed")
  l2 <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)

  tm <- tMod(l1, l2)
  st <- tm[[2]]

  expect_identical(tm[[1]]$coef, c("(Intercept)", "Days"))
  expect_equal(st$l1[st$stat == "N"], 180)
  expect_equal(st$l2[st$stat == "NAs"], 0)
  expect_equal(st$l1[st$stat == "n vars"], 1)

})
