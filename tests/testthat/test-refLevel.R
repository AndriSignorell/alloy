# Tests for refLevel() and its internal, tolerant variant .refLevels()

test_that("the reference level follows the contrasts actually used", {
  expect_identical(refLevel(lm(Sepal.Length ~ Species, data = iris)),
                   c(Species = "setosa"))

  d <- iris
  contrasts(d$Species) <- contr.treatment(3, base = 2)
  expect_identical(refLevel(lm(Sepal.Length ~ Species, data = d)),
                   c(Species = "versicolor"))

  expect_length(refLevel(lm(Sepal.Length ~ Petal.Width, data = iris)), 0L)
  expect_error(refLevel(1), "must be a model object")
})


test_that("codings without a reference level are rejected", {
  d <- iris
  d$sum <- d$Species
  contrasts(d$sum) <- contr.sum(3)
  d$hel <- d$Species
  contrasts(d$hel) <- contr.helmert(3)
  d$ord <- cut(d$Sepal.Width, 3, ordered_result = TRUE)

  # a sum coding stored as a matrix has a row without a 1 as well: the
  # last level must not be taken for a reference
  expect_error(refLevel(lm(Sepal.Length ~ sum, data = d)),
               "cannot determine reference level")
  expect_error(refLevel(lm(Sepal.Length ~ hel, data = d)),
               "cannot determine reference level")
  expect_error(refLevel(lm(Sepal.Length ~ ord, data = d)),
               "unsupported contrast 'contr.poly'")
})


test_that(".refLevels(strict = FALSE) leaves such factors out", {
  d <- iris
  d$sum <- d$Species
  contrasts(d$sum) <- contr.sum(3)
  d$ord <- cut(d$Sepal.Width, 3, ordered_result = TRUE)
  d$big <- factor(d$Petal.Width > 1, labels = c("no", "yes"))

  fit <- lm(Sepal.Length ~ sum + ord + big, data = d)
  expect_identical(.refLevels(fit, strict = FALSE), c(big = "no"))
})


test_that("glm and survival fits are covered", {
  expect_identical(
    refLevel(glm(am ~ cyl + wt, data = transform(mtcars, cyl = factor(cyl)),
                 family = binomial)),
    c(cyl = "4")
  )

  d <- Whas100
  d$bmiGrp <- cut(d$bmi, c(0, 25, 30, 100),
                  labels = c("normal", "over", "obese"))
  f <- survival::Surv(foltime, folstatus) ~ bmiGrp + age

  expect_identical(refLevel(survival::coxph(f, data = d)),
                   c(bmiGrp = "normal"))
  # survreg keeps no model frame, the levels come from xlevels
  expect_identical(refLevel(survival::survreg(f, data = d)),
                   c(bmiGrp = "normal"))
})


test_that("mixed models are covered, plain and fitted via fitMod()", {
  skip_if_not_installed("lme4")

  d <- lme4::sleepstudy
  d$grp <- factor(rep(c("a", "b", "c"), length.out = nrow(d)))

  expect_identical(
    refLevel(lme4::lmer(Reaction ~ Days + grp + (1 | Subject), data = d)),
    c(grp = "a")
  )
  expect_identical(
    refLevel(fitMod(Reaction ~ Days + grp + (1 | Subject), data = d,
                    engine = "lmMixed")),
    c(grp = "a")
  )
})
