# Tests for print.FitMod(): header rows of factor predictors, confidence
# level, and the cases that used to stop the print

headerRow <- function(out, var) grep(sprintf("^%s +\\(ref: ", var), out,
                                     value = TRUE)


test_that("a factor gets a header row with its reference and LR test", {
  fit <- fitMod(Sepal.Length ~ Species + Petal.Width, data = iris,
                engine = "lm")
  out <- capture.output(print(fit))
  hdr <- headerRow(out, "Species")

  expect_length(hdr, 1L)
  expect_true(grepl("(ref: setosa)", hdr, fixed = TRUE))
  expect_true(grepl(
    pharos::fm(drop1(fit, test = "F")["Species", "Pr(>F)"], fmt = "p",
               pThreshold = 1e-3, digits = 3),
    hdr, fixed = TRUE
  ))
  expect_identical(sum(grepl("^Species (versicolor|virginica) ", out)), 2L)
})


test_that("factors without a treatment coding do not stop the print", {
  d <- iris
  d$ord <- cut(d$Sepal.Width, 3, ordered_result = TRUE)

  out <- capture.output(print(fitMod(Sepal.Length ~ ord + Species, data = d,
                                     engine = "lm")))
  expect_length(headerRow(out, "ord"), 0L)
  expect_length(headerRow(out, "Species"), 1L)
  expect_true(any(grepl("^ord\\.L ", out)))

  op <- options(contrasts = c("contr.sum", "contr.poly"))
  on.exit(options(op))
  out <- capture.output(print(fitMod(Sepal.Length ~ Species, data = iris,
                                     engine = "lm")))
  expect_length(headerRow(out, "Species"), 0L)
  expect_true(any(grepl("^Species1 ", out)))
})


test_that("lmrob prints factors with a robust Wald test", {
  skip_if_not_installed("robustbase")

  fit <- fitMod(Sepal.Length ~ Species + Petal.Width, data = iris,
                engine = "lmrob")
  hdr <- headerRow(capture.output(print(fit)), "Species")

  # reference: the Wald test of anova.lmrob()
  full <- robustbase::lmrob(Sepal.Length ~ Species + Petal.Width,
                            data = iris)
  ref  <- anova(full, Sepal.Length ~ Petal.Width, test = "Wald")
  expect_equal(.wald_terms(fit, "Species"), ref[2L, "Pr(>chisq)"])

  expect_length(hdr, 1L)
  expect_true(grepl(pharos::fm(ref[2L, "Pr(>chisq)"], fmt = "p",
                               pThreshold = 1e-3, digits = 3),
                    hdr, fixed = TRUE))
})


test_that("mixed models print factors with a header row", {
  skip_if_not_installed("lme4")

  d <- lme4::sleepstudy
  d$grp <- factor(rep(c("a", "b", "c"), length.out = nrow(d)))
  fit <- fitMod(Reaction ~ Days + grp + (1 | Subject), data = d,
                engine = "lmMixed")
  hdr <- headerRow(capture.output(print(fit)), "grp")

  # lme4 names the p-value column of drop1() "Pr(Chi)"
  lrt <- drop1(fit$model, test = "Chisq")
  expect_length(hdr, 1L)
  expect_true(grepl(pharos::fm(lrt["grp", "Pr(Chi)"], fmt = "p",
                               pThreshold = 1e-3, digits = 3),
                    hdr, fixed = TRUE))
})


test_that("survival models keep their header rows", {
  d <- Whas100
  d$bmiGrp <- cut(d$bmi, c(0, 25, 30, 100),
                  labels = c("normal", "over", "obese"))

  for (engine in c("coxph", "weibull")) {
    fit <- fitMod(survival::Surv(foltime, folstatus) ~ bmiGrp + age,
                  data = d, engine = engine)
    hdr <- headerRow(capture.output(print(fit)), "bmiGrp")

    expect_length(hdr, 1L)
    expect_true(grepl("(ref: normal)", hdr, fixed = TRUE))
    expect_true(grepl(
      pharos::fm(drop1(fit, test = "Chisq")["bmiGrp", "Pr(>Chi)"],
                 fmt = "p", pThreshold = 1e-3, digits = 3),
      hdr, fixed = TRUE
    ))
  }
})


test_that("polr intervals follow conf.level", {
  fit <- fitMod(apply ~ pared + public + gpa, data = Ologit,
                engine = "polr")

  for (level in c(0.8, 0.95)) {
    xx <- .summary_polr(fit, conf.level = level)
    ci <- confint.default(fit, level = level)

    # the lower limit is the third column, named after the level
    expect_equal(xx$coefficients[[3L]], unname(ci[, 1L]))
    expect_equal(xx$coefficients$uci, unname(ci[, 2L]))
  }

  out <- capture.output(print(fit, conf.level = 0.8, digits = 4))
  expect_true(any(grepl("80%-lci", out, fixed = TRUE)))
  expect_true(grepl(
    pharos::fm(confint.default(fit, level = 0.8)["gpa", 1L], digits = 4),
    grep("^ *gpa ", out, value = TRUE), fixed = TRUE
  ))

  # odds ratios are the exponentiated limits at the same level
  xx <- .summary_polr(fit, conf.level = 0.8, output = "or")
  expect_equal(xx$coefficients$uci,
               unname(exp(confint.default(fit, level = 0.8)[, 2L])))
})
