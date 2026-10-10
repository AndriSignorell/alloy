# Tests for the beta regression engine of fitMod() (engine = "beta")

skip_if_not_installed("betareg")

data("GasolineYield",   package = "betareg")
data("FoodExpenditure", package = "betareg")
data("ReadingSkills",   package = "betareg")

# reference for the term tests: explicit refit and likelihood-ratio test
lrRef <- function(full, reduced) {
  lr <- 2 * (as.numeric(logLik(full)) - as.numeric(logLik(reduced)))
  df <- attr(logLik(full), "df") - attr(logLik(reduced), "df")
  c(lr, pchisq(lr, df, lower.tail = FALSE))
}

lrCols <- c("LR stat.", "p-value")


test_that("engine = 'beta' reproduces betareg()", {
  fit <- fitMod(yield ~ gravity + temp, data = GasolineYield,
                engine = "beta")
  ref <- betareg::betareg(yield ~ gravity + temp, data = GasolineYield)

  expect_s3_class(fit, c("FitMod", "betareg"), exact = TRUE)
  expect_identical(fit$engine, "beta")
  expect_equal(coef(fit), coef(ref))
  expect_equal(as.numeric(logLik(fit)), as.numeric(logLik(ref)))
})


test_that("a two-part formula fits a precision model", {
  fit <- fitMod(yield ~ gravity + temp | temp, data = GasolineYield,
                engine = "beta")
  expect_named(coef(fit), c("(Intercept)", "gravity", "temp",
                            "(phi)_(Intercept)", "(phi)_temp"))
})


test_that("beta regression is never chosen automatically", {
  expect_message(
    fit <- fitMod(yield ~ gravity + temp, data = GasolineYield),
    "engine = 'lm'"
  )
  expect_s3_class(fit, "lm")
})


test_that("a response outside [0, 1] is rejected", {
  d <- GasolineYield
  d$yield[1L] <- 1.2
  expect_error(fitMod(yield ~ gravity, data = d, engine = "beta"),
               "\\[0, 1\\]")
})


test_that("subset, weights and na.action are passed on", {
  d <- GasolineYield
  d$w <- rep(1:2, length.out = nrow(d))
  d$temp[3L] <- NA

  fit <- fitMod(yield ~ gravity + temp, data = d, engine = "beta",
                subset = gravity > 35, weights = w)
  ref <- betareg::betareg(yield ~ gravity + temp, data = d,
                          subset = gravity > 35, weights = w)

  expect_equal(coef(fit), coef(ref))
  expect_equal(nobs(fit), nobs(ref))
})


test_that("waldTable stacks the coefficient tables of all model parts", {
  fit <- fitMod(yield ~ gravity + temp | temp, data = GasolineYield,
                engine = "beta")
  tab <- fit$waldTable
  sm  <- coef(summary(fit))

  expect_true(is.matrix(tab))
  expect_identical(rownames(tab), names(coef(fit)))
  expect_identical(attr(tab, "component"),
                   rep(c("mean", "precision"), c(3L, 2L)))
  expect_equal(unname(tab[1:3, ]), unname(sm$mean))
  expect_equal(unname(tab[4:5, ]), unname(sm$precision))
  expect_equal(unname(tab[, "Estimate"]), unname(coef(fit)))

  # one-part formula: the constant precision is named "(phi)" as is
  fit1 <- fitMod(yield ~ gravity + temp, data = GasolineYield,
                 engine = "beta")
  expect_identical(rownames(fit1$waldTable),
                   c("(Intercept)", "gravity", "temp", "(phi)"))
  expect_identical(rownames(fit1$waldTable), names(coef(fit1)))
})


test_that("drop1 matches explicit likelihood-ratio tests", {
  fit <- fitMod(yield ~ gravity + temp | temp, data = GasolineYield,
                engine = "beta")
  d1  <- fit$drop1

  expect_identical(rownames(d1),
                   c("<none>", "gravity", "temp", "(phi)_temp"))
  expect_identical(names(d1), c("Df", "AIC", lrCols))
  expect_equal(d1["<none>", "AIC"], AIC(fit))
  expect_equal(d1$Df, c(5, 4, 4, 4))

  ref <- function(f)
    betareg::betareg(f, data = GasolineYield)

  red <- ref(yield ~ temp | temp)
  expect_equal(unlist(d1["gravity", lrCols], use.names = FALSE),
               lrRef(fit, red), tolerance = 1e-5)
  expect_equal(d1["gravity", "AIC"], AIC(red), tolerance = 1e-6)

  expect_equal(unlist(d1["temp", lrCols], use.names = FALSE),
               lrRef(fit, ref(yield ~ gravity | temp)), tolerance = 1e-5)

  # dropping the only precision regressor leaves a constant precision,
  # but on the log link of the full model
  expect_equal(unlist(d1["(phi)_temp", lrCols], use.names = FALSE),
               lrRef(fit, ref(yield ~ gravity + temp | 1)),
               tolerance = 1e-5)
})


test_that("drop1 uses the observations and weights of the full fit", {
  d <- GasolineYield
  d$w <- rep(1:2, length.out = nrow(d))
  # the NA sits in the term that is dropped: a naive refit would regain
  # this observation and the models would no longer be nested
  d$temp[3L] <- NA

  fit <- fitMod(yield ~ gravity + temp, data = d, engine = "beta",
                subset = gravity > 35, weights = w)
  red <- betareg::betareg(yield ~ gravity, data = d[!is.na(d$temp), ],
                          subset = gravity > 35, weights = w)

  expect_equal(nobs(red), nobs(fit))
  expect_equal(unlist(fit$drop1["temp", lrCols], use.names = FALSE),
               lrRef(fit, red), tolerance = 1e-5)
})


test_that("drop1 copes with transformed terms and factors", {
  fit <- fitMod(I(food / income) ~ log(income) + factor(persons > 3),
                data = FoodExpenditure, engine = "beta")
  red <- betareg::betareg(I(food / income) ~ factor(persons > 3),
                          data = FoodExpenditure)

  expect_identical(rownames(fit$drop1),
                   c("<none>", "log(income)", "factor(persons > 3)"))
  expect_equal(unlist(fit$drop1["log(income)", lrCols],
                      use.names = FALSE),
               lrRef(fit, red), tolerance = 1e-5)
})


test_that("drop1 respects marginality", {
  fit <- fitMod(I(food / income) ~ income * persons,
                data = FoodExpenditure, engine = "beta")
  expect_identical(rownames(fit$drop1), c("<none>", "income:persons"))
})


test_that("test = 'none' omits the test columns", {
  fit <- fitMod(yield ~ gravity + temp, data = GasolineYield,
                engine = "beta")
  expect_named(.drop1.betareg(fit, test = "none"), c("Df", "AIC"))
})


test_that("update() works on the result", {
  fit <- fitMod(yield ~ gravity + temp | temp, data = GasolineYield,
                engine = "beta")
  expect_s3_class(update(fit, . ~ . - temp), "betareg")
})


test_that("predict.FitMod() returns the response scale by default", {
  fit <- fitMod(yield ~ gravity + temp | temp, data = GasolineYield,
                engine = "beta")
  ref <- betareg::betareg(yield ~ gravity + temp | temp,
                          data = GasolineYield)
  nd  <- GasolineYield[1:5, ]

  expect_equal(predict(fit), fitted(ref))
  expect_equal(predict(fit, newdata = nd),
               predict(ref, newdata = nd, type = "response"))
  expect_true(all(predict(fit) > 0 & predict(fit) < 1))
})


test_that("predict.FitMod() passes on the types of predict.betareg()", {
  fit <- fitMod(yield ~ gravity + temp | temp, data = GasolineYield,
                engine = "beta")
  ref <- betareg::betareg(yield ~ gravity + temp | temp,
                          data = GasolineYield)
  nd  <- GasolineYield[1:5, ]

  for (type in c("link", "precision", "variance"))
    expect_equal(predict(fit, newdata = nd, type = type),
                 predict(ref, newdata = nd, type = type))

  expect_equal(predict(fit, newdata = nd, type = "quantile", at = 0.9),
               predict(ref, newdata = nd, type = "quantile", at = 0.9))
})


test_that("print shows both model parts and returns the summary", {
  fit <- fitMod(yield ~ gravity + temp | temp, data = GasolineYield,
                engine = "beta")
  out <- capture.output(res <- print(fit))

  expect_s3_class(res, "summary.betareg")
  expect_true(any(grepl("^Mean model \\(logit link\\):$", out)))
  expect_true(any(grepl("^Precision model \\(log link\\):$", out)))
  expect_true(any(grepl("estimate +95%-lci +uci +p-val", out)))
  expect_true(any(grepl("^Obs \\(NAs\\): 32 \\(0\\)", out)))
  expect_true(any(grepl("AIC:", out)))

  # precision rows are shown without the "(phi)_" prefix of coef()
  expect_false(any(grepl("(phi)_", out, fixed = TRUE)))
  expect_identical(sum(grepl("^temp ", out)), 2L)
})


test_that("print uses Wald intervals at the requested level", {
  fit <- fitMod(yield ~ gravity + temp, data = GasolineYield,
                engine = "beta")
  out <- capture.output(print(fit, conf.level = 0.9, digits = 4))
  ci  <- confint.default(fit, level = 0.9)["gravity", ]

  row <- grep("^gravity ", out, value = TRUE)
  expect_true(any(grepl("90%-lci", out, fixed = TRUE)))
  for (bound in pharos::fm(ci, digits = 4))
    expect_true(grepl(bound, row, fixed = TRUE))
})


test_that("print reports odds ratios for a logit link only", {
  fit <- fitMod(yield ~ gravity + temp, data = GasolineYield,
                engine = "beta")
  out <- capture.output(print(fit, output = "or", digits = 4))

  expect_true(any(grepl("odds ratios", out, fixed = TRUE)))
  expect_true(grepl(pharos::fm(exp(coef(fit)[["gravity"]]), digits = 4),
                    grep("^gravity ", out, value = TRUE), fixed = TRUE))

  probit <- fitMod(yield ~ gravity + temp, data = GasolineYield,
                   engine = "beta", link = "probit")
  expect_error(capture.output(print(probit, output = "or")), "logit link")
})


test_that("print adds a header row per factor with its LR test", {
  # batch is treatment coded with the last level as reference
  fit <- fitMod(yield ~ batch + temp, data = GasolineYield,
                engine = "beta")
  out <- capture.output(print(fit))
  hdr <- grep("^batch +\\(ref: 10\\)", out, value = TRUE)

  expect_length(hdr, 1L)
  expect_true(grepl(pharos::fm(fit$drop1["batch", "p-value"], fmt = "p",
                               pThreshold = 1e-3, digits = 3),
                    hdr, fixed = TRUE))
  expect_identical(sum(grepl("^batch [1-9] ", out)), 9L)

  # factor in the precision model
  d <- GasolineYield
  d$grp <- factor(rep(c("a", "b", "c"), length.out = nrow(d)))
  out <- capture.output(print(fitMod(yield ~ temp | grp, data = d,
                                     engine = "beta")))
  expect_length(grep("^grp +\\(ref: a\\)", out), 1L)
})


test_that("print gives no reference level for other contrasts", {
  # dyslexia is sum coded (-1 / 1)
  fit <- fitMod(accuracy ~ dyslexia + iq, data = ReadingSkills,
                engine = "beta")
  expect_length(.refLevels(fit, strict = FALSE), 0L)
  expect_false(any(grepl("(ref:", capture.output(print(fit)),
                         fixed = TRUE)))
})


test_that("print counts the omitted observations", {
  d <- GasolineYield
  d$temp[3L] <- NA
  out <- capture.output(print(fitMod(yield ~ gravity + temp, data = d,
                                     engine = "beta")))
  expect_true(any(grepl("^Obs \\(NAs\\): 31 \\(1\\)", out)))
})


test_that("print notes an unsupported vcov and passes on 'genuine'", {
  fit <- fitMod(yield ~ gravity + temp, data = GasolineYield,
                engine = "beta")
  expect_message(capture.output(print(fit, vcov = "HC3")),
                 "not supported for betareg")
  expect_output(print(fit, output = "genuine"), "Phi coefficients")
})


test_that("boundary values switch to the extended-support model", {
  skip_if_not_installed("statmod")
  skip_if_not_installed("numDeriv")

  # accuracy1 contains perfect scores (= 1)
  expect_true(any(ReadingSkills$accuracy1 == 1))

  fit <- fitMod(accuracy1 ~ dyslexia + iq | dyslexia,
                data = ReadingSkills, engine = "beta")

  expect_identical(fit$dist, "xbetax")
  expect_identical(rownames(fit$waldTable), names(coef(fit)))
  expect_identical(unique(attr(fit$waldTable, "component")),
                   c("mean", "precision", "nu"))

  red <- betareg::betareg(accuracy1 ~ dyslexia | dyslexia,
                          data = ReadingSkills)
  expect_identical(rownames(fit$drop1),
                   c("<none>", "dyslexia", "iq", "(phi)_dyslexia"))
  expect_equal(unlist(fit$drop1["iq", lrCols], use.names = FALSE),
               lrRef(fit, red), tolerance = 1e-4)

  expect_true(all(predict(fit) > 0 & predict(fit) <= 1))
  expect_output(print(fit), "Distribution: xbetax")
})


test_that("pseudoRSq offers the measures defined for a continuous response", {
  fit  <- fitMod(yield ~ gravity + temp | temp, data = GasolineYield,
                 engine = "beta")
  null <- betareg::betareg(yield ~ 1 | 1, data = GasolineYield)
  res  <- pseudoRSq(fit, which = "all")

  expect_named(res, c("CoxSnell", "Efron", "Ferrari", "AIC", "BIC",
                      "logLik", "logLik0", "G2"))

  l1 <- as.numeric(logLik(fit))
  l0 <- as.numeric(logLik(null))
  expect_equal(res[["logLik0"]], l0, tolerance = 1e-6)
  expect_equal(res[["G2"]], 2 * (l1 - l0), tolerance = 1e-6)
  expect_equal(res[["CoxSnell"]], 1 - exp(-2 * (l1 - l0) / nobs(fit)),
               tolerance = 1e-6)
  expect_equal(res[["Ferrari"]], fit$pseudo.r.squared)
  expect_equal(res[["AIC"]], AIC(fit))

  y <- GasolineYield$yield
  expect_equal(res[["Efron"]],
               1 - sum((y - fitted(fit))^2) / sum((y - mean(y))^2))

  # the log-likelihoods are positive here: no ratio of them is offered,
  # which includes the default measure
  expect_gt(l0, 0)
  expect_error(pseudoRSq(fit), "not defined for this fit: McFadden")
  expect_error(pseudoRSq(fit, "Nagelkerke"), "not defined")

  # plain betareg objects are handled as well
  expect_equal(pseudoRSq(betareg::betareg(yield ~ gravity + temp | temp,
                                          data = GasolineYield), "CoxSnell"),
               res["CoxSnell"])
})


test_that("pseudoRSq refits the null model on the sample of the full fit", {
  d <- GasolineYield
  d$w <- rep(1:2, length.out = nrow(d))
  d$temp[3L] <- NA

  fit  <- fitMod(yield ~ gravity + temp, data = d, engine = "beta",
                 subset = gravity > 35, weights = w)
  null <- betareg::betareg(yield ~ 1, data = d[!is.na(d$temp), ],
                           subset = gravity > 35, weights = w)

  expect_equal(pseudoRSq(fit, "logLik0")[["logLik0"]],
               as.numeric(logLik(null)), tolerance = 1e-6)
})


test_that("pseudoRSq covers the extended-support model", {
  skip_if_not_installed("statmod")
  skip_if_not_installed("numDeriv")

  fit  <- fitMod(accuracy1 ~ dyslexia + iq | dyslexia,
                 data = ReadingSkills, engine = "beta")
  null <- betareg::betareg(accuracy1 ~ 1 | 1, data = ReadingSkills)
  res  <- pseudoRSq(fit, which = "all")

  expect_equal(res[["logLik0"]], as.numeric(logLik(null)),
               tolerance = 1e-5)
  # betareg reports no pseudo R-squared for this distribution
  expect_false("Ferrari" %in% names(res))
})


test_that("vif refers to the mean model", {
  fit <- fitMod(yield ~ batch + temp | temp, data = GasolineYield,
                engine = "beta")
  res <- vif(fit)

  expect_identical(rownames(res), c("batch", "temp"))
  expect_equal(unname(res[, "Df"]), c(9, 1))

  # with two terms both share one generalized VIF, computed from the
  # correlation of the mean coefficients alone
  R <- cov2cor(vcov(fit, model = "mean")[-1L, -1L])
  expect_equal(unname(res[, "GVIF"]),
               rep(det(R[1:9, 1:9]) * R[10L, 10L] / det(R), 2L))
})


test_that("refLevel collects the factors of both model parts", {
  d <- FoodExpenditure
  d$size <- cut(d$persons, c(0, 2, 4, 10),
                labels = c("small", "medium", "large"))
  d$rich <- factor(d$income > 60, labels = c("no", "yes"))
  fit <- fitMod(I(food / income) ~ income + size | rich, data = d,
                engine = "beta")

  expect_identical(refLevel(fit), c(size = "small", rich = "no"))
  expect_identical(.refLevels(fit, part = "precision"), c(rich = "no"))

  # batch is treatment coded with the last level as reference
  expect_identical(
    refLevel(fitMod(yield ~ batch + temp, data = GasolineYield,
                    engine = "beta")),
    c(batch = "10")
  )
})


test_that("tMod takes beta regressions next to other models", {
  beta1 <- fitMod(yield ~ gravity + temp, data = GasolineYield,
                  engine = "beta")
  beta2 <- fitMod(yield ~ gravity + temp | temp, data = GasolineYield,
                  engine = "beta")
  lin   <- fitMod(yield ~ gravity + temp, data = GasolineYield,
                  engine = "lm")

  sm <- tModSummary(beta2, conf.level = 0.9)
  expect_identical(sm$coef$name, names(coef(beta2)))
  expect_equal(sm$coef$est, unname(coef(beta2)))
  expect_equal(cbind(sm$coef$lci, sm$coef$uci),
               unname(confint.default(beta2, level = 0.9)))
  expect_equal(sm$statsx[["N"]], 32)
  expect_equal(sm$statsx[["Ferrari"]], beta2$pseudo.r.squared)

  tm <- tMod(beta1, beta2, lin)
  expect_identical(tm[[1L]]$coef,
                   c("(Intercept)", "gravity", "temp", "(phi)",
                     "(phi)_(Intercept)", "(phi)_temp"))
  expect_identical(dimnames(tm$mall)[[2L]], c("beta1", "beta2", "lin"))
  expect_output(print(tm), "Ferrari")
})
