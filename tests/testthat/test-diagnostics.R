# Diagnostics for logistic models -----------------------------------------
#
# The plots are exercised on a null device: the point of the tests is the
# arithmetic behind them and the contract of the return values, not the
# pixels.

.diagData <- function(n = 400, seed = 7) {
  set.seed(seed)
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  g  <- factor(sample(letters[1:3], n, replace = TRUE))
  p  <- plogis(-0.4 + 0.9 * x1 - 0.5 * x2 + as.numeric(g) * 0.3)
  data.frame(y = rbinom(n, 1L, p), x1 = x1, x2 = x2, g = g)
}

.diagFit <- function(d = .diagData())
  fitMod(y ~ x1 + x2 + g, data = d, fitfn = "logit")

.nullDevice <- function(expr) {
  pdf(NULL)
  on.exit(dev.off())
  force(expr)
}


# --- quantileResid -------------------------------------------------------

test_that("quantile residuals are standard normal under a correct model", {

  fit <- .diagFit()

  set.seed(1)
  r <- quantileResid(fit)

  expect_length(r, nobs(fit))
  expect_true(all(is.finite(r)))
  expect_gt(ks.test(r, "pnorm")$p.value, 0.01)
  expect_lt(abs(mean(r)), 0.15)
})


test_that("quantileResid respects the seed and nSim", {

  fit <- .diagFit()

  set.seed(42); a <- quantileResid(fit)
  set.seed(42); b <- quantileResid(fit)
  expect_identical(a, b)

  set.seed(42); m <- quantileResid(fit, nSim = 3L)
  expect_equal(dim(m), c(nobs(fit), 3L))
  expect_false(isTRUE(all.equal(m[, 1L], m[, 2L])))

  expect_error(quantileResid(fit, nSim = 0), "nSim")
  expect_error(quantileResid(fit, nSim = 2.5), "nSim")
})


test_that("quantile residuals stay inside the jump of the distribution", {

  # r = qnorm(u) with u between F(y-1) and F(y): a success can never get a
  # residual below qnorm(1 - p), a failure never one above qnorm(1 - p).
  fit <- .diagFit()
  p   <- fitted(fit)
  y   <- model.response(model.frame(fit))
  y   <- if (is.factor(y)) as.integer(y) - 1L else as.numeric(y)

  set.seed(5)
  r <- quantileResid(fit)

  expect_true(all(r[y == 1] >= qnorm(1 - p[y == 1])))
  expect_true(all(r[y == 0] <= qnorm(1 - p[y == 0])))
})


# --- plotBinnedResid -----------------------------------------------------

test_that("plotBinnedResid returns one row per bin with a symmetric band", {

  fit <- .diagFit()
  tab <- .nullDevice(plotBinnedResid(fit, nBins = 12))

  expect_s3_class(tab, "data.frame")
  expect_named(tab, c("bin", "x", "y", "n", "se", "lci", "uci"))
  expect_equal(nrow(tab), 12L)
  expect_equal(sum(tab$n), nobs(fit))
  expect_equal(tab$lci, -tab$uci)
  expect_true(all(tab$se > 0))
})


test_that("a factor is binned by its levels", {

  d   <- .diagData()
  fit <- .diagFit(d)
  tab <- .nullDevice(plotBinnedResid(fit, var = "g"))

  expect_equal(nrow(tab), nlevels(d$g))
  expect_equal(tab$bin, levels(d$g))
})


test_that("the band width follows conf.level and method", {

  fit  <- .diagFit()
  wide <- .nullDevice(plotBinnedResid(fit, nBins = 10, conf.level = 0.99))
  narr <- .nullDevice(plotBinnedResid(fit, nBins = 10, conf.level = 0.90))

  expect_true(all(wide$uci > narr$uci))

  emp <- .nullDevice(plotBinnedResid(fit, nBins = 10, method = "empirical"))
  expect_equal(emp$y, narr$y)                 # same points
  expect_false(isTRUE(all.equal(emp$se, narr$se)))  # different band

  expect_error(plotBinnedResid(fit, conf.level = 1), "conf.level")
})


test_that("binned residuals detect a misspecified functional form", {

  set.seed(11)
  n <- 2000
  x <- runif(n, -3, 3)
  d <- data.frame(y = rbinom(n, 1L, plogis(-1 + 0.8 * x^2)), x = x)

  bad  <- fitMod(y ~ x,      data = d, fitfn = "logit")
  good <- fitMod(y ~ I(x^2), data = d, fitfn = "logit")

  outside <- function(f)
    attr(.nullDevice(plotBinnedResid(f, var = "x", nBins = 20)), "outside")

  expect_gt(outside(bad), 10)
  expect_lt(outside(good), 5)
})


test_that("a variable outside the model frame is found in the model data", {

  set.seed(3)
  d   <- data.frame(y = rbinom(200, 1L, 0.4), x = rnorm(200))
  fit <- fitMod(y ~ I(x^2), data = d, fitfn = "logit")

  expect_silent(.nullDevice(plotBinnedResid(fit, var = "x", nBins = 8)))
  expect_error(.nullDevice(plotBinnedResid(fit, var = "nope")), "nope")
})


# --- predictors ----------------------------------------------------------

test_that("predictors returns the term labels, optionally numeric only", {

  d   <- .diagData()
  fit <- .diagFit(d)

  expect_identical(predictors(fit), c("x1", "x2", "g"))
  expect_identical(predictors(fit), attr(terms(fit), "term.labels"))
  expect_identical(predictors(fit, numeric = TRUE), c("x1", "x2"))

  # transformed terms keep their written form, interactions come as one term
  fit2 <- fitMod(y ~ I(x1^2) + x1:x2, data = d, fitfn = "logit")
  expect_identical(predictors(fit2), c("I(x1^2)", "x1:x2"))

  expect_identical(predictors(fitMod(y ~ 1, data = d, fitfn = "logit")),
                   character(0))

  # works on the plain model too, not just on FitMod
  raw <- fit
  class(raw) <- setdiff(class(raw), "FitMod")
  expect_identical(predictors(raw), predictors(fit))
})


# --- binnedResid ---------------------------------------------------------

test_that("binnedResid computes without a device and vectorises over var", {

  fit  <- .diagFit()
  vars <- attr(terms(fit), "term.labels")

  one <- binnedResid(fit, var = "x1", nBins = 10)
  expect_s3_class(one, "data.frame")
  expect_equal(nrow(one), 10L)
  expect_identical(attr(one, "label"), "x1")
  expect_identical(attr(one, "method"), "model")
  expect_type(attr(one, "outside"), "integer")

  expect_identical(vars, predictors(fit))
  many <- binnedResid(fit, var = vars, nBins = 10)
  expect_named(many, vars)
  expect_equal(many$x1, one)

  # the fitted probabilities are the default binning variable
  expect_identical(attr(binnedResid(fit), "label"), "fitted probability")

  # a factor is grouped by its levels, and marked as categorical
  g <- binnedResid(fit, var = "g")
  expect_true(attr(g, "categorical"))
  expect_false(attr(one, "categorical"))
})


test_that("plotBinnedResid draws exactly what binnedResid computes", {

  fit <- .diagFit()

  drawn <- .nullDevice(plotBinnedResid(fit, var = "x1", nBins = 10))
  computed <- binnedResid(fit, var = "x1", nBins = 10)

  expect_equal(drawn, computed)

  # several variables are a facet job, not a single panel
  expect_error(.nullDevice(plotBinnedResid(fit, var = c("x1", "x2"))),
               "single binning variable")
})


test_that("panelBinnedResid draws into a prepared panel", {

  fit  <- .diagFit()
  bins <- binnedResid(fit, var = c("x1", "x2"), nBins = 8)

  .nullDevice({
    plot.new()
    plot.window(range(bins$x1$x), range(c(bins$x1$lci, bins$x1$uci)))
    expect_null(with(bins$x1, panelBinnedResid(x, y, lci, uci, col = 1)))
  })
})


# --- plotCalibration -----------------------------------------------------

test_that("in-sample calibration is perfect by construction", {

  fit <- .diagFit()
  cal <- .nullDevice(plotCalibration(fit))

  expect_equal(cal$intercept, 0, tolerance = 1e-6)
  # the returned value stays exact; only the annotation is rounded
  expect_false(identical(cal$intercept, 0))
  expect_identical(alloy:::.roundForDisplay(cal$intercept, 2), 0)
  expect_identical(alloy:::.roundForDisplay(-3.67e-13, 2), 0)
  expect_identical(alloy:::.roundForDisplay(-0.004, 2), 0)
  expect_equal(alloy:::.roundForDisplay(0.987, 2), 0.99)
  expect_equal(cal$slope, 1, tolerance = 1e-6)
  expect_true(cal$inSample)

  # brierScore() is fed vectors, not the model: handed a "FitMod" it takes
  # its glm branch, and the predict() call there dispatches to
  # predict.FitMod(), which returns a two-column data frame of class
  # probabilities rather than a vector.
  raw <- fit
  class(raw) <- setdiff(class(raw), "FitMod")
  expect_equal(cal$brier, brierScore(as.numeric(raw$y), fitted(raw)))
})


test_that("calibration on new data uses the model's own factor coding", {

  d    <- .diagData(n = 600)
  idx  <- seq_len(400)
  fit  <- fitMod(y ~ x1 + x2 + g, data = d[idx, ], fitfn = "logit")

  cal <- .nullDevice(plotCalibration(fit, newdata = d[-idx, ]))

  expect_false(cal$inSample)
  expect_true(is.finite(cal$slope))

  # a validation frame whose factor levels arrive in a different order
  # must not silently flip the response coding
  d2 <- d[-idx, ]
  d2$y <- factor(d2$y, levels = c(1, 0))
  fitF <- fitMod(factor(y) ~ x1 + x2 + g, data = d[idx, ], fitfn = "logit")
  calF <- .nullDevice(plotCalibration(fitF, newdata = d2))

  expect_equal(calF$slope, cal$slope, tolerance = 1e-8)

  expect_error(.nullDevice(plotCalibration(fit, newdata = d[-idx, -1L])), "y")
})


test_that("plotCalibration returns its bins", {

  fit <- .diagFit()
  cal <- .nullDevice(plotCalibration(fit, nBins = 8))

  expect_equal(nrow(cal$bins), 8L)
  expect_equal(sum(cal$bins$n), nobs(fit))
  expect_true(all(cal$bins$obs >= 0 & cal$bins$obs <= 1))

  expect_null(.nullDevice(plotCalibration(fit, nBins = FALSE))$bins)
})


test_that("the annotation accepts a named anchor position", {

  fit <- .diagFit()

  # the anchor is consumed by abcCoords() and must not reach boxedText()
  # as a coordinate, where it would coerce to NA
  for (lg in list(list(x = "bottomright"), list(x = "top", cex = 0.6),
                  list(cex = 0.6), TRUE, FALSE))
    expect_true(is.finite(.nullDevice(plotCalibration(fit, legend = lg))$slope))
})


# --- plotInfluence -------------------------------------------------------

test_that("influence statistics match the stats originals", {

  fit <- .diagFit()
  tab <- .nullDevice(plotInfluence(fit))

  raw <- fit
  class(raw) <- setdiff(class(raw), "FitMod")

  expect_equal(tab$cook, unname(cooks.distance(raw)))
  expect_equal(tab$hat,  unname(hatvalues(raw)))
  expect_equal(tab$dChisq,
               unname(residuals(raw, "pearson")^2 / (1 - hatvalues(raw))))
  expect_true(all(tab$dChisq >= 0))
  expect_true(all(tab$dDeviance >= 0))
})


test_that("plotInfluence label selection accepts a count", {

  fit <- .diagFit()
  expect_silent(.nullDevice(plotInfluence(fit, labels = 3)))
  expect_silent(.nullDevice(plotInfluence(fit, labels = FALSE)))
  expect_silent(.nullDevice(plotInfluence(fit, labels = TRUE)))
  expect_silent(.nullDevice(plotInfluence(fit, metric = "deviance")))
  expect_silent(.nullDevice(plotInfluence(fit, reference = FALSE)))
  expect_error(plotInfluence(fit, metric = "nope"))
})


test_that("the points lie on the two reference arms", {

  # at zero leverage the change statistic is a function of p alone; the
  # arms are that function, so the points can only sit above them, and by
  # exactly the leverage factor 1 / (1 - h)
  fit <- .diagFit()
  tab <- .nullDevice(plotInfluence(fit))

  raw <- fit
  class(raw) <- setdiff(class(raw), "FitMod")
  y <- as.numeric(raw$y)

  arm <- ifelse(y == 1, (1 - tab$p) / tab$p, tab$p / (1 - tab$p))

  expect_equal(tab$dChisq, arm / (1 - tab$hat))
  expect_true(all(tab$dChisq >= arm - 1e-9))

  armD <- ifelse(y == 1, -2 * log(tab$p), -2 * log(1 - tab$p))
  expect_true(all(tab$dDeviance >= armD - 1e-9))
})


test_that("the chi-squared cut-off is off for ungrouped data", {

  # grouped: the conventional line applies and is drawn
  d <- data.frame(succ = c(3, 7, 12, 2, 9), m = c(10, 12, 20, 8, 15),
                  x = c(1, 2, 3, 4, 5))
  grp <- glm(cbind(succ, m - succ) ~ x, d, family = binomial)
  class(grp) <- c("FitMod", class(grp))

  expect_silent(.nullDevice(plotInfluence(grp)))
  expect_silent(.nullDevice(plotInfluence(grp, threshold = NA)))
})


# --- plotSeparation ------------------------------------------------------

test_that("plotSeparation returns the sorted predictions", {

  fit <- .diagFit()
  sep <- .nullDevice(plotSeparation(fit))

  expect_equal(nrow(sep), nobs(fit))
  expect_false(is.unsorted(sep$p))
  expect_true(all(sep$y %in% c(0, 1)))

  expect_error(.nullDevice(plotSeparation(fit, col = "red")), "two colours")
})


# --- plotPartialResid ----------------------------------------------------

test_that("partial residuals are the term plus the working residual", {

  fit <- .diagFit()
  tab <- .nullDevice(plotPartialResid(fit, term = "x1"))

  raw <- fit
  class(raw) <- setdiff(class(raw), "FitMod")
  p <- fitted(raw)

  expect_equal(tab$partial,
               unname(coef(raw)["x1"] * model.frame(raw)$x1 +
                        (raw$y - p) / (p * (1 - p))))
  expect_equal(tab$weight, unname(p * (1 - p)))
})


test_that("plotPartialResid refuses terms that have no single slope", {

  fit <- .diagFit()

  expect_error(.nullDevice(plotPartialResid(fit, term = "g")), "not numeric")
  expect_error(.nullDevice(plotPartialResid(fit, term = "nope")),
               "not a predictor")

  # no argument: the first continuous term
  expect_equal(attr(.nullDevice(plotPartialResid(fit)), "class"), "data.frame")
})


# --- default titles ------------------------------------------------------

test_that("the default title shortens on terms, not on characters", {

  d <- .diagData()
  d[paste0("z", 1:8)] <- matrix(rnorm(nrow(d) * 8), nrow(d))

  wide <- fitMod(y ~ ., data = d, fitfn = "logit")
  ttl  <- alloy:::.modelTitle(wide, "influence")

  expect_lt(nchar(ttl), 70L)
  expect_match(ttl, "^influence: y ~ ")
  expect_match(ttl, "\\+ [0-9]+ more$")

  # a small model keeps its formula intact
  narrow <- .diagFit(d)
  expect_equal(alloy:::.modelTitle(narrow, "calibration"),
               "calibration: y ~ x1 + x2 + g")

  # an intercept-only model has nothing to shorten
  expect_match(alloy:::.modelTitle(
    fitMod(y ~ 1, data = d, fitfn = "logit"), "separation"), "~ 1$")
})


# --- plot.FitMod ---------------------------------------------------------

test_that("plot.FitMod draws the requested panels and passes others on", {

  fit <- .diagFit()

  res <- .nullDevice({
    op <- par(mfrow = c(2, 3)); on.exit(par(op))
    plot(fit, ask = FALSE)
  })

  expect_length(res, 5L)
  expect_named(res, as.character(1:5))

  expect_length(.nullDevice(plot(fit, which = 2, ask = FALSE)), 1L)
  expect_error(plot(fit, which = 9), "which")

  # a linear model keeps plot.lm
  fitLm <- fitMod(x1 ~ x2, data = .diagData())
  expect_silent(.nullDevice(plot(fitLm, which = 1)))
})


test_that("the diagnostics refuse non-logistic models", {

  fitLm <- fitMod(x1 ~ x2, data = .diagData())

  for (f in list(plotBinnedResid, plotCalibration, plotInfluence,
                 plotSeparation, plotPartialResid, quantileResid))
    expect_error(f(fitLm), "logit")
})
