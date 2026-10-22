
# tests for fitMod(engine = "logitFirth") and its penalized likelihood methods --

skip_if_not_installed("brglm2")

fitF <- fitMod(admit ~ gre + gpa + rank, Admit, engine = "logitFirth")

# separated data: z = 1 always has y = 1
set.seed(3)
dSep <- data.frame(x = rnorm(300), z = rbinom(300, 1, 0.1))
dSep$y <- rbinom(300, 1, plogis(-0.5 + dSep$x))
dSep$y[dSep$z == 1] <- 1

# penalized log-likelihood, written out independently of .firthFit()
lstar <- function(b, X, y) {
  p <- plogis(drop(X %*% b))
  sum(y * log(p) + (1 - y) * log(1 - p)) +
    0.5 * as.numeric(determinant(crossprod(X * sqrt(p * (1 - p))))$modulus)
}


test_that("the engine fits Firth's model and carries the class", {

  expect_s3_class(fitF, c("FitMod", "logitFirth", "brglmFit", "glm"))
  expect_identical(fitF$engine, "logitFirth")

  ref <- glm(admit ~ gre + gpa + rank, binomial, Admit,
             method = brglm2::brglmFit, type = "MPL_Jeffreys")
  expect_equal(coef(fitF), coef(ref))

  # the internal fitter finds the same maximum from scratch
  d <- alloy:::.firthData(fitF)
  ff <- alloy:::.firthFit(d, start = rep(0, length(d$coef)))
  expect_equal(ff$coef, coef(fitF), tolerance = 1e-6)
  expect_equal(ff$loglik, lstar(coef(fitF), d$X, d$y), tolerance = 1e-8)

})


test_that("estimates stay finite under separation", {

  fit <- fitMod(y ~ x + z, dSep, engine = "logitFirth")
  ml <- suppressWarnings(glm(y ~ x + z, binomial, dSep))

  expect_gt(coef(ml)["z"], 15)
  expect_lt(coef(fit)["z"], 10)

  ci <- confint(fit)
  expect_true(all(is.finite(ci)))
  # profile intervals are asymmetric where the likelihood is
  expect_gt(ci["z", 2] - coef(fit)["z"], coef(fit)["z"] - ci["z", 1])

})


test_that("drop1() gives penalized likelihood ratio tests", {

  tab <- drop1(fitF)

  expect_s3_class(tab, "anova")
  expect_identical(rownames(tab), c("<none>", "gre", "gpa", "rank"))
  expect_identical(tab$Df, c(NA, 1, 1, 3))

  # the restricted maximum, found by optim() on the full-model penalty
  d <- alloy:::.firthData(fitF)
  drop <- attr(d$X, "assign") == 3L
  b <- coef(fitF)
  obj <- function(v) { bb <- b; bb[drop] <- 0; bb[!drop] <- v
                       -lstar(bb, d$X, d$y) }
  o <- optim(b[!drop], obj, method = "BFGS",
             control = list(parscale = pmax(abs(b[!drop]), 1e-3),
                            reltol = 1e-15, maxit = 5000))
  plr <- 2 * (lstar(b, d$X, d$y) + o$value)

  expect_equal(tab["rank", "PLR"], plr, tolerance = 1e-5)
  expect_equal(tab["rank", "Pr(>Chi)"], pchisq(plr, 3, lower.tail = FALSE),
               tolerance = 1e-5)

})


test_that("confint() bounds sit where the PLR test just rejects", {

  ci <- confint(fitF, parm = c("gpa", "rank2"), level = 0.9)
  expect_identical(dimnames(ci), list(c("gpa", "rank2"), c("5 %", "95 %")))

  d <- alloy:::.firthData(fitF)
  b <- coef(fitF)
  lmax <- lstar(b, d$X, d$y)

  for (nm in rownames(ci)) for (bound in ci[nm, ]) {
    j <- match(nm, names(b))
    st <- b
    st[j] <- bound
    r <- alloy:::.firthFit(d, start = st, free = seq_along(b) != j)
    expect_equal(2 * (lmax - r$loglik), qchisq(0.9, 1), tolerance = 1e-4)
  }

  expect_true(all(ci[, 1] < b[rownames(ci)] & b[rownames(ci)] < ci[, 2]))

})


test_that("only the Jeffreys penalty with the logit link qualifies", {

  expect_true(alloy:::.isJeffreysLogit(fitF))

  med <- fitMod(admit ~ gre + gpa + rank, Admit, engine = "logitFirth",
                type = "AS_median")
  expect_false(alloy:::.isJeffreysLogit(med))

})


test_that("prediction, cross-validation, comparison and printing work", {

  p <- predict(fitF, newdata = head(Admit))
  expect_named(p, c("0", "1"))
  expect_equal(p[["1"]], unname(predict(fitF, head(Admit), type = "link") |>
                                  plogis()), ignore_attr = TRUE)
  expect_identical(levels(predict(fitF, output = "class")$class), c("0", "1"))

  set.seed(1)
  cv <- crossVal(fitF, k = 3)
  expect_identical(cv$engine, "logitFirth")
  expect_s3_class(cv$models, NA)
  expect_true(cv$summary["auc", "cv"] > 0.6)

  tm <- tMod(ML = fitMod(admit ~ gre + gpa + rank, Admit, engine = "logit"),
             Firth = fitF)
  expect_equal(unname(tm$mall["gpa", "Firth", ]),
               unname(c(coef(fitF)["gpa"], confint(fitF, "gpa"))))

  expect_output(print(fitF), "rank")
  expect_output(print(fitF, output = "or", useProfile = TRUE), "OR")

})
