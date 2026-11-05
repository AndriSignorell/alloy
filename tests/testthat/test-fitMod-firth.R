
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


test_that("a fit diverging from the default start is repeated from 0", {

  # brglmFit starts at the ML estimates, which diverge here: x = 1 only
  # among the cases (complete separation)
  set.seed(7)
  n <- 3000
  d <- data.frame(a = rbinom(n, 1, 0.3), b = rbinom(n, 1, 0.2),
                  x = rbinom(n, 1, 0.005), age = rnorm(n))
  d$y <- rbinom(n, 1, plogis(-0.3 + 1.5 * d$a - d$b + 0.4 * d$age))
  d$y[d$x == 1] <- 1

  plain <- suppressWarnings(
    glm(y ~ a + b + x + age, binomial, d, method = brglm2::brglmFit,
        type = "MPL_Jeffreys"))
  skip_if(isTRUE(plain$converged), "brglm2 converges here by now")

  fit <- fitMod(y ~ a + b + x + age, d, engine = "logitFirth")
  expect_true(fit$converged)
  expect_lt(coef(fit)["x"], 10)

  # the same maximum as the internal fitter, from 0
  dd <- alloy:::.firthData(fit)
  ff <- alloy:::.firthFit(dd, start = rep(0, ncol(dd$X)))
  expect_equal(ff$coef, coef(fit), tolerance = 1e-6)

  # no start values left in the call: update() with another formula works
  expect_null(fit$call$start)
  expect_true(update(fit, . ~ . - age)$converged)

  expect_true(all(is.finite(confint(fit))))

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


test_that("estimates, intervals and PLR test reproduce logistf", {
  # SAS and R, Example 8.15: 2x2 table with an empty cell
  d <- data.frame(pred = c(rep(1, 20), rep(0, 220)),
                  outcome = c(rep(1, 40), rep(0, 200)))
  f <- fitMod(outcome ~ pred, d, engine = "logitFirth")
  expect_equal(unname(coef(f)), c(-2.280389, 5.993961), tolerance = 1e-6)
  expect_equal(unname(confint(f)["pred", ]), c(3.947048, 10.852893), tolerance = 1e-6)
  expect_equal(drop1(f)["pred", "PLR"], 78.95473, tolerance = 1e-6)
})


test_that("print() reports the profile intervals and PLR tests by default", {

  ci <- confint(fitF)
  inf <- alloy:::.firthInference(fitF, ci = seq_along(coef(fitF)),
                                 pval = TRUE)
  expect_equal(unname(inf$ci), unname(ci))

  out <- capture.output(print(fitF))
  expect_true(any(grepl("CI: profile penalized likelihood", out,
                        fixed = TRUE)))
  row <- grep("^gpa ", out, value = TRUE)
  expect_true(grepl(pharos::fm(ci["gpa", 2L], digits = 3), row, fixed = TRUE))

  # useProfile = FALSE: Wald intervals, no note
  outW <- capture.output(print(fitF, useProfile = FALSE))
  expect_false(any(grepl("profile", outW, fixed = TRUE)))
  rowW <- grep("^gpa ", outW, value = TRUE)
  expect_true(grepl(pharos::fm(confint.default(fitF)["gpa", 2L], digits = 3),
                    rowW, fixed = TRUE))

  # plain logit keeps Wald unless asked
  fitL <- fitMod(admit ~ gre + gpa + rank, Admit, engine = "logit")
  expect_false(any(grepl("profile", capture.output(print(fitL)),
                         fixed = TRUE)))
  expect_true(any(grepl("CI: profile likelihood, p: Wald test",
                        capture.output(print(fitL, useProfile = TRUE)),
                        fixed = TRUE)))

})


test_that("the coefficient-wise PLR tests agree with intervals and drop1()", {

  inf <- alloy:::.firthInference(fitF, ci = seq_along(coef(fitF)),
                                 pval = TRUE)

  # 0 outside the 95% interval exactly when p < 0.05
  outside <- inf$ci[, 1L] > 0 | inf$ci[, 2L] < 0
  expect_identical(unname(outside), unname(inf$p < 0.05))

  # a one-df term: the coefficient test is the term test
  tab <- drop1(fitF)
  for (nm in c("gre", "gpa"))
    expect_equal(unname(inf$p[nm]), tab[nm, "Pr(>Chi)"], tolerance = 1e-8)

})


test_that("the profile results are memoized by the content of the model", {

  cache <- alloy:::.firthCache
  rm(list = ls(cache, all.names = TRUE), envir = cache)

  ci <- confint(fitF)
  expect_length(cache$.keys, 1L)

  # from now on any refit fails: what still works came from the cache
  # (print() would refit for the drop1() test of the factor rank)
  local_mocked_bindings(.firthFit = function(...) stop("refit"),
                        .package = "alloy")

  expect_identical(confint(fitF), ci)
  expect_identical(confint(fitF, parm = "gpa"), ci["gpa", , drop = FALSE])

  # modified estimates, data or level: a different model, no stale result
  fitB <- fitF
  fitB$coefficients["gpa"] <- fitB$coefficients["gpa"] + 1e-10
  expect_error(confint(fitB), "refit")

  fitY <- fitF
  fitY$y[1L] <- 1 - fitY$y[1L]
  expect_error(confint(fitY), "refit")

  expect_error(confint(fitF, level = 0.9), "refit")

})


test_that("the cache keeps the most recently used entries only", {

  cache <- alloy:::.firthCache
  rm(list = ls(cache, all.names = TRUE), envir = cache)
  on.exit(rm(list = ls(cache, all.names = TRUE), envir = cache))

  nmax <- alloy:::.firthCacheMax
  for (i in seq_len(nmax)) alloy:::.firthStore(paste0("k", i), list(i = i))
  alloy:::.firthStore("k1", list(i = 1))      # k1 used again: now newest
  alloy:::.firthStore("new", list(i = 0))     # pushes out k2

  expect_length(cache$.keys, nmax)
  expect_false(exists("k2", envir = cache, inherits = FALSE))
  expect_true(exists("k1", envir = cache, inherits = FALSE))
  expect_identical(tail(cache$.keys, 2L), c("k1", "new"))
  expect_setequal(ls(cache), cache$.keys)

})


test_that("the key covers the whole design, not only its sums", {

  d <- alloy:::.firthData(fitF)
  k <- alloy:::.firthKey(d, 0.95)
  expect_match(k, "^[0-9a-f]{64}$")

  # swap 'gre' between two rows with the same response: colSums(X) and
  # X'wy stay the same, the design (and the penalty) does not
  i <- which(d$y == d$y[1L] & d$X[, "gre"] != d$X[1L, "gre"])[1L]
  d2 <- d
  d2$X[c(1L, i), "gre"] <- d$X[c(i, 1L), "gre"]
  expect_equal(colSums(d2$X), colSums(d$X))
  expect_equal(crossprod(d2$X, d2$y), crossprod(d$X, d$y))
  expect_false(identical(alloy:::.firthKey(d2, 0.95), k))

})


test_that("a model with a single coefficient gets its profile interval", {

  d <- data.frame(y = c(rep(1, 40), rep(0, 200)))
  f <- fitMod(y ~ 1, d, engine = "logitFirth")
  ci <- confint(f)

  expect_true(all(is.finite(ci)))
  dd <- alloy:::.firthData(f)
  for (b in ci) {
    l0 <- alloy:::.firthFit(dd, start = coef(f))$loglik
    r  <- alloy:::.firthFit(dd, start = b, free = FALSE)
    expect_equal(2 * (l0 - r$loglik), qchisq(0.95, 1), tolerance = 1e-5)
  }

  expect_no_error(capture.output(print(f)))

})


test_that("failed restricted fits give NA with a warning and are not cached", {

  cache <- alloy:::.firthCache
  rm(list = ls(cache, all.names = TRUE), envir = cache)

  # the restricted fits get a single iteration and cannot converge
  orig <- alloy:::.firthFit
  local_mocked_bindings(
    .firthFit = function(d, start, free = rep(TRUE, length(start)), ...)
      orig(d, start, free, maxit = if (all(free)) 100L else 1L),
    .package = "alloy")

  expect_warning(ci <- confint(fitF, parm = "gpa"),
                 "no profile interval for: gpa")
  expect_true(all(is.na(ci)))
  expect_warning(tab <- drop1(fitF), "no penalized LR test")
  expect_true(all(is.na(tab$PLR[-1L])))

  # with working fits the NA entries are computed, not taken from the cache
  local_mocked_bindings(.firthFit = orig, .package = "alloy")
  expect_no_warning(ci <- confint(fitF, parm = "gpa"))
  expect_true(all(is.finite(ci)))

})
