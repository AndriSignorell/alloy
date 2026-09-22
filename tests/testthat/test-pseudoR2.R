
# == the result itself =====================================================

test_that("the default is McFadden and stays named", {

  fit <- binaryFit()
  res <- pseudoR2(fit)

  expect_type(res, "double")
  expect_length(res, 1L)
  expect_named(res, "McFadden")
})


test_that("which = 'all' returns everything available, in vocabulary order", {

  res <- pseudoR2(binaryFit(), which = "all")

  expect_named(res, .pseudoR2Measures)
  expect_false(anyNA(res))
})


test_that("several measures are returned in the requested order", {

  fit <- binaryFit()

  expect_named(pseudoR2(fit, c("Tjur", "McFadden")), c("Tjur", "McFadden"))
  expect_named(pseudoR2(fit, c("AIC", "BIC", "G2")), c("AIC", "BIC", "G2"))

  expect_equal(
    pseudoR2(fit, "Nagelkerke"),
    pseudoR2(fit, which = "all")["Nagelkerke"]
  )
})


test_that("measure names may be abbreviated", {

  fit <- binaryFit()

  expect_named(pseudoR2(fit, "Nagelk"), "Nagelkerke")
  expect_named(pseudoR2(fit, c("Tju", "Cox")), c("Tjur", "CoxSnell"))

  # McFadden and McFaddenAdj both start with McF
  expect_error(pseudoR2(fit, "McF"), "ambiguous measure")
})


test_that("a typo is caught before the null model is refitted", {

  fit <- binaryFit()

  expect_error(pseudoR2(fit, "Mcfadden"), "unknown measure")

  # a wrong name next to a correct one must not slip through
  expect_error(pseudoR2(fit, c("McFadden", "Bogus")), "unknown measure: Bogus")

  expect_error(pseudoR2(fit, 1), "must be a character vector")
})


test_that("a measure that the fit does not support is an error", {

  cloglog <- binaryFit(link = "cloglog")

  expect_error(pseudoR2(cloglog, "McKelveyZavoina"), "not defined for this fit")

  # but 'all' silently omits it
  expect_false("McKelveyZavoina" %in% names(pseudoR2(cloglog, which = "all")))
})


test_that("unsupported model classes are rejected by class, not by failure", {

  expect_error(
    pseudoR2(lm(mpg ~ wt, data = mtcars)),
    "no pseudo R-squared available"
  )

  expect_error(pseudoR2(1:10), "no pseudo R-squared available")
})




# == aggregation invariance ================================================

test_that("all measures are invariant to how binomial data are stored", {

  fits <- binomFits()
  res <- lapply(fits, pseudoR2, which = "all")

  expect_equal(res$single, res$matrix, tolerance = 1e-6)
  expect_equal(res$weights, res$matrix, tolerance = 1e-6)
  expect_equal(res$noResponse, res$matrix)
})


test_that("invariance also holds for the probit link", {

  res <- lapply(binomFits("probit"), pseudoR2, which = "all")

  expect_equal(res$single, res$matrix, tolerance = 1e-6)
  expect_equal(res$weights, res$matrix, tolerance = 1e-6)
})


test_that("the likelihood is the one of the single trials", {

  fits <- binomFits()

  res <- pseudoR2(fits$matrix, which = c("logLik", "logLik0", "AIC", "BIC"))
  ref <- pseudoR2(fits$single, which = c("logLik", "logLik0", "AIC", "BIC"))

  # the disaggregated fit is the reference stats::logLik() agrees with
  expect_equal(unname(ref["logLik"]), as.numeric(logLik(fits$single)))
  expect_equal(unname(ref["AIC"]), AIC(fits$single))
  expect_equal(unname(ref["BIC"]), BIC(fits$single))

  expect_equal(res, ref, tolerance = 1e-6)
})


test_that("the aggregated fit differs from stats by the binomial constant", {

  fit <- binomFits()$matrix

  const <- with(groupedBinom, sum(lchoose(total, survive)))

  expect_equal(
    as.numeric(logLik(fit)) - unname(pseudoR2(fit, "logLik")),
    const
  )

  expect_gt(const, 0)
})


test_that("frequency weights on a binary response behave like repeated rows", {

  d <- aggregate(list(w = rep(1, nrow(expandedBinom))), expandedBinom, length)

  weighted <- glm(survive ~ location + period, family = binomial,
                  data = d, weights = w)

  expect_equal(
    pseudoR2(weighted, which = "all"),
    pseudoR2(binomFits()$single, which = "all"),
    tolerance = 1e-6
  )
})


test_that("cells without trials are ignored", {

  expect_true(any(groupedBinom$total == 0))

  fit <- binomFits()$matrix
  dropped <- glm(cbind(survive, total - survive) ~ location + period,
                 family = binomial, data = subset(groupedBinom, total > 0))

  expect_equal(pseudoR2(dropped, which = "all"), pseudoR2(fit, which = "all"),
               tolerance = 1e-6)
})




# == the measures themselves ===============================================

test_that("the likelihood based measures follow their definitions", {

  fit <- binaryFit()
  res <- pseudoR2(fit, which = "all")

  l1 <- as.numeric(logLik(fit))
  l0 <- nullLogLikByUpdate(fit)
  n <- nobs(fit)
  edf <- attr(logLik(fit), "df")

  g2 <- 2 * (l1 - l0)
  coxSnell <- 1 - exp(-g2 / n)

  expect_equal(unname(res["logLik"]), l1)
  expect_equal(unname(res["logLik0"]), l0)
  expect_equal(unname(res["G2"]), g2)
  expect_equal(unname(res["McFadden"]), 1 - l1 / l0)
  expect_equal(unname(res["McFaddenAdj"]), 1 - (l1 - edf) / l0)
  expect_equal(unname(res["CoxSnell"]), coxSnell)
  expect_equal(unname(res["Nagelkerke"]), coxSnell / (1 - exp(2 * l0 / n)))
  expect_equal(unname(res["AldrichNelson"]), g2 / (g2 + n))
  expect_equal(
    unname(res["VeallZimmermann"]),
    (g2 / (g2 + n)) * (2 * l0 - n) / (2 * l0)
  )
  expect_equal(unname(res["AIC"]), AIC(fit))
  expect_equal(unname(res["BIC"]), BIC(fit))
})


test_that("G2 is the deviance difference of the glm", {

  for (fit in list(binaryFit(),
                   binomFits()$matrix,
                   glm(count ~ spray, family = poisson, data = InsectSprays))) {

    expect_equal(
      unname(pseudoR2(fit, "G2")),
      fit$null.deviance - fit$deviance
    )
  }
})


test_that("McKelveyZavoina follows its definition and needs a latent scale", {

  fit <- binaryFit()
  eta <- fit$linear.predictors
  sse <- sum((eta - mean(eta))^2)

  expect_equal(
    unname(pseudoR2(fit, "McKelveyZavoina")),
    sse / (nobs(fit) * pi^2 / 3 + sse)
  )

  probit <- binaryFit("probit")
  etaP <- probit$linear.predictors
  sseP <- sum((etaP - mean(etaP))^2)

  expect_equal(
    unname(pseudoR2(probit, "McKelveyZavoina")),
    sseP / (nobs(probit) * 1 + sseP)
  )

  # no latent variance for the remaining links
  for (link in c("cloglog", "cauchit")) {
    expect_false(
      "McKelveyZavoina" %in% names(pseudoR2(binaryFit(link), which = "all"))
    )
  }

  poisson <- glm(count ~ spray, family = poisson, data = InsectSprays)
  expect_false("McKelveyZavoina" %in% names(pseudoR2(poisson, which = "all")))
})


test_that("McKelveyZavoina counts trials, not rows", {

  fits <- binomFits()

  expect_equal(
    pseudoR2(fits$matrix, "McKelveyZavoina"),
    pseudoR2(fits$single, "McKelveyZavoina"),
    tolerance = 1e-6
  )

  # the row-wise version would be an entirely different number
  eta <- fits$matrix$linear.predictors
  sse <- sum((eta - mean(eta))^2)
  rowWise <- sse / (nrow(groupedBinom) * pi^2 / 3 + sse)

  expect_false(isTRUE(all.equal(
    rowWise, unname(pseudoR2(fits$matrix, "McKelveyZavoina"))
  )))
})


test_that("Efron uses the residual to total sum of squares ratio", {

  fit <- binaryFit()
  y <- fit$y
  yhat <- fitted(fit)

  expect_equal(
    unname(pseudoR2(fit, "Efron")),
    1 - sum((y - yhat)^2) / sum((y - mean(y))^2)
  )

  # for a gaussian glm it is the R squared of the corresponding lm
  gaussian <- glm(mpg ~ wt + hp, data = mtcars)

  expect_equal(
    unname(pseudoR2(gaussian, "Efron")),
    summary(lm(mpg ~ wt + hp, data = mtcars))$r.squared
  )
})


test_that("Efron keeps the within cell variation of aggregated data", {

  fits <- binomFits()

  expect_equal(pseudoR2(fits$matrix, "Efron"), pseudoR2(fits$single, "Efron"),
               tolerance = 1e-6)

  # naively treating the proportions as observations inflates the measure,
  # the variation within the cells is missing on both sides of the ratio
  keep <- groupedBinom$total > 0
  prop <- groupedBinom$prop[keep]
  mu <- fitted(fits$matrix)[keep]

  naive <- 1 - sum((prop - mu)^2) / sum((prop - mean(prop))^2)

  expect_gt(naive, unname(pseudoR2(fits$matrix, "Efron")))
})


test_that("Tjur is the difference of the mean fitted probabilities", {

  fit <- binaryFit()

  expect_equal(
    unname(pseudoR2(fit, "Tjur")),
    unname(diff(tapply(fitted(fit), fit$y, mean)))
  )

  # and the same for aggregated data, where the successes are counted
  fits <- binomFits()

  expect_equal(pseudoR2(fits$matrix, "Tjur"), pseudoR2(fits$single, "Tjur"),
               tolerance = 1e-6)
  expect_equal(pseudoR2(fits$weights, "Tjur"), pseudoR2(fits$single, "Tjur"),
               tolerance = 1e-6)
})


test_that("Tjur is not defined without a binomial response", {

  poisson <- glm(count ~ spray, family = poisson, data = InsectSprays)
  gaussian <- glm(mpg ~ wt, data = mtcars)

  expect_false("Tjur" %in% names(pseudoR2(poisson, which = "all")))
  expect_false("Tjur" %in% names(pseudoR2(gaussian, which = "all")))
  expect_error(pseudoR2(poisson, "Tjur"), "not defined for this fit")
})


test_that("the selected example fits yield measures inside the unit interval", {

  fits <- list(
    binaryFit(),
    binaryFit("probit"),
    binomFits()$matrix,
    glm(count ~ spray, family = poisson, data = InsectSprays),
    glm(mpg ~ wt + hp, data = mtcars)
  )

  r2 <- setdiff(.pseudoR2Measures, c("AIC", "BIC", "logLik", "logLik0", "G2"))

  for (fit in fits) {

    res <- pseudoR2(fit, which = "all")
    res <- res[intersect(r2, names(res))]

    expect_true(all(res >= 0 & res <= 1))
  }
})


test_that("a model without any effect is at zero, a perfect one at one", {

  d <- data.frame(y = rep(0:1, 50), x = rep(0:1, each = 50))

  empty <- glm(y ~ 1, family = binomial, data = transform(d, x = NULL))
  res <- pseudoR2(empty, which = "all")

  expect_equal(unname(res["McFadden"]), 0)
  expect_equal(unname(res["Tjur"]), 0)
  expect_equal(unname(res["G2"]), 0)
  expect_equal(unname(res["logLik"]), unname(res["logLik0"]))

  perfect <- suppressWarnings(
    glm(x ~ factor(seq_len(100) > 50), family = binomial,
        data = data.frame(x = rep(0:1, each = 50)))
  )
  res <- pseudoR2(perfect, which = "all")

  expect_equal(unname(res["McFadden"]), 1, tolerance = 1e-6)
  expect_equal(unname(res["Tjur"]), 1, tolerance = 1e-6)
  expect_equal(unname(res["Efron"]), 1, tolerance = 1e-6)
})




# == the null model ========================================================

test_that("logLik0 is the log-likelihood of the intercept-only fit", {

  skip_if_not_installed("MASS")

  for (fit in list(binaryFit(),
                   binaryFit("probit"),
                   glm(count ~ spray, family = poisson, data = InsectSprays),
                   glm(mpg ~ wt + hp, data = mtcars),
                   glm(Days ~ Sex, family = Gamma, data = MASS::quine,
                       subset = Days > 0))) {

    expect_equal(
      unname(pseudoR2(fit, "logLik0")),
      nullLogLikByUpdate(fit)
    )
  }
})


test_that("the null model keeps the offset of the original fit", {

  d <- data.frame(
    count = c(12, 9, 14, 7, 21, 18, 6, 11),
    x = rep(0:1, 4),
    time = c(10, 8, 12, 6, 20, 15, 5, 9)
  )

  fit <- glm(count ~ x + offset(log(time)), family = poisson, data = d)
  fit0 <- glm(count ~ 1 + offset(log(time)), family = poisson, data = d)

  expect_equal(unname(pseudoR2(fit, "logLik0")), as.numeric(logLik(fit0)))

  # dropping the offset in the null model would show up here
  naive <- glm(count ~ 1, family = poisson, data = d)
  expect_false(isTRUE(all.equal(
    unname(pseudoR2(fit, "logLik0")), as.numeric(logLik(naive))
  )))
})


test_that("glm needs neither the response nor the data of the original call", {

  fit <- local({
    d <- mtcars
    glm(vs ~ mpg + wt, family = binomial, data = d, y = FALSE, model = FALSE)
  })

  # the update() route, which the other classes take, would fail here
  expect_error(update(fit, . ~ 1), "not found")

  expect_equal(pseudoR2(fit, which = "all"), pseudoR2(binaryFit(), "all"))
})


test_that("missing values are dropped in the fit and in the null model", {

  fit <- glm(vs ~ mpg + wt, family = binomial, data = mtcarsNA)
  complete <- glm(vs ~ mpg + wt, family = binomial, data = na.omit(mtcarsNA))

  expect_equal(nobs(fit), nrow(mtcars) - 3L)
  expect_equal(pseudoR2(fit, which = "all"), pseudoR2(complete, which = "all"))

  # the null model is fitted on the rows the model used, while update() would
  # bring the incomplete rows back in, the response having no missings here
  expect_equal(
    unname(pseudoR2(fit, "logLik0")),
    as.numeric(logLik(glm(vs ~ 1, family = binomial, data = na.omit(mtcarsNA))))
  )

  expect_false(isTRUE(all.equal(
    unname(pseudoR2(fit, "logLik0")), nullLogLikByUpdate(fit)
  )))
})




# == other model classes ===================================================

test_that("polr is supported through its core measures", {

  skip_if_not_installed("MASS")

  fit <- MASS::polr(factor(gear) ~ mpg + wt, data = mtcars, Hess = TRUE)
  res <- pseudoR2(fit, which = "all")

  expect_equal(unname(res["logLik"]), as.numeric(logLik(fit)))
  expect_equal(unname(res["logLik0"]), nullLogLikByUpdate(fit))
  expect_equal(
    unname(res["McFadden"]),
    1 - as.numeric(logLik(fit)) / nullLogLikByUpdate(fit)
  )

  # measures needing a linear predictor are not available here
  expect_false(any(c("McKelveyZavoina", "Efron", "Tjur") %in% names(res)))
  expect_false(any(c("AldrichNelson", "VeallZimmermann") %in% names(res)))
})


test_that("multinom is supported through its core measures", {

  skip_if_not_installed("nnet")

  fit <- nnet::multinom(factor(gear) ~ mpg + wt, data = mtcars, trace = FALSE)
  res <- pseudoR2(fit, which = "all")

  expect_equal(unname(res["logLik"]), as.numeric(logLik(fit)))
  expect_equal(
    unname(res["McFadden"]),
    1 - as.numeric(logLik(fit)) / unname(res["logLik0"])
  )
  expect_equal(unname(res["G2"]),
               -fit$deviance - 2 * unname(res["logLik0"]),
               tolerance = 1e-6)
})


test_that("vglm reproduces the glm of the same model", {

  skip_if_not_installed("VGAM")

  fit <- VGAM::vglm(vs ~ mpg + wt, family = VGAM::binomialff,
                    data = mtcars, model = TRUE)

  measures <- setdiff(.pseudoR2Measures, c("AIC", "BIC"))

  expect_equal(
    pseudoR2(fit, which = "all")[measures],
    pseudoR2(binaryFit(), which = "all")[measures],
    tolerance = 1e-6
  )
})


test_that("vglm is invariant to aggregation as well", {

  skip_if_not_installed("VGAM")

  grouped <- suppressWarnings(
    VGAM::vglm(cbind(survive, total - survive) ~ location + period,
               family = VGAM::binomialff, model = TRUE,
               data = subset(groupedBinom, total > 0))
  )

  expect_equal(
    suppressWarnings(pseudoR2(grouped, which = "all")),
    pseudoR2(binomFits()$matrix, which = "all"),
    tolerance = 1e-6
  )
})


test_that("the link of a vglm is recognised under both namings", {

  skip_if_not_installed("VGAM")

  # VGAM takes the link unevaluated, hence the detour through its namespace
  vglmFit <- function(link) {
    fam <- eval(call("binomialff", link = as.name(link)), asNamespace("VGAM"))

    # do.call inlines the family, so that update() does not look for it
    do.call(VGAM::vglm, list(vs ~ mpg + wt, family = fam,
                             data = quote(mtcars), model = TRUE))
  }

  logit <- vglmFit("logitlink")
  probit <- vglmFit("probitlink")

  expect_equal(.linkName(logit), "logit")
  expect_equal(.linkName(probit), "probit")

  expect_true("McKelveyZavoina" %in% names(pseudoR2(logit, which = "all")))
  expect_true("McKelveyZavoina" %in% names(pseudoR2(probit, which = "all")))

  # the names VGAM used before 1.1-0
  old <- logit
  old@misc$link <- "logit"
  expect_equal(.linkName(old), "logit")

  cloglog <- logit
  cloglog@misc$link <- "clogloglink"
  expect_equal(.linkName(cloglog), NA_character_)
})


test_that("quasi families keep what does not need a likelihood", {

  fit <- glm(cbind(survive, total - survive) ~ location + period,
             family = quasibinomial, data = groupedBinom)

  res <- suppressWarnings(pseudoR2(fit, which = "all"))

  expect_true(all(is.na(res[c("McFadden", "CoxSnell", "logLik", "G2")])))

  # the quasi fit has the same coefficients, hence the same latent measures
  ref <- pseudoR2(binomFits()$matrix, which = "all")

  expect_equal(res[c("McKelveyZavoina", "Efron", "Tjur")],
               ref[c("McKelveyZavoina", "Efron", "Tjur")])
})




# == internal helpers ======================================================

test_that(".bernoulliLogLik is the binomial likelihood without its constant", {

  d <- subset(groupedBinom, total > 0)

  mu <- plogis(seq(-2, 2, length.out = nrow(d)))

  expect_equal(
    .bernoulliLogLik(d$prop, mu, d$total),
    sum(dbinom(d$survive, d$total, mu, log = TRUE)) -
      sum(lchoose(d$total, d$survive))
  )

  # 0 * log(0) counts as 0, at both ends
  expect_equal(.bernoulliLogLik(c(0, 1), c(0, 1), c(5, 5)), 0)
  expect_equal(.bernoulliLogLik(0, 1, 1), -Inf)
})


test_that(".glmResponse recovers a dropped response", {

  fits <- binomFits()

  expect_equal(.glmResponse(fits$noResponse), .glmResponse(fits$matrix))

  binary <- binaryFit(y = FALSE)

  expect_equal(.glmResponse(binary), binaryFit()$y)
  expect_true(all(.glmResponse(binary) %in% c(0, 1)))
})


test_that(".matchMeasures resolves names the way pseudoR2 promises", {

  expect_equal(.matchMeasures("Tjur"), "Tjur")
  expect_equal(.matchMeasures(c("logLik", "G2")), c("logLik", "G2"))

  # logLik and logLik0 share their prefix, the exact name still wins
  expect_error(.matchMeasures("logLi"), "ambiguous")
  expect_equal(.matchMeasures("logLik0"), "logLik0")
  expect_error(.matchMeasures("tjur"), "unknown measure")
})


test_that(".familyName and .priorWeights read a glm", {

  fit <- binomFits()$matrix

  expect_equal(.familyName(fit, "glm"), "binomial")
  expect_equal(.familyName(glm(mpg ~ wt, data = mtcars), "glm"), "gaussian")

  expect_equal(.priorWeights(fit, "glm"), groupedBinom$total)
  expect_equal(.priorWeights(binaryFit(), "glm"), rep(1, nrow(mtcars)))
})


test_that(".getModelInfo reports the trial count as effective sample size", {

  info <- .getModelInfo(binomFits()$matrix)

  expect_equal(info$n, nrow(groupedBinom))
  expect_equal(info$nEff, sum(groupedBinom$total))

  info <- .getModelInfo(binaryFit())

  expect_equal(info$n, nrow(mtcars))
  expect_equal(info$nEff, nrow(mtcars))
})


# == additional regression tests ===========================================

test_that("factor and logical responses agree with numeric binary responses", {

  d <- mtcars
  d$logical <- d$vs == 1
  d$category <- factor(d$vs, levels = c(0, 1), labels = c("no", "yes"))

  ref <- pseudoR2(binaryFit(), "all")
  logicalFit <- glm(logical ~ mpg + wt, data = d, family = binomial)
  factorFit <- glm(category ~ mpg + wt, data = d, family = binomial)

  expect_equal(pseudoR2(logicalFit, "all"), ref)
  expect_equal(pseudoR2(factorFit, "all"), ref)
})


test_that("reversing the binary outcome leaves the measures unchanged", {

  reversed <- glm(I(1 - vs) ~ mpg + wt, data = mtcars, family = binomial)

  expect_equal(pseudoR2(reversed, "all"), pseudoR2(binaryFit(), "all"),
               tolerance = 1e-6)
})


test_that("aliased coefficients do not increase the parameter penalty", {

  d <- transform(mtcars, mpgDuplicate = 2 * mpg)
  fit <- glm(vs ~ mpg + wt + mpgDuplicate, data = d, family = binomial)

  expect_true(anyNA(coef(fit)))
  expect_equal(pseudoR2(fit, "all"), pseudoR2(binaryFit(), "all"),
               tolerance = 1e-6)
})


test_that("aggregation invariance survives offsets and a dropped model frame", {

  grouped <- transform(groupedBinom,
                       off = 0.15 * as.integer(location) * as.integer(period))
  single <- transform(expandedBinom,
                      off = 0.15 * as.integer(location) * as.integer(period))

  fit <- glm(cbind(survive, total - survive) ~ location + period + offset(off),
             data = grouped, family = binomial, y = FALSE, model = FALSE)
  ref <- glm(survive ~ location + period, offset = off,
             data = single, family = binomial)
  null <- glm(survive ~ 1, offset = off, data = single, family = binomial)

  expect_equal(pseudoR2(fit, "all"), pseudoR2(ref, "all"), tolerance = 1e-6)
  expect_equal(unname(pseudoR2(fit, "logLik0")), as.numeric(logLik(null)),
               tolerance = 1e-6)
})


test_that("na.exclude and a dropped response retain only the fitted rows", {

  # residuals.glm() pads excluded rows, whereas fitted.values is unpadded.
  # The response reconstruction must account for that difference.
  fit <- glm(vs ~ mpg + wt, family = binomial, data = mtcarsNA,
             na.action = na.exclude, y = FALSE, model = FALSE)
  ref <- glm(vs ~ mpg + wt, family = binomial, data = na.omit(mtcarsNA))

  expect_length(.glmResponse(fit), nobs(fit))
  expect_equal(unname(.glmResponse(fit)), unname(ref$y))
  expect_equal(pseudoR2(fit, "all"), pseudoR2(ref, "all"))
})


test_that("polr fits its null model on the same complete cases", {

  skip_if_not_installed("MASS")

  # Missingness is confined to predictors, so update(. ~ 1) can restore rows.
  fit <- MASS::polr(factor(gear) ~ mpg + wt, data = mtcarsNA, Hess = TRUE)
  ref <- MASS::polr(factor(gear) ~ mpg + wt,
                    data = na.omit(mtcarsNA), Hess = TRUE)
  null <- MASS::polr(factor(gear) ~ 1,
                     data = na.omit(mtcarsNA), Hess = TRUE)

  expect_equal(nobs(fit), nobs(ref))
  expect_equal(unname(pseudoR2(fit, "logLik0")), as.numeric(logLik(null)),
               tolerance = 1e-6)
  expect_equal(pseudoR2(fit, "all"), pseudoR2(ref, "all"), tolerance = 1e-6)
})


test_that("polr keeps its sample without a stored model frame", {

  skip_if_not_installed("MASS")

  # model.frame.polr() passes model = FALSE on to model.frame(), which takes
  # it for a variable of length one; the frame has to be rebuilt without it
  fit <- MASS::polr(factor(gear) ~ mpg + wt, data = mtcars, Hess = TRUE,
                    model = FALSE)
  null <- MASS::polr(factor(gear) ~ 1, data = mtcars, Hess = TRUE)

  expect_equal(unname(pseudoR2(fit, "logLik0")), as.numeric(logLik(null)),
               tolerance = 1e-6)
})


test_that("a dropped frame and missing predictors do not widen the sample", {

  skip_if_not_installed("MASS")

  # the combination of both: no frame to freeze the sample with, and rows
  # that update(. ~ 1) would bring back, the response having no missings
  complete <- na.omit(mtcarsNA[, c("gear", "mpg", "wt")])

  fit <- MASS::polr(factor(gear) ~ mpg + wt, data = mtcarsNA, Hess = TRUE,
                    model = FALSE)
  null <- MASS::polr(factor(gear) ~ 1, data = complete, Hess = TRUE)
  wide <- MASS::polr(factor(gear) ~ 1, data = mtcarsNA, Hess = TRUE)

  expect_equal(nobs(fit), nrow(complete))
  expect_equal(unname(pseudoR2(fit, "logLik0")), as.numeric(logLik(null)),
               tolerance = 1e-6)

  # the null model fitted on all rows would be the silent error here
  expect_false(isTRUE(all.equal(
    unname(pseudoR2(fit, "logLik0")), as.numeric(logLik(wide))
  )))
})


test_that("subset and weights survive a rebuilt model frame", {

  skip_if_not_installed("MASS")

  d <- transform(mtcarsNA, w = rep(c(1, 2), 16))

  fit <- MASS::polr(factor(gear) ~ mpg + wt, data = d, subset = cyl > 4,
                    weights = w, Hess = TRUE, model = FALSE)
  complete <- na.omit(subset(d, cyl > 4)[, c("gear", "mpg", "wt", "w")])

  null <- MASS::polr(factor(gear) ~ 1, data = complete, weights = w,
                     Hess = TRUE)

  expect_equal(unname(pseudoR2(fit, "logLik0")), as.numeric(logLik(null)),
               tolerance = 1e-6)
})


test_that("multinom fits its null model on the same complete cases", {

  skip_if_not_installed("nnet")

  fit <- nnet::multinom(factor(gear) ~ mpg + wt, data = mtcarsNA, trace = FALSE)
  ref <- nnet::multinom(factor(gear) ~ mpg + wt,
                        data = na.omit(mtcarsNA), trace = FALSE)
  null <- nnet::multinom(factor(gear) ~ 1,
                         data = na.omit(mtcarsNA), trace = FALSE)

  expect_equal(as.numeric(logLik(fit)), as.numeric(logLik(ref)))
  expect_equal(unname(pseudoR2(fit, "logLik0")), as.numeric(logLik(null)),
               tolerance = 1e-6)
  expect_equal(pseudoR2(fit, "all"), pseudoR2(ref, "all"), tolerance = 1e-6)
})


test_that("adjusted McFadden may be negative for an intercept-only model", {

  fit <- glm(vs ~ 1, data = mtcars, family = binomial)
  res <- pseudoR2(fit, c("McFadden", "McFaddenAdj"))

  expect_equal(unname(res["McFadden"]), 0)
  expect_lt(unname(res["McFaddenAdj"]), 0)
})


test_that("zero-weight observations cannot contaminate Bernoulli likelihood", {

  # Even an impossible prediction contributes nothing when its weight is zero.
  # Multiplying a precomputed -Inf by zero would produce NaN.
  expect_equal(.bernoulliLogLik(c(1, 0), c(0, 0.25), c(0, 2)),
               2 * log(0.75))
  expect_equal(.bernoulliLogLik(c(0, 1), c(1, 0.75), c(0, 2)),
               2 * log(0.75))
})
