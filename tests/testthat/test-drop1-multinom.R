# Tests for the term-wise likelihood-ratio tests of multinomial models

lrCols <- c("LR stat.", "p-value")

# reference: explicit refit of the reduced model and its LR test
refRow <- function(full, reduced) {
  lr <- reduced$deviance - full$deviance
  c(reduced$edf, reduced$AIC, lr,
    pchisq(lr, full$edf - reduced$edf, lower.tail = FALSE))
}


test_that("drop1 matches explicit refits", {
  fit <- fitMod(ice_cream ~ video + puzzle + female, data = IceCream,
                engine = "multinom")
  d1  <- fit$drop1

  expect_identical(rownames(d1), c("<none>", "video", "puzzle", "female"))
  expect_identical(names(d1), c("Df", "AIC", lrCols))
  expect_equal(unlist(d1["<none>", c("Df", "AIC")], use.names = FALSE),
               c(fit$edf, fit$AIC))

  for (term in c("video", "puzzle", "female")) {
    red <- nnet::multinom(
      update(ice_cream ~ video + puzzle + female,
             as.formula(paste(". ~ . -", term))),
      data = IceCream, maxit = 500, trace = FALSE
    )
    expect_equal(unlist(d1[term, ], use.names = FALSE), refRow(fit, red),
                 tolerance = 1e-5)
  }
})


test_that("transformed terms no longer stop fitMod()", {
  fit <- fitMod(ice_cream ~ log(video) + sqrt(puzzle) + factor(female),
                data = IceCream, engine = "multinom")
  red <- nnet::multinom(ice_cream ~ log(video) + factor(female),
                        data = IceCream, maxit = 500, trace = FALSE)

  expect_identical(rownames(fit$drop1),
                   c("<none>", "log(video)", "sqrt(puzzle)",
                     "factor(female)"))
  expect_equal(unlist(fit$drop1["sqrt(puzzle)", ], use.names = FALSE),
               refRow(fit, red), tolerance = 1e-5)
})


test_that("drop1 uses the observations and weights of the full fit", {
  d <- IceCream
  d$w <- rep(1:3, length.out = nrow(d))
  # the NA sits in the term that is dropped
  d$puzzle[5L] <- NA

  fit <- fitMod(ice_cream ~ video + puzzle, data = d, engine = "multinom",
                weights = w, subset = video > 30)
  red <- nnet::multinom(ice_cream ~ video,
                        data = subset(d, video > 30 & !is.na(puzzle)),
                        weights = w, maxit = 500, trace = FALSE)

  expect_equal(unlist(fit$drop1["puzzle", ], use.names = FALSE),
               refRow(fit, red), tolerance = 1e-5)
})


test_that("drop1 does not depend on where the data live", {
  fitLocal <- function() {
    local <- IceCream
    fitMod(ice_cream ~ log(video) + female, data = local,
           engine = "multinom")
  }
  expect_false(anyNA(fitLocal()$drop1[-1L, ]))
})


test_that("drop1 respects marginality and the test argument", {
  fit <- fitMod(ice_cream ~ video * female, data = IceCream,
                engine = "multinom")
  expect_identical(rownames(fit$drop1), c("<none>", "video:female"))
  expect_named(.drop1.multinom(fit, test = "none"), c("Df", "AIC"))
})
