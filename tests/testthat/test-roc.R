
# tests for roc() and rocTest() ------------------------------------------------

skip_if_not_installed("pROC")

fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, engine = "logit")


test_that("roc() of a FitMod uses P(second class) with a fixed direction", {

  expect_no_warning(r <- roc(fitLogit, quiet = TRUE))

  p <- predict(fitLogit)[, 2L]
  expect_equal(as.numeric(r$auc), cStat(p, Admit$admit))
  expect_identical(r$direction, "<")
  expect_identical(r$levels, c("0", "1"))

  # a model worse than chance stays worse than chance
  d <- Admit
  d$admit <- 1L - d$admit
  rRev <- roc(p, ref = d$admit, levels = c(0, 1), direction = "<",
              quiet = TRUE)
  expect_lt(as.numeric(rRev$auc), 0.5)

  # arguments in ... override the defaults
  expect_identical(roc(fitLogit, direction = ">", quiet = TRUE)$direction,
                   ">")

  # a logical response is coded like predict.FitMod() codes it
  d <- Admit
  d$admit <- d$admit == 1L
  fitL <- fitMod(admit ~ gre + gpa + rank, d, engine = "logit")
  expect_equal(as.numeric(roc(fitL, quiet = TRUE)$auc), as.numeric(r$auc))

})


test_that("roc() of a crossVal() uses the out-of-fold predictions", {

  set.seed(1)
  cv <- crossVal(fitLogit, k = 5)
  r <- roc(cv, quiet = TRUE)

  ord <- order(cv$pred$row)
  expect_equal(as.numeric(r$auc),
               cStat(cv$pred$pred[ord], as.integer(cv$pred$obs[ord]) - 1L))
  # one repetition: the pooled AUC of the summary
  expect_equal(as.numeric(r$auc), cv$summary["auc", "pooled"])
  expect_identical(r$direction, "<")
  expect_length(r$response, 400L)

  # repeats: one entry per observation, its mean prediction
  set.seed(1)
  cv3 <- crossVal(fitLogit, k = 5, repeats = 3)
  r3 <- roc(cv3, quiet = TRUE)
  pm <- tapply(cv3$pred$pred, cv3$pred$row, mean)
  expect_length(r3$response, 400L)
  expect_equal(as.numeric(r3$auc), cStat(as.numeric(pm), Admit$admit))

  expect_error(roc(cv, ref = Admit$admit), "'ref' cannot be given")

  set.seed(1)
  cvR <- crossVal(fitMod(Fertility ~ ., swiss, engine = "lm"), k = 5)
  expect_error(roc(cvR), "binary classification")

})


test_that("roc() leaves out observations of failed folds, with a warning", {

  d <- Admit
  d$rank <- as.character(d$rank)
  d$rank[1L] <- "5"
  d$rank <- factor(d$rank)
  fit <- fitMod(admit ~ gre + rank, d, engine = "logit")

  set.seed(3)
  cv <- suppressWarnings(crossVal(fit, k = 5))
  expect_warning(r <- roc(cv, quiet = TRUE), "no out-of-fold prediction")
  expect_length(r$response, sum(cv$reps$nPredicted))

})


test_that("rocTest() is roc.test() and accepts models and crossVal()s", {

  idx <- seq(1, nrow(Admit), by = 2)
  dev <- Admit[idx, ]
  ext <- Admit[-idx, ]
  fit <- fitMod(admit ~ gre + gpa + rank, dev, engine = "logit")

  rDev <- roc(fit, quiet = TRUE)
  rExt <- roc(predict(fit, newdata = ext)[, 2L], ref = ext$admit,
              levels = c(0, 1), direction = "<", quiet = TRUE)

  # unpaired DeLong, identical to pROC
  tt <- rocTest(rDev, rExt)
  expect_s3_class(tt, "htest")
  expect_equal(tt, pROC::roc.test(rDev, rExt))
  expect_match(tt$method, "DeLong")
  expect_false(grepl("correlated", tt$method))
  expect_identical(tt$data.names, "rDev and rExt")

  # a FitMod is converted with roc()
  expect_equal(rocTest(fit, rExt)$p.value, tt$p.value)

  # two models cross-validated on the same sample: paired
  set.seed(1)
  cvFull <- crossVal(fitLogit, k = 5)
  set.seed(1)
  cvSmall <- crossVal(fitMod(admit ~ gpa, Admit, engine = "logit"), k = 5)

  tp <- rocTest(cvFull, cvSmall)
  expect_match(tp$method, "DeLong")
  expect_match(tp$method, "correlated")
  expect_equal(unname(tp$estimate),
               c(as.numeric(roc(cvFull, quiet = TRUE)$auc),
                 as.numeric(roc(cvSmall, quiet = TRUE)$auc)))

  # arguments reach roc.test()
  expect_match(rocTest(cvFull, cvSmall, alternative = "greater")$alternative,
               "greater")

  expect_error(rocTest(1:3, rExt), "'roc1' must be an ROC curve")
  expect_error(rocTest(rDev, "x"), "'roc2' must be an ROC curve")

})
