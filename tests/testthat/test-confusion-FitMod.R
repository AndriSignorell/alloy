
# tests for confusion.FitMod() -------------------------------------------------

fit <- fitMod(admit ~ gre + gpa + rank, Admit, engine = "logit")


test_that("a logit model gives the same result as the glm method", {

  g <- glm(admit ~ gre + gpa + rank, binomial, Admit)

  a <- confusion(fit)
  b <- confusion(g)

  expect_s3_class(a, "Confusion")
  expect_identical(a$pos, "1")
  expect_equal(unclass(a$table), unclass(b$table), ignore_attr = TRUE)
  expect_equal(a$byclass, b$byclass)

  expect_equal(confusion(fit, cutoff = 0.3)$byclass,
               confusion(g, cutoff = 0.3)$byclass)

})


test_that("the cutoff and the positive class are honoured", {

  p <- predict(fit)[["1"]]

  cm <- confusion(fit, cutoff = 0.3)
  expect_identical(sum(cm$table["1", ]), sum(p > 0.3))

  cm <- confusion(fit, pos = "0")
  expect_identical(cm$pos, "0")
  expect_identical(rownames(cm$table)[1L], "0")

})


test_that("new data are classified and compared with their response", {

  set.seed(1)
  d <- splitTrainTest(Admit, p = 0.3)
  m <- update(fit, data = d$train)

  cm <- confusion(m, newdata = d$test)
  expect_identical(cm$n, nrow(d$test))

  p <- predict(m, newdata = d$test)[["1"]]
  expect_identical(cm$diag,
                   sum((p > 0.5) == (d$test$admit == 1L)))

})


test_that("multiclass and other engines work, regression does not", {

  cm <- confusion(fitMod(Species ~ ., iris, engine = "multinom"))
  expect_identical(dim(cm$table), c(3L, 3L))
  expect_null(cm$pos)

  skip_if_not_installed("rpart")
  tree <- fitMod(Species ~ ., iris, engine = "rpart")
  expect_equal(confusion(tree)$acc,
               mean(predict(tree, output = "class")$class == iris$Species))

  expect_error(confusion(fitMod(Fertility ~ ., swiss, engine = "lm")),
               "needs a classification model")

})


test_that("Firth models are covered", {

  skip_if_not_installed("brglm2")

  f <- fitMod(admit ~ gre + gpa + rank, Admit, engine = "logitFirth")
  p <- predict(f)[["1"]]
  expect_identical(confusion(f)$diag,
                   sum((p > 0.5) == (Admit$admit == 1L)))

})
