# predictSql(): the SQL is run in an in-memory database and compared with
# predict.FitMod() on the same data.

skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

# SQLite: its LOG() is the natural logarithm, and LN() may be missing
sqlBackend <- function()
  list(con = DBI::dbConnect(RSQLite::SQLite()),
       fix = function(sql) gsub("LN(", "LOG(", sql, fixed = TRUE))

# predictions of predictSql() for 'data', in the order of the rows
sqlPredict <- function(fit, data, ..., id = "ROW_ID") {
  db <- sqlBackend()
  on.exit(DBI::dbDisconnect(db$con))
  d <- data
  d[] <- lapply(d, function(v) if (is.factor(v)) as.character(v) else v)
  d$ROW_ID <- seq_len(nrow(d))
  DBI::dbWriteTable(db$con, "T1", d)
  sql <- predictSql(fit, "T1", ..., statement = "select", id = id)
  res <- DBI::dbGetQuery(db$con, db$fix(as.character(sql)))
  res <- res[order(res$ROW_ID), , drop = FALSE]
  res[, setdiff(names(res), names(d)), drop = FALSE]
}

cars <- transform(mtcars,
                  cyl = factor(cyl), gear = factor(gear),
                  am  = factor(am, labels = c("auto", "manual")))
# scoring data with a missing value and a level the models do not know
score <- cars
score$wt[2L]  <- NA
score$cyl     <- as.character(score$cyl)
score$cyl[5L] <- "12"
scoreR <- transform(score, cyl = factor(cyl, levels = levels(cars$cyl)))


test_that("linear models: transformations, interactions, contrasts, offset", {
  fit <- fitMod(mpg ~ wt * cyl + log(hp) + I(disp^2) + sqrt(qsec) +
                  cut(drat, c(0, 3.5, 4, Inf)) + offset(carb / 10),
                cars, engine = "lm")
  expect_equal(sqlPredict(fit, score)$PRED,
               unname(predict(fit, newdata = scoreR)))

  fit <- fitMod(mpg ~ 0 + cyl + gear + cyl:wt, cars, engine = "lm",
                contrasts = list(cyl = "contr.sum", gear = "contr.helmert"))
  expect_equal(sqlPredict(fit, score)$PRED,
               unname(predict(fit, newdata = scoreR)))
})


test_that("NULL and unknown levels yield NULL, not the reference level", {
  fit <- fitMod(mpg ~ wt + cyl, cars, engine = "lm")
  expect_identical(which(is.na(sqlPredict(fit, score)$PRED)), c(2L, 5L))
})


test_that("logit: output and type as in predict.FitMod()", {
  fit <- fitMod(am ~ wt + hp, cars, engine = "logit")

  res <- sqlPredict(fit, score, output = "both")
  ref <- predict(fit, newdata = scoreR, output = "both")
  expect_named(res, c("P_auto", "P_manual", "PRED_CLASS"))
  expect_equal(res$P_manual, ref$manual)
  expect_identical(res$PRED_CLASS, as.character(ref$class))

  for (type in c("link", "response"))
    expect_equal(sqlPredict(fit, score, type = type)$PRED,
                 unname(predict(fit, newdata = scoreR, type = type)))
})


test_that("glm families and links", {
  fit <- fitMod(carb ~ wt + cyl + offset(log(hp)), cars, engine = "poisson")
  expect_equal(sqlPredict(fit, score)$PRED,
               unname(predict(fit, newdata = scoreR)))
  expect_equal(sqlPredict(fit, score, type = "link")$PRED,
               unname(predict(fit, newdata = scoreR, type = "link")))

  fit <- fitMod(mpg ~ wt + cyl, cars, engine = "gamma")
  expect_equal(sqlPredict(fit, score)$PRED,
               unname(predict(fit, newdata = scoreR)))

  fit <- fitMod(am ~ wt, cars, engine = "logit", family = binomial("probit"))
  expect_error(predictSql(fit, "T1"), "probit")
})


test_that("multinom and polr", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("MASS")

  fit <- fitMod(gear ~ wt + hp, cars, engine = "multinom")
  res <- sqlPredict(fit, score, output = "both")
  ref <- predict(fit, newdata = scoreR, output = "both")
  expect_equal(unname(as.matrix(res[1:3])), unname(as.matrix(ref[1:3])))
  expect_identical(res$PRED_CLASS, as.character(ref$class))

  fit <- fitMod(ordered(gear) ~ wt + hp, cars, engine = "polr")
  res <- sqlPredict(fit, score, output = "both")
  ref <- predict(fit, newdata = scoreR, output = "both")
  expect_equal(unname(as.matrix(res[1:3])), unname(as.matrix(ref[1:3])))
  expect_identical(res$PRED_CLASS, as.character(ref$class))
})


test_that("rpart: regression, classification, leaf", {
  skip_if_not_installed("rpart")

  fit <- fitMod(mpg ~ wt + hp + cyl + gear, cars, engine = "rpart",
                minsplit = 5, cp = 0.001)
  expect_equal(sqlPredict(fit, cars)$PRED, predict(fit, newdata = cars))

  fit <- fitMod(Species ~ ., iris, engine = "rpart")
  vars <- setNames(gsub(".", "_", names(iris), fixed = TRUE), names(iris))
  dat  <- setNames(iris, vars)
  res  <- sqlPredict(fit, dat, output = "both", vars = vars)
  ref  <- predict(fit, newdata = iris, output = "both")
  expect_equal(unname(as.matrix(res[1:3])), unname(as.matrix(ref[1:3])))
  expect_identical(res$PRED_CLASS, as.character(ref$class))
  expect_identical(as.character(sqlPredict(fit, dat, output = "leaf",
                                           vars = vars)$PRED),
                   predict(fit, newdata = iris, output = "leaf"))
})


test_that("survival models", {
  skip_if_not_installed("survival")
  lung <- transform(survival::lung, sex = factor(sex))
  lung <- lung[complete.cases(lung[c("age", "sex", "ph.karno")]), ]
  vars <- c(ph.karno = "ph_karno")
  dat  <- setNames(lung, sub("ph.karno", "ph_karno", names(lung), fixed = TRUE))

  fit <- fitMod(survival::Surv(time, status) ~ age + sex + ph.karno, lung,
                engine = "coxph")
  for (type in c("risk", "lp"))
    expect_equal(sqlPredict(fit, dat, type = type, vars = vars)$PRED,
                 unname(predict(fit, newdata = lung, type = type)))

  fit <- fitMod(survival::Surv(time, status) ~ age + sex + ph.karno, lung,
                engine = "weibull")
  expect_equal(sqlPredict(fit, dat, vars = vars)$PRED,
               unname(predict(fit, newdata = lung)))
})


test_that("glmnet", {
  skip_if_not_installed("glmnet")
  set.seed(1)
  fit <- fitMod(am ~ wt + hp + cyl + qsec, cars, engine = "glmnet")
  res <- sqlPredict(fit, cars, output = "both", s = "lambda.min")
  ref <- predict(fit, newdata = cars, output = "both", s = "lambda.min")
  expect_equal(res$P_manual, ref$manual)
  expect_identical(res$PRED_CLASS, as.character(ref$class))
})


test_that("statements", {
  fit <- fitMod(am ~ wt + hp, cars, engine = "logit")

  up <- predictSql(fit, "LAB.CARS", type = "response", into = "P_MANUAL")
  expect_s3_class(up, "PredictSql")
  expect_match(up, "^UPDATE LAB.CARS\nSET P_MANUAL =")
  expect_identical(attr(up, "ddl"),
                   "ALTER TABLE LAB.CARS ADD P_MANUAL DOUBLE PRECISION;")

  ex <- predictSql(fit, type = "link", statement = "expr")
  expect_named(ex, "PRED")

  cr <- predictSql(fit, "CARS", type = "link", statement = "create",
                   target = "CARS_PRED", id = "ID", dialect = "mssql")
  expect_match(cr, "INTO CARS_PRED\nFROM CARS;$")

  expect_error(predictSql(fit, "CARS", into = "A"), "'into' must name 2")
  expect_error(predictSql(fit), "'table'")
})


test_that("dialect exasol: guarded log, sqrt and division", {
  fit <- fitMod(mpg ~ log(hp) + sqrt(disp) + I(wt / carb) + log1p(qsec) +
                  log(drat, 2), cars, engine = "lm")

  # same predictions as the plain dialect where everything is defined
  expect_equal(sqlPredict(fit, cars, dialect = "exasol")$PRED,
               unname(predict(fit, newdata = cars)))

  # outside the domain: NULL for the row, no error
  bad <- cars
  bad$hp[1L] <- 0; bad$disp[2L] <- -1; bad$carb[3L] <- 0
  expect_identical(which(is.na(sqlPredict(fit, bad, dialect = "exasol")$PRED)),
                   1:3)

  up <- predictSql(fit, "LAB.CARS", dialect = "exasol")
  expect_match(up, "CASE WHEN hp > 0 THEN LN(hp) END", fixed = TRUE)
  expect_match(up, "NULLIF(carb, 0)", fixed = TRUE)
  expect_identical(
    attr(up, "ddl"),
    "ALTER TABLE LAB.CARS ADD COLUMN IF NOT EXISTS PRED DOUBLE PRECISION;")

  old <- options(alloy.sqlDialect = "exasol")
  on.exit(options(old))
  expect_identical(predictSql(fit, "LAB.CARS"), up)
})


test_that("what cannot be translated stops", {
  expect_error(predictSql(fitMod(mpg ~ poly(wt, 2), cars, engine = "lm"), "T"),
               "poly")
  expect_error(predictSql(fitMod(mpg ~ wt + pmax(hp, 100), cars,
                                 engine = "lm"), "T"),
               "cannot translate")
  expect_error(predictSql(fitMod(Sepal.Length ~ Sepal.Width, iris,
                                 engine = "lm"), "T"),
               "vars")
})


# == regression tests of the review ========================================

# value of a single SQL expression
sqlValue <- function(expr) {
  db <- sqlBackend()
  on.exit(DBI::dbDisconnect(db$con))
  DBI::dbGetQuery(db$con, db$fix(paste("SELECT", expr, "AS V")))$V
}

# table T1 with 'data' in an open database; the caller disconnects
sqlTable <- function(data) {
  db <- sqlBackend()
  d  <- data
  d[] <- lapply(d, function(v) if (is.factor(v)) as.character(v) else v)
  d$ROW_ID <- seq_len(nrow(d))
  DBI::dbWriteTable(db$con, "T1", d)
  db
}


test_that("terms with zero coefficients keep the NULL handling", {
  fit <- fitMod(mpg ~ wt + hp + cyl, cars, engine = "lm")
  fit$coefficients[c("hp", "cyl6", "cyl8")] <- 0

  bad <- cars
  bad$hp[3L] <- NA                       # missing, coefficient 0
  bad$cyl    <- as.character(bad$cyl)
  bad$cyl[4L] <- "12"                    # unknown level, effects 0
  res <- sqlPredict(fit, bad)$PRED

  expect_identical(which(is.na(res)), 3:4)
  expect_equal(res[-(3:4)],
               unname(predict(fit, newdata = cars))[-(3:4)])
})


test_that("glmnet: predictors shrunk to zero are still required", {
  skip_if_not_installed("glmnet")
  set.seed(1)
  fit <- fitMod(mpg ~ wt + hp + qsec + drat, cars, engine = "glmnet")
  sql <- predictSql(fit, statement = "expr", s = "lambda.1se")
  for (v in c("wt", "hp", "qsec", "drat"))
    expect_match(sql, paste0("* ", v), fixed = TRUE)
})


test_that("arguments are matched by name, not by position", {
  fit <- fitMod(mpg ~ log(base = 2, x = hp) + log(wt, base = 10), cars,
                engine = "lm")
  expect_equal(sqlPredict(fit, cars)$PRED,
               unname(predict(fit, newdata = cars)))

  fit <- fitMod(mpg ~ factor(levels = c(4, 6, 8), x = cyl) + wt, mtcars,
                engine = "lm")
  expect_equal(sqlPredict(fit, mtcars)$PRED,
               unname(predict(fit, newdata = mtcars)))
})


test_that("factor labels are rejected however they are given", {
  for (f in c(mpg ~ factor(cyl, c(4, 6, 8), c("vier", "sechs", "acht")),
              mpg ~ factor(cyl, lab = c("vier", "sechs", "acht")),
              mpg ~ ordered(cyl, c(4, 6, 8), c("vier", "sechs", "acht")),
              mpg ~ factor(labels = c("vier", "sechs", "acht"), x = cyl)))
    expect_error(predictSql(fitMod(f, mtcars, engine = "lm"), "T1"),
                 "'labels' is not supported")

  # levels alone are fine
  fit <- fitMod(mpg ~ factor(cyl, c(8, 6, 4)) + wt, mtcars, engine = "lm")
  expect_equal(sqlPredict(fit, mtcars)$PRED,
               unname(predict(fit, newdata = mtcars)))
})


test_that("quoted identifiers that look like references", {
  fit <- fitMod(am ~ wt + hp, cars, engine = "logit")
  ref <- predictSql(fit, type = "response", statement = "expr")

  for (q in c("[{eta}]", "\"{eta}\"", "`{eta}`", "[a]]{eta}]")) {
    sql <- predictSql(fit, type = "response", statement = "expr",
                      vars = c(wt = q))
    expect_identical(unclass(sql)[[1L]],
                     gsub("wt", q, unclass(ref)[[1L]], fixed = TRUE))
    expect_no_error(predictSql(fit, "T1", output = "both",
                               statement = "select", id = "ROW_ID",
                               vars = c(wt = q)))
  }
})


test_that("softmax and logistic link do not overflow", {
  # the reviewer's example: logits 0, 1000, 1001
  p <- vapply(.sqlSoftmax(c("0e0", "1e3", "1.001e3")), sqlValue, numeric(1L))
  expect_equal(unname(p), c(0, 1 / (1 + exp(1)), 1 / (1 + exp(-1))))

  expect_identical(sqlValue(.sqlLinkInv("logit", "(-2e3)")), 0)
  expect_identical(sqlValue(.sqlLinkInv("logit", "(2e3)")), 1)
  for (eta in c(-690, -30, 0, 30, 690))
    expect_equal(sqlValue(.sqlLinkInv("logit", sprintf("(%s)", .sqlNum(eta)))),
                 plogis(eta), tolerance = 1e-14)
  for (eta in c(-2e3, -100, 5, 50, 2e3)) {
    arg <- sprintf("(%s)", .sqlNum(eta))
    expect_equal(sqlValue(.sqlLinkInv("cloglog", arg)), -expm1(-exp(eta)))
    expect_equal(sqlValue(.sqlLinkInv("loglog", arg)), exp(-exp(-eta)))
  }
  expect_identical(sqlValue(.sqlLinkInv("log", "(-2e3)")), 0)

  # a model scored far outside of its data
  skip_if_not_installed("nnet")
  fit <- fitMod(gear ~ wt + hp, cars, engine = "multinom")
  far <- transform(cars, wt = wt * 1e4, hp = hp * 1e4)
  res <- sqlPredict(fit, far, output = "both")

  eta <- cbind(0, cbind(1, far$wt, far$hp) %*% t(coef(fit)))
  ref <- exp(eta - apply(eta, 1L, max))
  ref <- ref / rowSums(ref)
  expect_false(anyNA(res))
  expect_equal(unname(as.matrix(res[1:3])), unname(ref))
  expect_identical(res$PRED_CLASS, levels(cars$gear)[max.col(eta, "first")])
})


test_that("as.numeric() is rejected", {
  fit <- fitMod(mpg ~ as.numeric(cyl) + wt, cars, engine = "lm")
  expect_error(predictSql(fit, "T1"), "cannot translate 'as.numeric(cyl)'",
               fixed = TRUE)
})


test_that("levels that look like references or contain quotes", {
  odd <- transform(cars, cyl = factor(cyl, labels = c("{eta}", "it's {p_1}",
                                                      "\"{class}\"")))
  fit <- fitMod(am ~ wt + cyl, odd, engine = "logit")
  ref <- predict(fit, newdata = odd, output = "both")

  for (id in list("ROW_ID", NULL)) {            # staged and inlined
    res <- sqlPredict(fit, odd, output = "both", id = id)
    expect_equal(res$P_manual, ref$manual)
    expect_identical(res$PRED_CLASS, as.character(ref$class))
  }

  # as the levels of the response
  fit <- fitMod(cyl ~ wt + hp, odd, engine = "multinom")
  res <- sqlPredict(fit, odd, output = "class")
  expect_identical(res$PRED_CLASS,
                   as.character(predict(fit, newdata = odd,
                                        output = "class")$class))
})



# == executed statements and further engines ===============================

test_that("UPDATE and CREATE are executed", {
  fit <- fitMod(am ~ wt + hp, cars, engine = "logit")
  ref <- predict(fit, newdata = cars, output = "both")
  db  <- sqlTable(cars)
  on.exit(DBI::dbDisconnect(db$con))

  up <- predictSql(fit, "T1", output = "both")
  for (ddl in attr(up, "ddl"))
    DBI::dbExecute(db$con, ddl)
  expect_identical(DBI::dbExecute(db$con, db$fix(up)), nrow(cars))
  res <- DBI::dbGetQuery(
    db$con, "SELECT P_auto, P_manual, PRED_CLASS FROM T1 ORDER BY ROW_ID")
  expect_equal(res$P_auto, ref$auto)
  expect_equal(res$P_manual, ref$manual)
  expect_identical(res$PRED_CLASS, as.character(ref$class))

  cr <- predictSql(fit, "T1", type = "link", statement = "create",
                   target = "T1_PRED", id = "ROW_ID")
  DBI::dbExecute(db$con, db$fix(cr))
  res <- DBI::dbGetQuery(db$con, "SELECT * FROM T1_PRED ORDER BY ROW_ID")
  expect_named(res, c("ROW_ID", "PRED"))
  expect_equal(res$PRED, unname(predict(fit, newdata = cars, type = "link")))
})


test_that("zeroinfl", {
  skip_if_not_installed("pscl")
  dat <- transform(cars, n = carb - 1L)
  fit <- fitMod(n ~ wt + cyl | hp, dat, engine = "zeroinfl")
  for (type in c("response", "count", "zero"))
    expect_equal(sqlPredict(fit, dat, type = type)$PRED,
                 unname(predict(fit, newdata = dat, type = type)))
})


test_that("mixed models: population level", {
  skip_if_not_installed("lme4")
  sleep <- lme4::sleepstudy

  fit <- suppressMessages(
    fitMod(Reaction ~ Days + (Days | Subject), sleep, engine = "lmMixed"))
  expect_message(sql <- predictSql(fit, "T1", statement = "select",
                                   id = "ROW_ID"), "population-level")
  expect_equal(suppressMessages(sqlPredict(fit, sleep))$PRED,
               unname(predict(fit$model, newdata = sleep, re.form = NA)))

  sleep$slow <- factor(sleep$Reaction > 300, labels = c("no", "yes"))
  fit <- suppressWarnings(suppressMessages(
    fitMod(slow ~ Days + (1 | Subject), sleep, engine = "logitMixed")))
  res <- suppressMessages(sqlPredict(fit, sleep, output = "both"))
  ref <- unname(predict(fit$model, newdata = sleep, re.form = NA,
                        type = "response"))
  expect_equal(res$P_yes, ref)
  expect_identical(res$PRED_CLASS, ifelse(ref > 0.5, "yes", "no"))
})


test_that("beta regression", {
  skip_if_not_installed("betareg")
  data("GasolineYield", package = "betareg", envir = environment())
  dat <- GasolineYield

  fit <- fitMod(yield ~ gravity + temp | temp, dat, engine = "beta")
  for (type in c("response", "link", "precision", "variance"))
    expect_equal(sqlPredict(fit, dat, type = type)$PRED,
                 unname(predict(fit, newdata = dat, type = type)))

  # constant precision
  fit <- fitMod(yield ~ gravity + temp, dat, engine = "beta")
  for (type in c("response", "precision"))
    expect_equal(sqlPredict(fit, dat, type = type)$PRED,
                 unname(predict(fit, newdata = dat, type = type)))
})


test_that("update by key is a MERGE with the predictor computed once", {
  fit <- fitMod(am ~ wt + hp, cars, engine = "logit")
  ref <- predict(fit, newdata = cars, output = "both")

  mg <- predictSql(fit, "T1", output = "both", id = "ROW_ID")
  expect_match(mg, "^MERGE INTO T1 t\nUSING \\(")
  expect_match(mg, "ON (t.ROW_ID = q.ROW_ID)\nWHEN MATCHED THEN UPDATE SET",
               fixed = TRUE)
  expect_match(mg, "PRED_CLASS = q.PRED_CLASS;", fixed = TRUE)
  expect_length(gregexpr("* wt", mg, fixed = TRUE)[[1L]], 1L)
  expect_identical(attr(mg, "ddl"),
                   attr(predictSql(fit, "T1", output = "both"), "ddl"))

  # the source of the MERGE yields the predictions
  src <- sub("\n\\) q\nON .*$", "", sub("^MERGE INTO T1 t\nUSING \\(\n", "",
                                         as.character(mg)))
  db  <- sqlTable(cars)
  on.exit(DBI::dbDisconnect(db$con))
  res <- DBI::dbGetQuery(db$con, db$fix(src))
  res <- res[order(res$ROW_ID), ]
  expect_equal(res$P_manual, ref$manual)
  expect_identical(res$PRED_CLASS, as.character(ref$class))

  # several key columns, qualified targets for Exasol
  mg <- predictSql(fit, "T1", type = "response", id = c("A", "B"),
                   dialect = "exasol")
  expect_match(mg, "ON (t.A = q.A AND t.B = q.B)", fixed = TRUE)
  expect_match(mg, "  t.PRED = q.PRED;", fixed = TRUE)
})


test_that("Exasol: update without a key merges on ROWID", {
  fit <- fitMod(am ~ wt + hp, cars, engine = "logit")

  mg <- predictSql(fit, "T1", type = "response", dialect = "exasol")
  expect_match(mg, "SELECT ROWID AS ROW_ID_,", fixed = TRUE)
  expect_match(mg, "ON (t.ROWID = q.ROW_ID_)", fixed = TRUE)
  expect_length(gregexpr("* wt", mg, fixed = TRUE)[[1L]], 1L)

  # a tree has a single stage per output: the key is renamed there too
  skip_if_not_installed("rpart")
  rp <- fitMod(mpg ~ wt + hp, cars, engine = "rpart")
  expect_match(predictSql(rp, "T1", output = "leaf", dialect = "exasol"),
               "SELECT ROWID AS ROW_ID_,", fixed = TRUE)

  # the plain UPDATE on request, and always in the other dialects
  expect_match(predictSql(fit, "T1", type = "response", dialect = "exasol",
                          id = FALSE), "^UPDATE T1\nSET PRED =")
  expect_match(predictSql(fit, "T1", type = "response"), "^UPDATE T1\nSET PRED =")
  expect_error(predictSql(fit, "T1", id = 1), "'id' must be")
})
