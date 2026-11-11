
#' Translate the predictions of a model into SQL
#'
#' Writes the prediction rule of a model fitted with \code{\link{fitMod}} as
#' SQL code, so that the model can be scored directly in a database: as an
#' \code{UPDATE} of a column, a \code{SELECT}, a \code{CREATE TABLE ... AS}
#' or as bare expressions to paste into a query of one's own.  The arguments
#' \code{output}, \code{type} and \code{s} have the meaning and the defaults
#' they have in \code{\link{predict.FitMod}}.
#'
#' @param object A fitted model of class \code{"FitMod"}.
#' @param table Name of the database table holding the predictors.  Not
#'   needed for \code{statement = "expr"}.
#' @param output For classification models one of \code{"prob"} (default),
#'   \code{"class"} or \code{"both"}; for \code{engine = "rpart"}
#'   additionally \code{"where"} and \code{"leaf"}.  Ignored for regression
#'   and survival models.
#' @param s For \code{engine = "glmnet"} only: the penalty parameter
#'   \eqn{\lambda}, default \code{"lambda.1se"}.
#' @param type For regression models \code{"response"} (default) or
#'   \code{"link"}; for \code{"coxph"} \code{"risk"} (default) or
#'   \code{"lp"}; for the parametric survival models and \code{"tobit"}
#'   \code{"response"} (default) or \code{"lp"}; for \code{"beta"} also
#'   \code{"precision"} and \code{"variance"}; for \code{"zeroinfl"}
#'   \code{"response"} (default), \code{"count"} or \code{"zero"}.  For
#'   \code{"logit"} and \code{"logitFirth"} an explicit \code{"link"} or
#'   \code{"response"} yields a single column, as in
#'   \code{predict.FitMod()}.
#' @param statement The kind of SQL to write: \code{"update"} (default),
#'   \code{"select"}, \code{"create"} or \code{"expr"}, see Details.
#' @param into Names of the columns the predictions are written to.  If
#'   \code{NULL} (default) they are \code{PRED} for a single numeric
#'   prediction, \code{P_<level>} for class probabilities and
#'   \code{PRED_CLASS} for the predicted class.
#' @param id For \code{"select"} and \code{"create"}: the columns of
#'   \code{table} to return along with the predictions, typically the key.
#'   If \code{NULL} (default) all columns are returned.
#' @param target For \code{statement = "create"}: name of the new table.
#' @param vars Named character vector mapping variable names of the model
#'   to column names of the database, e.g.
#'   \code{c(Sepal.Length = "SEPAL_LENGTH")}.  Variables not listed keep
#'   their name.  The column names are used as given (they may be quoted or
#'   qualified).
#' @param dialect SQL dialect, one of \code{"ansi"}, \code{"exasol"} or
#'   \code{"mssql"}; the default is taken from
#'   \code{getOption("alloy.sqlDialect", "ansi")}.  \code{"ansi"} is
#'   understood by Oracle, PostgreSQL, Snowflake, DuckDB and most other
#'   databases.  \code{"exasol"} guards the operations that abort a
#'   statement in Exasol, see Details.  \code{"mssql"} is for Microsoft SQL
#'   Server (\code{LOG()} for the natural logarithm,
#'   \code{SELECT ... INTO}).
#' @param ... Not used.
#'
#' @details
#' \strong{Supported models.}  All models with a linear predictor:
#' \code{"lm"}, \code{"lmrob"}, \code{"logit"}, \code{"logitFirth"},
#' \code{"poisson"}, \code{"quasipoisson"}, \code{"gamma"},
#' \code{"negbin"}, \code{"beta"}, \code{"tobit"}, \code{"zeroinfl"},
#' \code{"glmnet"}, \code{"coxph"}, \code{"weibull"}, \code{"exponential"},
#' \code{"lognormal"}, \code{"loglogistic"}; further \code{"multinom"},
#' \code{"polr"} and \code{"rpart"}.  For the mixed models
#' (\code{"lmMixed"}, \code{"logitMixed"}, ...) the population-level
#' prediction is written, i.e. that of
#' \code{predict(object, re.form = NA)}; a message says so.  The SQL
#' reproduces \code{predict.FitMod()} up to floating point rounding.
#'
#' Not supported are \code{"nnet"}, \code{"C5.0"}, \code{"lda"},
#' \code{"qda"}, \code{"svm"}, \code{"naiveBayes"}, \code{"randomForest"}
#' and \code{"xgboost"}; further links without a closed form in SQL
#' (probit), Cox models with strata, and terms that span several columns of
#' the design matrix (\code{poly()}, \code{ns()}, \code{bs()}, matrices).
#'
#' \strong{Terms.}  Within the formula the operators \code{+ - * / ^} and
#' the functions \code{log}, \code{log2}, \code{log10}, \code{log1p},
#' \code{exp}, \code{expm1}, \code{sqrt}, \code{abs} and \code{I()} are
#' translated, as are \code{factor()}, \code{as.factor()} and
#' \code{cut()} with constant breaks.  Interactions and any contrast coding
#' are supported: the effect of each factor level (or combination of levels)
#' is computed in R and written as one \code{CASE} expression.  Anything
#' else stops with an error rather than producing SQL that might be wrong.
#' Factor levels are written as string literals; a numeric database column
#' behind \code{factor(x)} relies on the implicit conversion of the
#' database.
#'
#' \strong{Missing values and unknown levels.}  A \code{NULL} in a
#' predictor yields a \code{NULL} prediction (\code{na.pass}).  So does a
#' factor level that the model has not seen: it does not silently fall into
#' the reference category.  This holds for every predictor of the model,
#' also for those whose coefficients are zero (aliased, or shrunk to zero
#' by \code{"glmnet"}): they stay in the SQL with a coefficient of 0.  For \code{"rpart"} there are no surrogate
#' splits in SQL; a row whose split variable is \code{NULL} gets a
#' \code{NULL} prediction, where \code{predict()} would use a surrogate.
#'
#' \strong{Statements.}  \code{"update"} writes the predictions into
#' existing columns of \code{table}; the columns must exist (the
#' \code{ALTER TABLE} needed is in the attribute \code{"ddl"} of the
#' result).  \code{"select"} and \code{"create"} return the predictions
#' along with \code{id}; intermediate results that are needed more than
#' once (the linear predictor, the leaf of a tree) are then computed in a
#' subquery.  \code{"expr"} returns the bare expressions as a named
#' character vector.
#'
#' \strong{Exasol.}  A logarithm of a value that is not positive, a square
#' root of a negative value or a division by zero is an error in Exasol
#' and aborts the whole statement, where R returns \code{-Inf},
#' \code{NaN} or \code{Inf} for the row concerned.  With
#' \code{dialect = "exasol"} these operations are guarded
#' (\code{CASE WHEN x > 0 THEN LN(x) END}, \code{NULLIF(x, 0)}) and
#' yield \code{NULL}, so that a single such row costs its own prediction
#' only.  The \code{ALTER TABLE} statements use
#' \code{ADD COLUMN IF NOT EXISTS} and can be run repeatedly.  Unquoted
#' column names are folded to upper case by Exasol, so the variable names
#' of the model need no mapping as long as the columns were created
#' unquoted.
#'
#' Coefficients are written with the 15 to 17 significant digits that
#' reproduce the double exactly, in exponent notation, so that every
#' database treats them as floating point numbers.
#'
#' @return A character string of class \code{"PredictSql"} holding the
#'   statement, printed with \code{cat()}; for \code{statement = "expr"} a
#'   named character vector with one expression per output column.  The
#'   attribute \code{"exprs"} always holds these expressions.
#'
#' @examples
#' fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, engine = "logit")
#'
#' # probability of the second class into an existing column
#' predictSql(fitLogit, table = "LAB.ADMIT", type = "response",
#'            into = "P_ADMIT")
#'
#' # both class probabilities and the class, with the key
#' predictSql(fitLogit, table = "LAB.ADMIT", output = "both",
#'            statement = "select", id = "ID")
#'
#' # the linear predictor as a bare expression
#' predictSql(fitLogit, type = "link", statement = "expr")
#'
#' @seealso \code{\link{predict.FitMod}}
#' @family modelling
#' @concept prediction
#' @aliases PredictSql-class
#' @importFrom methods setOldClass
#' @exportClass PredictSql
#' @export
predictSql <- function(object, table,
                       output = c("prob", "class", "both", "where", "leaf"),
                       s = "lambda.1se",
                       type = NULL,
                       statement = c("update", "select", "create", "expr"),
                       into = NULL, id = NULL, target = NULL,
                       vars = NULL,
                       dialect = getOption("alloy.sqlDialect", "ansi"),
                       ...) {

  if (!inherits(object, "FitMod"))
    stop("'object' must be a model fitted with fitMod().")

  output    <- match.arg(output)
  statement <- match.arg(statement)
  dialect   <- match.arg(dialect, c("ansi", "exasol", "mssql"))

  if (statement != "expr" && (missing(table) || !is.character(table) ||
                              length(table) != 1L))
    stop("'table' must be the name of the database table.")
  if (statement == "create" && is.null(target))
    stop("statement = 'create' needs the name of the new table in 'target'.")
  if (!is.null(vars) && (!is.character(vars) || is.null(names(vars))))
    stop("'vars' must be a named character vector (model name = column).")

  engine <- object$engine

  # as in predict.FitMod(): the natural class, and the raw model of the
  # wrapped engines
  obj <- object
  class(obj) <- setdiff(class(obj), "FitMod")
  fit <- if (inherits(obj, "FitMod.lme4") || inherits(obj, "FitMod.xgboost"))
    obj$model
  else
    obj

  ctx <- list(vars  = vars,
              ln    = if (dialect == "mssql") "LOG" else "LN",
              guard = dialect == "exasol",
              env   = .sqlFormulaEnv(fit))

  spec <- .sqlSpec(obj, fit, engine, output = output, type = type, s = s,
                   ctx = ctx)

  if (!is.null(into)) {
    if (length(into) != length(spec$out))
      stop("'into' must name ", length(spec$out), " column(s): ",
           paste(names(spec$out), collapse = ", "), ".")
    names(spec$out) <- into
  }

  exprs <- vapply(spec$out, function(nm) .sqlInline(spec$defs, nm),
                  character(1L))

  res <- switch(statement,
    expr   = exprs,
    update = .sqlUpdate(exprs, table),
    select = .sqlSelect(spec, exprs, table, id),
    create = {
      sel <- .sqlSelect(spec, exprs, table, id, into =
                          if (dialect == "mssql") target)
      if (dialect == "mssql") sel
      else paste0("CREATE TABLE ", target, " AS\n", sel)
    })

  if (statement != "expr")
    res <- paste0(res, ";")

  attr(res, "exprs") <- exprs
  if (statement == "update")
    attr(res, "ddl") <- .sqlAlter(spec, table, dialect)
  class(res) <- c("PredictSql", "character")
  res
}


# Registered as an S4 class extending "character", so that the result can
# be handed to the S4 generics of DBI as it is: DBI::dbExecute(con, sql)
methods::setOldClass(c("PredictSql", "character"))


#' @rdname predictSql
#' @param x An object of class \code{"PredictSql"}.
#' @export
print.PredictSql <- function(x, ...) {
  nms <- names(x)
  if (is.null(nms))
    cat(x, sep = "\n")
  else
    cat(paste0("-- ", nms, "\n", x), sep = "\n\n")
  invisible(x)
}



# == model -> prediction rule =============================================

# The prediction rule of a model as a list with
#   defs  named character vector of SQL expressions, in order of
#         dependency; "{name}" refers to an earlier definition
#   out   named character vector: output column -> definition
# Intermediate results (linear predictor, leaf) are definitions of their
# own, so that a statement can compute them once (.sqlSelect) or have them
# inlined (.sqlInline).
#' @keywords internal
.sqlSpec <- function(obj, fit, engine, output, type, s, ctx) {

  unsupported <- c("nnet", "C5.0", "lda", "qda", "svm", "naiveBayes",
                   "randomForest", "xgboost")
  if (engine %in% unsupported)
    stop("predictSql() does not support engine = '", engine, "'.")

  if (output %in% c("where", "leaf") && engine != "rpart")
    stop("output = '", output, "' is only available for engine = 'rpart'.")

  if (engine == "rpart")
    return(.sqlSpecRpart(fit, output, ctx))
  if (engine == "polr")
    return(.sqlSpecPolr(fit, output, ctx))
  if (engine == "multinom" ||
      (engine == "glmnet" && inherits(fit$glmnet.fit, "multnet")))
    return(.sqlSpecMultinomial(obj, fit, engine, output, s, ctx))
  if (engine == "zeroinfl")
    return(.sqlSpecZeroinfl(fit, type, ctx))
  if (engine == "beta")
    return(.sqlSpecBeta(fit, type, ctx))

  # --- one linear predictor and a link ---
  p <- .sqlParts(obj, fit, engine, s)
  defs <- c(eta = .sqlLinPred(p$beta, p$terms, p$xlev, p$contrasts, ctx))

  # logistic models with an explicit type: a single column, as predict.glm
  single <- engine %in% c("logit", "logitFirth") &&
    isTRUE(type %in% c("link", "response"))

  # the survival engines are unknown to .is_classification()
  survival <- engine == "coxph" || inherits(fit, "survreg")

  if (!single && !survival && .is_classification(obj, fit, engine)) {
    if (!is.null(type))
      warning("'type' is ignored for classification models; ",
              "use 'output' to control the result.", call. = FALSE)
    defs["p"] <- .sqlLinkInv(p$link, "{eta}")
    lev <- if (engine == "glmnet") fit$glmnet.fit$classnames
           else .response_levels(fit)
    if (is.null(lev)) lev <- c("0", "1")
    return(.sqlClassOutputs(
      defs, output, lev,
      probs = c("(1e0 - {p})", "{p}"),
      class = sprintf("CASE WHEN {p} > 5e-01 THEN %s WHEN {p} <= 5e-01 THEN %s END",
                      .sqlStr(lev[2L]), .sqlStr(lev[1L]))))
  }

  # --- regression and survival ---
  linear   <- c("link", "lp", "linear")
  dflt     <- if (engine == "coxph") "risk" else "response"
  type     <- if (is.null(type)) dflt else type
  if (!type %in% c(dflt, linear))
    stop("type = '", type, "' is not available in predictSql() for engine = '",
         engine, "'.")

  if (type %in% linear || p$link == "identity")
    return(list(defs = defs, out = c(PRED = "eta")))

  defs["pred"] <- .sqlLinkInv(p$link, "{eta}")
  list(defs = defs, out = c(PRED = "pred"))
}


# Coefficients, terms, factor levels, contrasts and link of the models
# with a single linear predictor.  The coefficients are aligned with the
# columns of the design matrix; a leading "(Intercept)" is a constant even
# if the terms have no intercept (glmnet, the centring of coxph).
#' @keywords internal
.sqlParts <- function(obj, fit, engine, s) {

  if (engine == "glmnet") {
    link <- switch(class(fit$glmnet.fit)[1L],
                   elnet = "identity", lognet = "logit", fishnet = "log",
                   stop("predictSql() does not support this glmnet family."))
    cf <- as.matrix(stats::coef(fit, s = s))
    return(list(beta = stats::setNames(cf[, 1L], rownames(cf)),
                terms = obj$terms, xlev = obj$xlev, contrasts = NULL,
                link = link))
  }

  if (inherits(obj, "FitMod.lme4")) {
    message("predictSql: population-level prediction (fixed effects only), ",
            "as predict(object, re.form = NA).")
    tt <- stats::terms(fit)
    return(list(beta = lme4::fixef(fit), terms = tt,
                xlev = stats::.getXlevels(tt, stats::model.frame(fit)),
                contrasts = attr(lme4::getME(fit, "X"), "contrasts"),
                link = stats::family(fit)$link))
  }

  if (!is.null(fit$call$offset))
    stop("predictSql() cannot translate the argument 'offset'; ",
         "use an offset() term in the formula instead.")

  beta <- stats::coef(fit)

  link <- if (engine == "coxph") {
    if (!is.null(attr(stats::terms(fit), "specials")$strata))
      stop("predictSql() does not support Cox models with strata.")
    # predict.coxph() centres the linear predictor
    b    <- replace(beta, is.na(beta), 0)
    beta <- c("(Intercept)" = -sum(b * fit$means[names(b)]), beta)
    "log"
  } else if (inherits(fit, "survreg")) {
    # tobit is a gaussian survreg; the other distributions model log(time)
    if (fit$dist %in% c("weibull", "exponential", "lognormal", "loglogistic"))
      "log" else "identity"
  } else if (inherits(fit, "glm")) {
    stats::family(fit)$link
  } else {
    "identity"
  }

  list(beta = beta, terms = stats::terms(fit), xlev = fit$xlevels,
       contrasts = fit$contrasts, link = link)
}


# Output columns of a classifier from the expressions of its class
# probabilities and of the predicted class
#' @keywords internal
.sqlClassOutputs <- function(defs, output, lev, probs, class) {

  pn <- paste0("p_", seq_along(lev))
  defs[pn]      <- probs
  defs["class"] <- class

  out <- c(if (output != "class")
             stats::setNames(pn, paste0("P_", .sqlColName(lev))),
           if (output != "prob") c(PRED_CLASS = "class"))

  list(defs = defs, out = out)
}


# multinom and multinomial glmnet: one linear predictor per class
# (multinom: 0 for the first class), probabilities by .sqlSoftmax()
#' @keywords internal
.sqlSpecMultinomial <- function(obj, fit, engine, output, s, ctx) {

  if (engine == "multinom") {
    lev <- fit$lev
    cf  <- stats::coef(fit)
    if (is.null(dim(cf)))
      cf <- matrix(cf, nrow = 1L, dimnames = list(lev[2L], names(cf)))
    betas <- c(list(NULL), lapply(seq_len(nrow(cf)), function(i) cf[i, ]))
    tt <- stats::terms(fit); xlev <- fit$xlevels; ctr <- fit$contrasts
  } else {
    lev   <- fit$glmnet.fit$classnames
    betas <- lapply(stats::coef(fit, s = s)[lev], function(m) {
      m <- as.matrix(m)
      stats::setNames(m[, 1L], rownames(m))
    })
    tt <- obj$terms; xlev <- obj$xlev; ctr <- NULL
  }

  en   <- paste0("eta_", seq_along(lev))
  defs <- character(0L)
  ref  <- character(length(lev))        # how a class' eta is referred to
  for (k in seq_along(lev)) {
    if (is.null(betas[[k]])) {
      ref[k] <- "0e0"
    } else {
      defs[en[k]] <- .sqlLinPred(betas[[k]], tt, xlev, ctr, ctx)
      ref[k] <- sprintf("{%s}", en[k])
    }
  }

  .sqlClassOutputs(defs, output, lev,
                   probs = .sqlSoftmax(ref),
                   class = .sqlArgmax(ref, lev))
}


# polr: cumulative probabilities F(zeta_k - eta), class probabilities as
# their differences, class with the largest probability
#' @keywords internal
.sqlSpecPolr <- function(fit, output, ctx) {

  lev <- fit$lev
  K   <- length(lev)

  # the distribution functions are the inverse links
  link <- switch(fit$method,
    logistic = "logit", cloglog = "cloglog", loglog = "loglog",
    cauchit  = "cauchit",
    stop("predictSql() does not support polr with method = '", fit$method,
         "' (no closed form in SQL)."))

  defs <- c(eta = .sqlLinPred(c("(Intercept)" = 0, stats::coef(fit)),
                              stats::terms(fit), fit$xlevels,
                              fit$contrasts, ctx))
  cn <- paste0("cum_", seq_len(K - 1L))
  defs[cn] <- .sqlLinkInv(link, sprintf("(%s - {eta})",
                                        .sqlNum(unname(fit$zeta))))

  hi <- c(sprintf("{%s}", cn), "1e0")
  lo <- c("", sprintf(" - {%s}", cn))

  .sqlClassOutputs(defs, output, lev,
                   probs = paste0(hi, lo),
                   class = .sqlArgmax(sprintf("{p_%d}", seq_len(K)), lev))
}


# zeroinfl: count model (log link) and zero-inflation model
#' @keywords internal
.sqlSpecZeroinfl <- function(fit, type, ctx) {

  type <- if (is.null(type)) "response" else type
  if (!type %in% c("response", "count", "zero"))
    stop("type = '", type, "' is not available in predictSql() for ",
         "engine = 'zeroinfl'.")

  lp <- function(part)
    .sqlLinPred(fit$coefficients[[part]], fit$terms[[part]], fit$levels,
                fit$contrasts[[part]], ctx)

  defs <- c(eta_count = lp("count"), eta_zero = lp("zero"))
  defs["mu"]   <- .sqlLinkInv("log", "{eta_count}")
  defs["pi"]   <- .sqlLinkInv(fit$link, "{eta_zero}")
  defs["pred"] <- "(1e0 - {pi}) * {mu}"

  list(defs = defs,
       out  = c(PRED = switch(type, response = "pred", count = "mu",
                              zero = "pi")))
}


# betareg: mean model and precision model.  The components are accessed by
# position, their names depend on the distribution (see .waldTable.betareg)
#' @keywords internal
.sqlSpecBeta <- function(fit, type, ctx) {

  type <- if (is.null(type)) "response" else type
  if (!type %in% c("response", "link", "precision", "variance"))
    stop("type = '", type, "' is not available in predictSql() for ",
         "engine = 'beta'.")
  if (!is.null(fit[["dist"]]) && !identical(fit[["dist"]], "beta") &&
      type != "link")
    stop("predictSql() supports the extended-support beta regression ",
         "(dist = '", fit[["dist"]], "') with type = 'link' only.")

  lp <- function(i) {
    b <- fit$coefficients[[i]]
    # a constant precision on the identity link is named "(phi)"
    names(b)[names(b) == "(phi)"] <- "(Intercept)"
    .sqlLinPred(b, fit$terms[[i]], fit$levels[[i]], fit$contrasts[[i]], ctx)
  }

  defs <- c(eta = lp(1L))
  if (type == "link")
    return(list(defs = defs, out = c(PRED = "eta")))

  defs["mu"] <- .sqlLinkInv(fit$link[[1L]]$name, "{eta}")
  if (type == "response")
    return(list(defs = defs, out = c(PRED = "mu")))

  defs["eta_phi"] <- lp(2L)
  defs["phi"]     <- .sqlLinkInv(fit$link[[2L]]$name, "{eta_phi}")
  defs["var"]     <- "{mu} * (1e0 - {mu}) / (1e0 + {phi})"

  list(defs = defs,
       out  = c(PRED = if (type == "precision") "phi" else "var"))
}


# rpart: the leaf as a nested CASE over the splits, all outputs as a
# lookup on the leaf
#' @keywords internal
.sqlSpecRpart <- function(fit, output, ctx) {

  ff     <- fit$frame
  nodes  <- as.numeric(rownames(ff))
  isLeaf <- ff$var == "<leaf>"
  xlev   <- attr(fit, "xlevels")

  # row of the primary split of each node in fit$splits
  first <- cumsum(c(1L, ff$ncompete + ff$nsurrogate + !isLeaf))

  branch <- function(node, indent) {

    i <- match(node, nodes)
    if (isLeaf[i])
      return(format(node, scientific = FALSE))

    v    <- as.character(ff$var[i])
    sp   <- fit$splits[first[i], ]
    ncat <- sp[["ncat"]]

    if (ncat < 2L) {
      # continuous: ncat = -1 sends x < cut to the left, +1 x >= cut
      x   <- .sqlExpr(str2lang(v), ctx)
      cut <- .sqlNum(sp[["index"]])
      cond <- sprintf(c("%s < %s", "%s >= %s"), x, cut)
      if (ncat > 0L) cond <- rev(cond)
    } else {
      # categorical: 1 = left, 3 = right, 2 = level not present at the node
      lv   <- xlev[[v]]
      dir  <- fit$csplit[sp[["index"]], seq_along(lv)]
      lc   <- .sqlLevelConds(v, lv, ctx)
      cond <- vapply(c(1L, 3L), function(d)
        paste0("(", paste(lc[dir == d], collapse = " OR "), ")"),
        character(1L))
    }

    pad <- strrep("  ", indent)
    paste0("CASE\n",
           pad, "  WHEN ", cond[1L], " THEN ",
           branch(2 * node, indent + 1L), "\n",
           pad, "  WHEN ", cond[2L], " THEN ",
           branch(2 * node + 1, indent + 1L), "\n",
           pad, "END")
  }

  defs <- c(leaf = branch(1, 0L))
  leaf <- format(nodes[isLeaf], scientific = FALSE)

  lookup <- function(values)
    paste0("CASE {leaf}",
           paste0(" WHEN ", leaf, " THEN ", values, collapse = ""), " END")

  if (output == "leaf")
    return(list(defs = defs, out = c(PRED = "leaf")))
  if (output == "where") {
    defs["where"] <- lookup(which(isLeaf))
    return(list(defs = defs, out = c(PRED = "where")))
  }

  if (!identical(fit$method, "class")) {
    defs["pred"] <- lookup(.sqlNum(ff$yval[isLeaf]))
    return(list(defs = defs, out = c(PRED = "pred")))
  }

  # classification: yval2 = (class, counts, probabilities, node prob.)
  lev <- attr(fit, "ylevels")
  K   <- length(lev)
  y2  <- ff$yval2[isLeaf, , drop = FALSE]

  .sqlClassOutputs(
    defs, output, lev,
    probs = vapply(seq_len(K), function(k) lookup(.sqlNum(y2[, 1L + K + k])),
                   character(1L)),
    class = lookup(.sqlStr(lev[y2[, 1L]])))
}



# == linear predictor =====================================================

# The linear predictor X %*% beta as a SQL expression.
#
# The design matrix is not rebuilt column by column.  For a term without
# factors the contribution is coefficient * expression; for a term with
# factors the effect of every level (combination of levels) is computed
# here - row of the coding matrix times the coefficients of the term - and
# written as one CASE.  This covers any contrast coding and interactions,
# and a level the model does not know yields NULL instead of the reference
# category.  Whether a factor enters a term by contrasts or by indicators
# is taken from the "factors" attribute of the terms, as model.matrix()
# does.
#
# beta must be aligned with the columns of the design matrix.  The column
# names are reconstructed and compared with names(beta): a term that
# cannot be mapped stops, it never yields wrong SQL.
#' @keywords internal
.sqlLinPred <- function(beta, tt, xlev, contrasts, ctx) {

  tt   <- stats::delete.response(tt)
  fac  <- attr(tt, "factors")
  labs <- attr(tt, "term.labels")
  dc   <- attr(tt, "dataClasses")
  beta <- replace(beta, is.na(beta), 0)      # aliased coefficients

  pieces   <- character(0L)
  expected <- character(0L)

  # --- constant ---
  hasInt <- attr(tt, "intercept") == 1L
  if (identical(names(beta)[1L], "(Intercept)")) {
    if (beta[[1L]] != 0)
      pieces <- .sqlNum(beta[[1L]])
    if (!hasInt) beta <- beta[-1L]
  }
  if (hasInt) expected <- "(Intercept)"
  pos <- length(expected)

  # without an intercept model.matrix() codes the first factor it meets
  # by indicators, whatever the "factors" attribute says
  if (!hasInt) {
    isFac <- rownames(fac) %in% names(xlev)
    hit   <- which(fac > 0L & isFac, arr.ind = TRUE)
    if (nrow(hit)) {
      hit <- hit[order(hit[, 2L], hit[, 1L])[1L], ]
      fac[hit[1L], hit[2L]] <- 2L
    }
  }

  # --- terms ---
  for (j in seq_along(labs)) {

    v   <- rownames(fac)[fac[, j] > 0L]
    isF <- v %in% names(xlev)

    for (k in v[!isF]) {
      cls <- if (is.null(dc)) "numeric" else dc[[k]]
      if (!cls %in% c("numeric", "integer"))
        stop("predictSql() cannot translate the term '", labs[j], "': '", k,
             "' is of class '", cls, "'",
             if (startsWith(cls, "nmatrix"))
               " (poly(), ns(), bs() and matrices are not supported)"
             else if (cls == "logical")
               " (convert logical predictors to factors)", ".")
    }

    # coding matrix per factor: contrasts or indicators
    cod <- lapply(v[isF], function(k)
      .sqlCoding(xlev[[k]], contrasts[[k]], full = fac[k, j] == 2L,
                 ordered = identical(unname(dc[k]), "ordered")))

    # names of the design columns of the term, first variable fastest
    parts <- vector("list", length(v))
    parts[isF]  <- Map(function(k, m) paste0(k, colnames(m)), v[isF], cod)
    parts[!isF] <- as.list(v[!isF])
    cn <- Reduce(function(a, b) as.vector(outer(a, b, paste, sep = ":")),
                 parts)

    bj  <- beta[pos + seq_along(cn)]
    pos <- pos + length(cn)
    expected <- c(expected, cn)

    # A term whose coefficients are all zero (aliased, or shrunk by
    # glmnet) is written all the same: 0 * x is NULL for a missing x, and
    # the CASE of a factor still rejects unknown levels - as predict()
    # does, which needs every predictor whatever its coefficient
    num <- vapply(v[!isF], function(k) .sqlExpr(str2lang(k), ctx),
                  character(1L))

    if (!any(isF)) {
      pieces <- c(pieces, paste(c(.sqlNum(bj[[1L]]), num), collapse = " * "))
      next
    }

    # effect per combination of levels
    conds <- Map(function(k, lv) .sqlLevelConds(k, lv, ctx),
                 v[isF], xlev[v[isF]])
    grid  <- as.matrix(expand.grid(lapply(cod, function(m) seq_len(nrow(m)))))
    eff   <- apply(grid, 1L, function(ix) {
      r <- 1
      for (q in seq_along(cod))
        r <- kronecker(cod[[q]][ix[q], ], r)
      sum(r * bj)
    })
    when <- apply(grid, 1L, function(ix)
      paste(vapply(seq_along(ix), function(q) conds[[q]][ix[q]],
                   character(1L)), collapse = " AND "))

    case <- paste0("CASE",
                   paste0("\n    WHEN ", when, " THEN ", .sqlNum(eff),
                          collapse = ""),
                   "\n  END")
    pieces <- c(pieces, paste(c(case, num), collapse = " * "))
  }

  if (!identical(unname(expected), unname(names(beta))))
    stop("predictSql() cannot map the coefficients to the terms of the ",
         "model.\n  expected: ", paste(expected, collapse = ", "),
         "\n  found:    ", paste(names(beta), collapse = ", "))

  # --- offset() terms ---
  off <- attr(tt, "offset")
  if (length(off))
    pieces <- c(pieces, vapply(
      as.list(attr(tt, "variables"))[-1L][off],
      function(e) .sqlExpr(e[[2L]], ctx), character(1L)))

  if (length(pieces) == 0L) "0e0" else paste(pieces, collapse = "\n  + ")
}


# Coding matrix of a factor within a term (levels x design columns):
# indicators of all levels, or the contrasts the model was fitted with
#' @keywords internal
.sqlCoding <- function(lev, contr, full, ordered = FALSE) {

  m <- if (full) {
    diag(length(lev))
  } else {
    if (is.null(contr))
      contr <- getOption("contrasts")[[if (ordered) "ordered" else "unordered"]]
    if (is.character(contr))
      contr <- get(contr, mode = "function")
    if (is.function(contr)) contr(lev) else as.matrix(contr)
  }

  if (nrow(m) != length(lev))
    stop("predictSql(): the contrasts do not match the factor levels.")

  dimnames(m) <- list(lev, if (full) lev
                           else if (is.null(colnames(m))) seq_len(ncol(m))
                           else colnames(m))
  m
}


# One SQL condition per level of a factor variable of the model.  The
# variable is given by its label in the terms: a plain variable,
# factor(x) / as.factor(x), or cut(x, breaks).
#' @keywords internal
.sqlLevelConds <- function(label, lev, ctx) {

  e  <- str2lang(label)
  fn <- if (is.call(e)) .sqlFnName(e) else ""

  if (fn == "cut")
    return(.sqlCutConds(e, lev, ctx))

  if (fn %in% c("factor", "as.factor", "ordered", "as.ordered",
                "as.character")) {
    # The arguments are matched as R matches them - by name, by partial
    # name and by position: factor(levels = l, x = v) is valid R, and so
    # are labels given as the third argument or as 'lab ='.  ordered()
    # hands its arguments on to factor().  With labels the levels of the
    # model are not the values of the database: rejected.
    m <- .sqlArgs(e, if (fn %in% c("factor", "ordered")) base::factor
                     else function(x, ...) NULL)
    if (is.null(m$x))
      stop("predictSql() cannot translate '", label, "'.")
    if (!is.null(m$labels))
      stop("predictSql() cannot translate '", label,
           "': the argument 'labels' is not supported.")
    e <- m$x
  }

  stats::setNames(paste0(.sqlExpr(e, ctx), " = ", .sqlStr(lev)), lev)
}


# cut(x, breaks, ...) with constant breaks: one interval condition per
# level.  The levels are obtained by evaluating the call itself, so
# 'labels', 'right', 'include.lowest' and 'dig.lab' are honoured.
#' @keywords internal
.sqlCutConds <- function(e, lev, ctx) {

  cl <- match.call(utils::getS3method("cut", "default"), e)
  breaks <- eval(cl$breaks, ctx$env)
  if (!is.numeric(breaks) || length(breaks) < 2L)
    stop("predictSql() needs cut() with a vector of breaks: ", deparse1(e))

  arg <- function(nm, default)
    if (is.null(cl[[nm]])) default else eval(cl[[nm]], ctx$env)
  right  <- arg("right", TRUE)
  lowest <- arg("include.lowest", FALSE)

  breaks <- sort(breaks)
  n      <- length(breaks) - 1L

  probe      <- cl
  probe[[1L]] <- quote(cut)
  probe$x    <- numeric(0L)
  all        <- levels(eval(probe, ctx$env))
  if (length(all) != n || !all(lev %in% all))
    stop("predictSql() cannot translate ", deparse1(e))

  x  <- .sqlExpr(cl$x, ctx)
  lo <- breaks[-(n + 1L)]
  hi <- breaks[-1L]

  # closed at the lower end for right = FALSE, and for the lowest interval
  # with include.lowest (the highest one for right = FALSE)
  loOp <- ifelse(!right | (lowest & seq_len(n) == 1L), ">=", ">")
  hiOp <- ifelse(right  | (lowest & seq_len(n) == n),  "<=", "<")

  cond <- vapply(seq_len(n), function(i) {
    p <- c(if (is.finite(lo[i])) paste(x, loOp[i], .sqlNum(lo[i])),
           if (is.finite(hi[i])) paste(x, hiOp[i], .sqlNum(hi[i])))
    if (length(p)) paste(p, collapse = " AND ")
    else paste(x, "IS NOT NULL")
  }, character(1L))

  stats::setNames(cond, all)[lev]
}



# == expressions ==========================================================

# An R expression of a model formula as SQL.  Only a whitelist of
# operators and functions is translated; a subexpression without
# variables is evaluated and written as a number.
#' @keywords internal
.sqlExpr <- function(e, ctx) {

  if (is.numeric(e) && length(e) == 1L)
    return(.sqlNum(e))
  if (is.name(e))
    return(.sqlIdent(as.character(e), ctx))
  if (!is.call(e))
    stop("predictSql() cannot translate '", deparse1(e), "'.")

  if (length(all.vars(e)) == 0L) {
    val <- tryCatch(eval(e, ctx$env), error = function(err) NULL)
    if (is.numeric(val) && length(val) == 1L && is.finite(val))
      return(.sqlNum(val))
  }

  fn  <- .sqlFnName(e)
  n   <- length(e) - 1L
  a   <- function(i) .sqlExpr(e[[i + 1L]], ctx)
  f1  <- function(fmt) {
    if (n != 1L) bad()
    gsub("%s", a(1L), gsub("%1$s", "%s", fmt, fixed = TRUE), fixed = TRUE)
  }
  # guarded: outside its domain the logarithm is NULL, not an error
  ln  <- function(x)
    if (ctx$guard) sprintf("CASE WHEN %1$s > 0 THEN %2$s(%1$s) END", x, ctx$ln)
    else sprintf("%s(%s)", ctx$ln, x)
  bad <- function()
    stop("predictSql() cannot translate '", deparse1(e), "' to SQL.",
         call. = FALSE)

  switch(fn,
    # as.numeric() is not among them: for a factor it yields the level
    # numbers, which SQL cannot know
    "(" = , I = f1("(%s)"),
    "+" = if (n == 1L) a(1L) else sprintf("(%s + %s)", a(1L), a(2L)),
    "-" = if (n == 1L) sprintf("(-%s)", a(1L))
          else sprintf("(%s - %s)", a(1L), a(2L)),
    "*" = sprintf("(%s * %s)", a(1L), a(2L)),
    # 1e0 *: no integer division for integer columns
    "/" = sprintf(if (ctx$guard) "(1e0 * %s / NULLIF(%s, 0))"
                  else "(1e0 * %s / %s)", a(1L), a(2L)),
    "^" = sprintf("POWER(%s, %s)", a(1L), a(2L)),
    log = {
      # by name, not by position: log(base = 2, x = v) is valid R
      m <- .sqlArgs(e, function(x, base) NULL)
      if (is.null(m$x)) bad()
      x <- ln(.sqlExpr(m$x, ctx))
      if (is.null(m$base)) x
      else sprintf("(%s / %s)", x, ln(.sqlExpr(m$base, ctx)))
    },
    log2  = f1(paste0("(", ln("%s"), " / ", .sqlNum(log(2)), ")")),
    log10 = f1(paste0("(", ln("%s"), " / ", .sqlNum(log(10)), ")")),
    log1p = f1(ln("(1e0 + %s)")),
    exp   = f1("EXP(%s)"),
    expm1 = f1("(EXP(%s) - 1e0)"),
    sqrt  = f1(if (ctx$guard) "CASE WHEN %1$s >= 0 THEN SQRT(%1$s) END"
               else "SQRT(%s)"),
    abs   = f1("ABS(%s)"),
    bad())
}


# Arguments of a call matched to the formals of 'def', NULL if they do
# not match
#' @keywords internal
.sqlArgs <- function(e, def)
  tryCatch(as.list(match.call(def, e))[-1L], error = function(err) NULL)


# Name of the function of a call, without a namespace prefix
#' @keywords internal
.sqlFnName <- function(e) {
  head <- e[[1L]]
  if (is.call(head) && length(head) == 3L)      # pkg::fn
    head <- head[[3L]]
  if (is.name(head)) as.character(head) else ""
}


# Column of the database for a variable of the model
#' @keywords internal
.sqlIdent <- function(name, ctx) {

  if (name %in% names(ctx$vars))
    return(ctx$vars[[name]])

  if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", name))
    stop("'", name, "' is not a valid SQL column name; ",
         "map it to its column with 'vars'.", call. = FALSE)
  name
}


# Numbers in exponent notation, with as many digits (15 to 17) as it takes
# to read the same double back.  Exponent notation makes the literal a
# floating point number in every dialect (a plain 0.1 is a DECIMAL in
# several of them, with its own rounding rules).  Not a case for fm():
# the aim is exactness, not readability.
#' @keywords internal
.sqlNum <- function(x) {

  if (!all(is.finite(x)))
    stop("predictSql(): the model contains non-finite values.")

  s   <- formatC(x, digits = 14L, format = "e")
  bad <- as.numeric(s) != x
  s[bad] <- formatC(x[bad], digits = 16L, format = "e")
  sub("\\.?0*e", "e", s)
}


# String literals
#' @keywords internal
.sqlStr <- function(x)
  paste0("'", gsub("'", "''", x, fixed = TRUE), "'")


# Factor levels as part of a column name
#' @keywords internal
.sqlColName <- function(x)
  gsub("[^A-Za-z0-9_]", "_", x)


# Inverse link functions, written so that EXP() can neither overflow nor
# underflow: where R returns Inf or 0, a database may return an error
# (SQLite does so even for an underflow) or NULL, and Inf / Inf is lost in
# any case.  Each function is cut off where it has reached 0 or 1: the
# logistic function beyond +-700 (it differs from 0 or 1 by less than
# 1e-300 there), the complementary log-log links where the inner
# exponential leaves the range of EXP().  In between the usual formula is
# exact.  The log link is only protected against underflow: an overflow
# is its result.  A NULL argument falls through to ELSE and stays NULL.
#' @keywords internal
.sqlLinkInv <- function(link, x) {
  fmt <- switch(link,
    identity = "%s",
    log      = "CASE WHEN %s < -7e2 THEN 0e0 ELSE EXP(%s) END",
    logit    = paste("CASE WHEN %s > 7e2 THEN 1e0 WHEN %s < -7e2 THEN 0e0",
                     "ELSE 1e0 / (1e0 + EXP(-%s)) END"),
    cloglog  = paste("CASE WHEN %s > 6.5e0 THEN 1e0 WHEN %s < -7e2 THEN 0e0",
                     "ELSE 1e0 - EXP(-EXP(%s)) END"),
    loglog   = paste("CASE WHEN %s < -6.5e0 THEN 0e0 WHEN %s > 7e2 THEN 1e0",
                     "ELSE EXP(-EXP(-%s)) END"),
    cauchit  = paste0("5e-01 + ATAN(%s) / ", .sqlNum(pi)),
    inverse  = "1e0 / %s",
    sqrt     = "POWER(%s, 2e0)",
    "1/mu^2" = "1e0 / SQRT(%s)",
    stop("predictSql() does not support the link '", link,
         "' (no closed form in SQL)."))
  vapply(x, function(xi) gsub("%s", xi, fmt, fixed = TRUE), character(1L),
         USE.NAMES = FALSE)
}


# Probabilities of a softmax over several linear predictors.  Not as
# EXP(eta_k) / sum(EXP(eta_j)), which overflows to Inf / Inf for large
# predictors, but as 1 / sum(EXP(eta_j - eta_k)) with the differences
# limited to +-700: beyond that the term is 0 or the probability is 0 to
# 300 digits.  A NULL difference stays NULL.
#' @keywords internal
.sqlSoftmax <- function(refs) {
  vapply(seq_along(refs), function(k) {
    d <- sprintf("(%s - %s)", refs[-k], refs[k])
    paste0("1e0 / (1e0",
           paste0(sprintf(paste(" + EXP(CASE WHEN %1$s > 7e2 THEN 7e2",
                                "WHEN %1$s < -7e2 THEN -7e2 ELSE %1$s END)"),
                          d), collapse = ""),
           ")")
  }, character(1L))
}


# The label of the largest of several values.  Ties go to the first; no
# ELSE, so that a NULL among the values yields NULL.
#' @keywords internal
.sqlArgmax <- function(refs, labels) {
  K <- length(refs)
  when <- vapply(seq_len(K), function(k) {
    paste(c(sprintf("%s > %s",  refs[k], refs[seq_len(k - 1L)]),
            sprintf("%s >= %s", refs[k], refs[-seq_len(k)])),
          collapse = " AND ")
  }, character(1L))
  paste0("CASE",
         paste0("\n    WHEN ", when, " THEN ", .sqlStr(labels), collapse = ""),
         "\n  END")
}


# Environment for the constants of a formula (breaks of cut(), ...)
#' @keywords internal
.sqlFormulaEnv <- function(fit) {
  env <- tryCatch(environment(stats::formula(fit)), error = function(e) NULL)
  if (is.null(env)) globalenv() else env
}



# == statements ===========================================================

# The definitions refer to each other by "{name}".  A factor level or a
# quoted column name may contain the same characters, so everything that
# reads or replaces references works on the text outside of SQL string
# literals and quoted identifiers only: .sqlOutside() applies a function
# to exactly these pieces.  Quoted identifiers are "name", [name] (SQL
# Server) and `name`, each with its doubled closing character as escape.  References are replaced in one pass; what has
# been inserted is not searched again.
#' @keywords internal
.sqlOutside <- function(x, f) {
  lit <- gregexpr(paste("'(?:[^']|'')*'", "\"(?:[^\"]|\"\")*\"",
                        "\\[(?:[^\\]]|\\]\\])*\\]", "`(?:[^`]|``)*`",
                        sep = "|"), x, perl = TRUE)
  regmatches(x, lit, invert = TRUE) <-
    list(vapply(regmatches(x, lit, invert = TRUE)[[1L]], f, character(1L),
                USE.NAMES = FALSE))
  x
}

.sqlRefPattern <- "\\{[A-Za-z0-9_]+\\}"

# Names of the definitions an expression refers to
#' @keywords internal
.sqlRefs <- function(x) {
  found <- character(0L)
  .sqlOutside(x, function(piece) {
    m <- regmatches(piece, gregexpr(.sqlRefPattern, piece))[[1L]]
    found <<- c(found, substr(m, 2L, nchar(m) - 1L))
    piece
  })
  unique(found)
}

# An expression with each reference replaced by to(name)
#' @keywords internal
.sqlReplaceRefs <- function(x, to) {
  .sqlOutside(x, function(piece) {
    pos <- gregexpr(.sqlRefPattern, piece)
    m   <- regmatches(piece, pos)[[1L]]
    if (length(m))
      regmatches(piece, pos) <-
        list(vapply(substr(m, 2L, nchar(m) - 1L), to, character(1L)))
    piece
  })
}


# A definition with all its references replaced by their expressions
#' @keywords internal
.sqlInline <- function(defs, name)
  .sqlReplaceRefs(defs[[name]],
                  function(r) paste0("(", .sqlInline(defs, r), ")"))


#' @keywords internal
.sqlUpdate <- function(exprs, table) {
  paste0("UPDATE ", table, "\nSET ",
         paste0(names(exprs), " =\n  ", exprs, collapse = ",\n"))
}


# ALTER TABLE for the columns of an UPDATE: floating point numbers, a
# string for the predicted class
#' @keywords internal
.sqlAlter <- function(spec, table, dialect) {
  isChr <- spec$out == "class"
  type  <- ifelse(isChr, "VARCHAR(100)",
                  if (dialect == "mssql") "FLOAT" else "DOUBLE PRECISION")
  add   <- if (dialect == "exasol") " ADD COLUMN IF NOT EXISTS " else " ADD "
  paste0("ALTER TABLE ", table, add, names(spec$out), " ", type, ";")
}


# SELECT of the predictions.  Without 'id' all columns of the table are
# returned and the expressions are inlined.  With 'id' the definitions are
# computed stage by stage in nested subqueries, each one once.
#' @keywords internal
.sqlSelect <- function(spec, exprs, table, id, into = NULL) {

  into <- if (is.null(into)) "" else paste0("\nINTO ", into)

  if (is.null(id))
    return(paste0("SELECT t.*,\n",
                  paste0("  ", exprs, " AS ", names(exprs), collapse = ",\n"),
                  into, "\nFROM ", table, " t"))

  # only what the outputs need
  need <- character(0L)
  walk <- function(nm) {
    if (nm %in% need) return()
    need <<- c(need, nm)
    for (r in .sqlRefs(spec$defs[[nm]])) walk(r)
  }
  for (nm in spec$out) walk(nm)
  defs <- spec$defs[names(spec$defs) %in% need]

  # stage of a definition: one more than the deepest it refers to
  depth <- stats::setNames(integer(length(defs)), names(defs))
  for (nm in names(defs))
    depth[nm] <- 1L + max(0L, depth[.sqlRefs(defs[[nm]])])
  D <- max(depth)

  alias <- stats::setNames(paste0(toupper(names(defs)), "_"), names(defs))
  body  <- vapply(defs, .sqlReplaceRefs, character(1L),
                  to = function(r) alias[[r]])

  query <- table
  for (d in seq_len(D)) {
    cur  <- names(defs)[depth == d]
    cols <- if (d < D) {
      c(id, alias[depth < d], paste0(body[cur], " AS ", alias[cur]))
    } else {
      # last stage: the outputs under their names
      c(id, vapply(names(spec$out), function(o) {
        nm <- spec$out[[o]]
        paste0(if (depth[[nm]] == D) body[[nm]] else alias[[nm]], " AS ", o)
      }, character(1L)))
    }
    from  <- if (d == 1L) query
             else paste0("(\n  ",
                         # indented, but not within a literal
                         .sqlOutside(query, function(piece)
                           gsub("\n", "\n  ", piece, fixed = TRUE)),
                         "\n) q", d - 1L)
    query <- paste0("SELECT ", paste(cols, collapse = ",\n  "),
                    if (d == D) into, "\nFROM ", from)
  }
  query
}
