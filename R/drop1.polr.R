
#' @keywords internal
.polr_term_names <- function(model) {
  # polr stores thresholds in zeta, not as "(Intercept)" coefficient —
  # therefore simply return term labels without intercept check
  labels(terms(model))
}

#' @keywords internal
.polr_df_terms <- function(model) {
  # Compute degrees of freedom for all terms in one pass (no recursion)
  asgn  <- attr(model.matrix(model), "assign")
  terms <- labels(terms(model))
  setNames(
    vapply(seq_along(terms),
           function(i) sum(asgn == i),
           integer(1L)),
    terms
  )
}

#' @keywords internal
.polr_relatives <- function(term, names, factors) {
  is.relative <- function(t1, t2)
    all(!(factors[, t1] & !factors[, t2]))
  if (length(names) == 1L)
    return(NULL)
  which.term <- which(term == names)
  idx <- seq_along(names)[-which.term]
  idx[sapply(names[-which.term], function(t2) is.relative(term, t2))]
}

#' Likelihood-ratio drop1 table for polr models
#'
#' Computes a Type-II analysis-of-deviance table for a fitted
#' \code{\link[MASS]{polr}} model by sequentially dropping each term and
#' comparing the resulting likelihood-ratio statistic against the full model
#' (or the model without higher-order relatives for hierarchical terms).
#'
#' @details
#' Reduced models are fitted on the \emph{model matrix} of the full model
#' (with the relevant columns removed) rather than by manipulating the
#' original formula.  This is intentional: it avoids complex formula
#' reconstruction for transformed terms (\code{poly()}, \code{ns()}, etc.)
#' and is consistent with the Type-II approach used in \pkg{car}.  As a
#' consequence, \code{offset}, \code{subset}, and special formula terms are
#' not re-evaluated in the reduced fits — they are already baked into the
#' model matrix.
#'
#' Fits that fail to converge are silently set to \code{NA}.  Note that
#' \code{polr()} sometimes issues convergence \emph{warnings} without
#' throwing an error; such fits are accepted as-is (consistent with the
#' behaviour of \code{drop1.default}).
#'
#' @param mod A fitted \code{"polr"} object.
#' @param ... Currently unused.
#' @return A \code{data.frame} of class \code{c("anova", "data.frame")}.
#' @keywords internal
.drop1.polr <- function(mod, ...) {
  
  # Guard: intercept-only model has no terms to drop
  nms <- .polr_term_names(mod)
  if (length(nms) == 0L) {
    result <- data.frame(
      AIC          = numeric(0),
      "LR Chisq"   = numeric(0),
      Df           = integer(0),
      "Pr(>Chisq)" = numeric(0),
      check.names  = FALSE
    )
    class(result) <- c("anova", "data.frame")
    attr(result, "heading") <- c(
      "Analysis of Deviance Table (Type II tests)\n",
      sprintf("Response: %s", .response_name(mod))
    )
    return(result)
  }
  
  fac     <- attr(terms(mod), "factors")
  n.terms <- length(nms)
  
  # Compute model frame once
  mf   <- model.frame(mod)
  y    <- model.response(mf)
  wt   <- model.weights(mf)
  
  X    <- model.matrix(mod)
  asgn <- attr(X, "assign")
  df   <- .polr_df_terms(mod)
  
  # Map term name to column indices in model matrix
  which.nms <- function(name) {
    term.idx <- match(name, nms)
    which(asgn == term.idx)
  }
  
  # Safe polr fitter: returns NULL on error, issues message on convergence warning
  .safe_polr <- function(formula, weights) {
    mod <- tryCatch(
      withCallingHandlers(
        MASS::polr(formula, weights = weights),
        warning = function(w) {
          if (grepl("conver|probabilities numerically 0 or 1", conditionMessage(w),
                    ignore.case = TRUE))
            message("polr convergence warning: ", conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) NULL
    )
    mod
  }
  
  aic <- numeric(n.terms)
  LR  <- numeric(n.terms)
  p   <- numeric(n.terms)
  
  for (term in seq_len(n.terms)) {
    rels      <- nms[.polr_relatives(nms[term], nms, fac)]
    exclude.1 <- unlist(lapply(c(nms[term], rels), which.nms),
                        use.names = FALSE)
    
    mod.1 <- if (n.terms > 1L)
      .safe_polr(y ~ X[, -c(1L, exclude.1)], weights = wt)
    else
      .safe_polr(y ~ 1L, weights = wt)
    
    if (is.null(mod.1)) {
      aic[term] <- NA_real_
      LR[term]  <- NA_real_
      p[term]   <- NA_real_
      next
    }
    
    dev.1 <- deviance(mod.1)
    
    mod.2 <- if (length(rels) == 0L) {
      mod
    } else {
      exclude.2 <- unlist(lapply(rels, which.nms), use.names = FALSE)
      .safe_polr(y ~ X[, -c(1L, exclude.2)], weights = wt)
    }
    
    if (is.null(mod.2)) {
      aic[term] <- NA_real_
      LR[term]  <- NA_real_
      p[term]   <- NA_real_
      next
    }
    
    dev.2     <- deviance(mod.2)
    LR[term]  <- dev.1 - dev.2
    p[term]   <- pchisq(LR[term], df[term], lower.tail = FALSE)
    aic[term] <- AIC(mod.1)
  }
  
  result <- data.frame(
    AIC          = aic,
    "LR Chisq"   = LR,
    Df           = df,
    "Pr(>Chisq)" = p,
    check.names  = FALSE
  )
  rownames(result) <- nms
  class(result)    <- c("anova", "data.frame")
  attr(result, "heading") <- c(
    "Analysis of Deviance Table (Type II tests)\n",
    sprintf("Response: %s", .response_name(mod))
  )
  result
}






#' @keywords internal
.summary_polr <- function(x, conf.level = 0.95, output = c("coef", "or")) {
  
  output <- match.arg(output)
  alpha  <- 1 - (1 - conf.level) / 2
  
  r.summary  <- summary(x)
  coefs      <- r.summary$coefficients  # all rows: predictors + thresholds
  
  # Split into predictors and thresholds
  zeta_names <- names(x$zeta)
  pred_names <- setdiff(rownames(coefs), zeta_names)
  
  # --- predictor block ---
  coefs_pred <- coefs[pred_names, , drop = FALSE]
  est <- coefs_pred[, "Value"]
  se  <- coefs_pred[, "Std. Error"]
  z   <- abs(est / se)
  p   <- 2 * (1 - pnorm(z))
  
  d.coef <- data.frame(
    id        = pred_names,
    estimate  = est,
    lci       = x$ci[pred_names, 1L],
    uci       = x$ci[pred_names, 2L],
    z         = z,
    pval      = p,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  
  # --- threshold block ---
  # confint.default does not cover zeta — derive CI from SE in summary
  zeta_se <- coefs[zeta_names, "Std. Error"]
  d.zeta <- data.frame(
    id       = zeta_names,
    estimate = x$zeta,
    lci      = x$zeta - qnorm(alpha) * zeta_se,
    uci      = x$zeta + qnorm(alpha) * zeta_se,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  
  # --- rename CI columns after conf.level ---
  lci_label <- sprintf("%s-lci", fm(conf.level, fmt = "%", digits = 0))
  names(d.coef)[names(d.coef) == "lci"] <- lci_label
  names(d.zeta)[names(d.zeta) == "lci"] <- lci_label
  
  # --- transform to OR if requested ---
  if (output == "or") {
    cols <- c("estimate", lci_label, "uci")
    d.coef[cols] <- exp(d.coef[cols])
    # ORs for thresholds are not meaningful
    d.zeta <- NULL
  }
  
  structure(
    list(
      coefficients = d.coef,
      thresholds   = d.zeta,
      call         = x$call,
      nobs         = nobs(x),
      na.action    = if (is.null(x$na.action)) 0L else length(x$na.action),
      PseudoR2     = pseudoR2(x, which = "all"),
      conf.level   = conf.level,
      output       = output,
      results      = if (output == "or") "Odds Ratios" else "Coefficients"
    ),
    class = c("SummaryFitMod", "list")
  )
}



#' @keywords internal
.print_polr <- function(x, digits = 3, pdigits = 3,
                        conf.level = 0.95,
                        output = c("coef", "or"), ...) {
  
  output <- match.arg(output)
  xx     <- if (inherits(x, "SummaryFitMod")) x
  else .summary_polr(x, conf.level = conf.level, output = output)
  
  lci_col <- grep("-lci$", names(xx$coefficients), value = TRUE)
  
  # Format coefficient table
  out_coef <- cbind(
    " "    = xx$coefficients$id,
    fm(xx$coefficients[, c("estimate", lci_col, "uci")], digits = digits),
    "pval" = fm(xx$coefficients$pval, fmt = "p",
                    digits = pdigits, eps = 10^-pdigits),
    " "    = fm(xx$coefficients$pval, fmt = "*")
  )
  
  # Format threshold table (coef mode only)
  if (!is.null(xx$thresholds)) {
    out_zeta <- cbind(
      " " = xx$thresholds$id,
      fm(xx$thresholds[, c("estimate", lci_col, "uci")], digits = digits)
    )
  }
  
  # Print
  cat("\nCall:\n",
      paste(deparse(xx$call), sep = "\n", collapse = "\n"),
      "\n", sep = "")
  cat(sprintf("\n%s:\n", xx$results))
  print(out_coef, quote = FALSE, print.gap = 2L, row.names = FALSE)
  
  if (!is.null(xx$thresholds)) {
    cat("\nThresholds:\n")
    print(out_zeta, quote = FALSE, print.gap = 2L, row.names = FALSE)
  }
  
  cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  cat(sprintf("\nObs (NAs): %d (%d)", xx$nobs, xx$na.action))
  cat("\tPseudo R\u00B2 (McFadden):",
      fm(xx$PseudoR2["McFadden"], digits = digits))
  cat("   AIC:", fm(xx$PseudoR2["AIC"], digits = digits))
  cat("\n\n")
  
  invisible(xx)
}


#' @export
OddsRatio.polr <- function(x, conf.level = 0.95, digits = 3, ...) {
  .summary_polr(x, conf.level = conf.level, output = "or")
}