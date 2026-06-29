
#' Print method for FitMod objects
#'
#' Displays a formatted summary of a fitted model of class \code{"FitMod"},
#' with confidence intervals, p-values, reference category headers for factor
#' predictors, and model fit statistics.  The output style is consistent
#' across all supported model types and loosely follows Stata conventions.
#'
#' @param x A fitted model of class \code{"FitMod"}.
#' @param digits Integer.  Number of significant digits for estimates and
#'   confidence intervals.  Default is \code{3}.
#' @param pdigits Integer.  Number of digits for p-values.  Default is
#'   \code{3}.
#' @param conf.level Numeric scalar in \eqn{(0, 1)}.  Confidence level for
#'   intervals.  Default is \code{0.95}.
#' @param output Character string controlling the scale of the coefficient
#'   table.  Supported values depend on the model type:
#'   \describe{
#'     \item{\code{"coef"}}{Raw coefficients (default for \code{lm},
#'       \code{glm}, \code{lmrob}, \code{polr}, parametric survival).}
#'     \item{\code{"or"}}{Odds ratios - \code{exp(coef)} - for logistic
#'       and ordinal models.}
#'     \item{\code{"irr"}}{Incidence rate ratios - \code{exp(coef)} - for
#'       Poisson and negative binomial models.}
#'     \item{\code{"hr"}}{Hazard ratios (default for \code{coxph}).}
#'     \item{\code{"lhr"}}{Log hazard ratios for \code{coxph}.}
#'     \item{\code{"tr"}}{Time ratios - \code{exp(coef)} - (default for
#'       parametric survival models).}
#'     \item{\code{"genuine"}}{Passes through to the original
#'       \code{summary()} / \code{print()} of the underlying model object.}
#'   }
#'   If \code{NULL} (default), an appropriate value is chosen automatically
#'   based on the model class.
#' @param useProfile Logical.  If \code{TRUE} and the model is a
#'   \code{glm}, profile-likelihood confidence intervals are computed via
#'   \code{\link[stats]{confint}}.  Otherwise (default) Wald intervals via
#'   \code{\link[stats]{confint.default}} are used.  Ignored for non-GLM
#'   models.
#' @param vcov Character string specifying the type of
#'   heteroscedasticity-consistent covariance matrix to use for standard
#'   errors, e.g. \code{"HC3"} (recommended), \code{"HC0"}, \code{"HC1"}.
#'   Passed to \code{\link[sandwich]{vcovHC}}.  Supported for \code{lm},
#'   \code{glm}, and \code{lmrob} models; ignored with a message for all
#'   others.  If \code{NULL} (default) the model's own standard errors are
#'   used.
#' @param ... Further arguments passed to the underlying print helper.
#'
#' @return Invisibly returns the result of \code{summary(x)}.
#'
#' @details
#' Factor predictors are displayed with a header row showing the reference
#' category and an overall p-value from \code{\link[stats]{drop1}}.
#' Dummy-coded rows are indented below the header.
#'
#' For negative binomial models an additional overdispersion block is
#' printed showing the parameter \eqn{\alpha = 1/\theta} (Stata
#' convention) with a one-sided likelihood-ratio test against the Poisson
#' model.
#'
#' For quasi-Poisson and quasi-binomial models, pseudo-R\eqn{^2} and AIC
#' are not available and a note is displayed instead.
#'
#' @examples
#' fitLm <- fitMod(Fertility ~ ., swiss)
#' print(fitLm)
#' print(fitLm, vcov = "HC3")
#'
#' fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
#' print(fitLogit)
#' print(fitLogit, output = "or")
#' print(fitLogit, output = "or", vcov = "HC3")
#'
#' fitPois <- fitMod(daysabs ~ mathnce + langnce + gender,
#'                   Lahigh, fitfn = "poisson")
#' print(fitPois, output = "irr")
#'
#' fitCox <- fitMod(Surv(foltime, folstatus) ~ gender, Whas100,
#'                  fitfn = "coxph")
#' print(fitCox)
#' print(fitCox, output = "lhr")
#'
#' fitWei <- fitMod(Surv(foltime, folstatus) ~ gender + age, Whas100,
#'                  fitfn = "weibull")
#' print(fitWei)
#' print(fitWei, output = "coef")
#' print(fitWei, output = "genuine")
#'
#' @seealso \code{\link{fitMod}}, \code{\link{predict.FitMod}}



#' @family modelling  
#' @concept modelling
#'
#'
#' @export
print.FitMod <- function(x, digits = 3, pdigits = 3,
                         conf.level = 0.95,
                         output = NULL,
                         useProfile = FALSE,
                         vcov = NULL,
                         ...) {
  
  # Determine model-appropriate default if output not specified
  if (is.null(output)) {
    output <- if (inherits(x, "coxph"))
      "hr"
    else if (inherits(x, "survreg") && !inherits(x, "tobit"))
      "tr"
    else
      "coef"
  }
  
  output <- match.arg(output, c("coef", "or", "irr", "hr", "lhr", "tr", "genuine"))
  
  # --- genuine: pass through to original summary/print ---
  if (output == "genuine") {
    obj <- x
    class(obj) <- class(obj)[class(obj) != "FitMod"]
    print(summary(obj), ...)
    return(invisible(x))
  }
  
  if (inherits(x, "glm") || inherits(x, "lm") || inherits(x, "lmrob")) {
    .print_lm(x, digits, pdigits, conf.level,
              useProfile = useProfile, output = output,
              vcov = vcov, ...)
  } else if (inherits(x, "multinom")) {
    if (!is.null(vcov))
      message("Note: vcov is not supported for multinom - standard SE used")
    .print_multinom(x, digits, pdigits, conf.level, ...)
  } else if (inherits(x, "polr")) {
    if (!is.null(vcov))
      message("Note: vcov is not supported for polr - standard SE used")
    .print_polr(x, digits, pdigits, conf.level, output = output, ...)
  } else if (inherits(x, "tobit")) {
    .print_tobit(x, digits, pdigits, conf.level, ...)
  } else if (inherits(x, "zeroinfl")) {
    .print_zeroinfl(x, digits, pdigits, conf.level, ...)
  } else if (inherits(x, "coxph")) {
    .print_coxph(x, digits, pdigits, conf.level, output = output, ...)
  } else if (inherits(x, "survreg") && !inherits(x, "tobit")) {
    .print_survreg(x, digits, pdigits, conf.level, output = output, ...)
  } else if (inherits(x, "FitMod.lme4")) {
    .print_mixed(x, digits, pdigits, conf.level, output = output, ...)
  } else if (inherits(x, "lmerMod") || inherits(x, "glmerMod")) {
    .print_mixed(x, digits, pdigits, conf.level, output = output, ...)
  } else if (x$fitfn %in% c("randomForest", "nnet", "rpart", "C5.0",
                          "svm", "naiveBayes", "lda", "qda",
                          "glmnet", "xgboost")) {
    .print_ml(x, digits, pdigits, ...)
  } else {
    class(x) <- class(x)[class(x) != "FitMod"]
    print(x, ...)
  }
}



#' @keywords internal
.print_lm <- function(x, digits = 3, pdigits = 3,
                      conf.level = 0.95,
                      useProfile = FALSE,
                      output = c("coef", "or", "irr"),
                      vcov = NULL, ...) {
  
  output  <- match.arg(output)
  isGLM   <- inherits(x, "glm")
  isLMROB <- inherits(x, "lmrob")
  xx      <- summary(x)
  
  # --- override SE/CI/p-values with sandwich estimator if requested ---
  if (!is.null(vcov)) {
    if (!requireNamespace("sandwich", quietly = TRUE))
      stop("Package 'sandwich' must be installed for robust standard errors")
    
    V <- tryCatch(
      sandwich::vcovHC(x, type = vcov),
      error = function(e) {
        warning("vcovHC failed for this model type: ", conditionMessage(e),
                "; falling back to standard SE", call. = FALSE)
        NULL
      }
    )
    
    if (!is.null(V)) {
      se_robust <- sqrt(diag(V))
      # Match SE to coefficient names
      nm  <- rownames(xx$coefficients)
      se  <- se_robust[nm]
      est <- xx$coefficients[, 1L]
      # Recalculate t/z, p-values and CI with robust SE
      tval <- est / se
      df_r <- if (isGLM) Inf else xx$df[2L]
      pval <- 2 * pt(-abs(tval), df = df_r)
      z_alpha <- qnorm(1 - (1 - conf.level) / 2)
      ci <- cbind(est - z_alpha * se, est + z_alpha * se)
      rownames(ci) <- nm
      
      # Overwrite relevant columns in xx$coefficients
      xx$coefficients[, "Std. Error"]                     <- se
      xx$coefficients[, if (isGLM) "z value" else "t value"] <- tval
      xx$coefficients[, 4L]                               <- pval
    }
  } else {
    ci <- if (useProfile && isGLM)
      confint(x, level = conf.level)
    else
      confint.default(x, level = conf.level)
  }
  
  ref <- refLevel(x)
  
  # --- overall p-values per predictor via drop1 ---
  anova_p <- if (isLMROB) {
    pvals <- xx$coefficients[names(ref), 4L]
    setNamesX(pvals, names(ref))
  } else if (isGLM) {
    drop1(x, test = "Chisq")[names(ref), "Pr(>Chi)"]
  } else {
    drop1(x, test = "F")[names(ref), "Pr(>F)"]
  }
  
  # --- header ---
  cat("\nCall:\n",
      paste(deparse(xx$call), sep = "\n", collapse = "\n"),
      "\n", sep = "")
  
  # --- coefficients ---
  if (length(xx$aliased) == 0L) {
    cat("\nNo Coefficients\n")
  } else {
    df  <- xx$df
    rdf <- df[2L]
    
    if ((nsingular <- df[3L] - df[1L]) > 0L)
      cat("\nCoefficients: (", nsingular,
          " not defined because of singularities)\n", sep = "")
    else {
      coef_header <- switch(output,
                            coef = "\nCoefficients:\n",
                            or   = "\nOdds Ratios:\n",
                            irr  = "\nIncidence Rate Ratios:\n"
      )
      cat(coef_header)
    }
    
    # Handle aliased coefficients
    coefs <- xx$coefficients
    if (any(aliased <- xx$aliased)) {
      cn    <- names(aliased)
      coefs <- matrix(NA_real_, length(aliased), 4L,
                      dimnames = list(cn, colnames(coefs)))
      coefs[!aliased, ] <- xx$coefficients
    }
    
    # Transform to OR/IRR if requested
    est <- coefs[, 1L]
    ci_out <- ci
    if (output %in% c("or", "irr")) {
      est    <- exp(est)
      ci_out <- exp(ci)
    }
    
    # Build output matrix
    ci_label <- sprintf(c("%s-lci", "uci"),
                        fm(conf.level, fmt = "%",
                           digits = max(0L, nDec(as.character(signif(conf.level))) - 2L)))
    
    est_label <- switch(output,
                        coef = "estimate",
                        or   = "OR",
                        irr  = "IRR"
    )
    
    out <- cbind(
      fm(cbind(est, ci_out), digits = digits),
      fm(coefs[, 4L], fmt = "p",
                       eps = 10^-pdigits, digits = pdigits),
      fm(coefs[, 4L], fmt = "*")
    )
    colnames(out) <- c(est_label, ci_label, "p-val", "")
    
    # Insert variable-level summary rows and indent coefficient rows
    for (i in seq_along(ref)) {
      pat <- sprintf("^%s", gsub("[^a-zA-Z0-9_]", " ", names(ref)[i]))
      rnr <- grep(pat, gsub("[^a-zA-Z0-9_]", " ", rownames(out)))[1L]
      if (is.na(rnr)) next
      
      p <- anova_p[i]
      summary_row <- c(
        rep(".", 3L),
        fm(p, fmt = "p", eps = 10^-pdigits, digits = pdigits),
        fm(p, fmt = "*")
      )
      out <- appendX(out, rbind(summary_row), after = rnr - 1L, rows = TRUE)
      rownames(out)[rnr] <- sprintf("%s (ref: %s)", names(ref)[i], ref[i])
      dummy_rows <- grep(sprintf("^%s", names(ref)[i]), rownames(out))
      rownames(out)[dummy_rows] <- sub(
        names(ref)[i],
        paste0(names(ref)[i], " "),
        rownames(out)[dummy_rows]
      )
    }
    
    print(out, quote = FALSE, right = TRUE, print.gap = 2L)
    cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  }
  
  
  # --- overdispersion block for negative binomial models ---
  if (inherits(x, "negbin")) {
    cat("\nOverdispersion parameter (\u03b1 = 1/\u03b8):\n")
    
    theta    <- x$theta
    se_theta <- x$SE.theta
    
    # Convert to alpha = 1/theta (Stata convention)
    # SE via delta method: se(1/theta) = se(theta) / theta^2
    alpha    <- 1 / theta
    se_alpha <- se_theta / theta^2
    
    # Wald CI for alpha
    z_alpha_ci <- qnorm(1 - (1 - conf.level) / 2)
    lci        <- alpha - z_alpha_ci * se_alpha
    uci        <- alpha + z_alpha_ci * se_alpha
    
    # One-sided LR test H0: alpha = 0 (i.e. no overdispersion, equivalent to Poisson)
    # Divided by 2 because alpha is bounded below at 0 (boundary test)
    pois_fit <- tryCatch({
      mf <- model.frame(x)
      wt <- model.weights(mf)
      args <- list(
        formula = formula(x),
        family  = "poisson",
        data    = mf
      )
      if (!is.null(wt)) args$weights <- wt
      do.call(glm, args)
    },
    error = function(e) NULL
    )
    
    p_disp <- if (!is.null(pois_fit)) {
      lr_stat <- 2 * as.numeric(logLik(x) - logLik(pois_fit))
      pchisq(lr_stat, df = 1, lower.tail = FALSE) / 2
    } else {
      NA_real_
    }
    
    # Format as single-row table consistent with coefficient output
    ci_label <- sprintf(c("%s-lci", "uci"),
                        fm(conf.level, fmt = "%",
                           digits = max(0L, nDec(as.character(signif(conf.level))) - 2L)))
    out_alpha <- cbind(
      fm(cbind(alpha, lci, uci), digits = digits),
      fm(p_disp, fmt = "p", eps = 10^-pdigits, digits = pdigits),
      fm(p_disp, fmt = "*")
    )
    colnames(out_alpha) <- c("estimate", ci_label, "p-val", "")
    rownames(out_alpha) <- ""
    
    print(out_alpha, quote = FALSE, right = TRUE, print.gap = 2L)
    cat("---\n")
  }
  
  
  # --- footer ---
  n_na <- if (is.null(x$na.action)) 0L else length(x$na.action)
  cat(sprintf("\nObs (NAs): %d (%d)", nobs(x), n_na))
  
  if (isLMROB) {
    if (!is.null(xx$r.squared))
      cat("\tR\u00B2/R\u00B2adj:",
          paste(formatC(c(xx$r.squared, xx$adj.r.squared),
                        digits = digits), collapse = "/"))
    cat(sprintf("   Scale: %s", fm(xx$sigma, digits = digits)))
    
  } else if (!is.null(xx$fstatistic)) {
    cat("\tR\u00B2/R\u00B2adj:",
        paste(formatC(c(xx$r.squared, xx$adj.r.squared),
                      digits = digits), collapse = "/"))
    
  } else if (isGLM) {
    is_quasi <- grepl("quasi", x$family$family, ignore.case = TRUE)
    if (is_quasi) {
      cat("\tPseudo R\u00B2/AIC: not available (quasi model)")
    } else {
      pr2 <- tryCatch(pseudoR2(x)["McFadden"], error = function(e) NA_real_)
      aic <- tryCatch(AIC(x), error = function(e) NA_real_)
      if (!is.na(pr2))
        cat("\tPseudo R\u00B2 (McFadden):", fm(pr2, digits = digits))
      if (!is.na(aic))
        cat("   AIC:", fm(aic, digits = digits))
    }
  }
  
  cat("\n\n")
  invisible(xx)
}




#' @keywords internal
.print_zeroinfl <- function(x, digits = 3, pdigits = 3,
                            conf.level = 0.95, ...) {
  
  xx      <- summary(x)
  alpha   <- 1 - (1 - conf.level) / 2
  z_alpha <- qnorm(alpha)
  
  ci_label <- sprintf(c("%s-lci", "uci"),
                      fm(conf.level, fmt = "%",
                         digits = max(0L, nDec(as.character(signif(conf.level))) - 2L)))
  
  # Helper: format one coefficient block
  .fmt_block <- function(coefs) {
    est <- coefs[, "Estimate"]
    se  <- coefs[, "Std. Error"]
    ci  <- cbind(est - z_alpha * se, est + z_alpha * se)
    out <- cbind(
      fm(cbind(est, ci), digits = digits),
      fm(coefs[, "Pr(>|z|)"], fmt = "p",
         eps = 10^-pdigits, digits = pdigits),
      fm(coefs[, "Pr(>|z|)"], fmt = "*")
    )
    colnames(out) <- c("estimate", ci_label, "p-val", "")
    out
  }
  
  # --- header ---
  cat("\nCall:\n",
      paste(deparse(xx$call), sep = "\n", collapse = "\n"),
      "\n", sep = "")
  
  # --- count model ---
  # xx$link refers to the zero-inflation part (logit) - count link depends on dist
  count_link <- switch(xx$dist,
                       poisson   = "log",
                       negbin    = "log",
                       geometric = "log",
                       xx$link
  )
  cat(sprintf("\nCount model (%s with %s link):\n",
              xx$dist, count_link))
  print(.fmt_block(xx$coefficients$count),
        quote = FALSE, right = TRUE, print.gap = 2L)
  
  
  # --- zero-inflation model ---
  cat("\nZero-inflation model (binomial with logit link):\n")
  print(.fmt_block(xx$coefficients$zero),
        quote = FALSE, right = TRUE, print.gap = 2L)
  
  cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  
  # --- footer ---
  n_na <- if (is.null(x$na.action)) 0L else length(x$na.action)
  cat(sprintf("\nObs (NAs): %d (%d)", xx$n, n_na))
  cat(sprintf("   Log-lik: %s", fm(xx$loglik, digits = digits)))
  cat(sprintf("   AIC: %s", fm(AIC(x), digits = digits)))
  cat("\n\n")
  
  invisible(xx)
}



#' @keywords internal
.print_coxph <- function(x, digits = 3, pdigits = 3,
                         conf.level = 0.95,
                         output = c("hr", "lhr"), ...) {
  
  output  <- match.arg(output)
  xx      <- summary(x)
  coefs   <- xx$coefficients
  ci_mat  <- xx$conf.int
  
  alpha   <- 1 - (1 - conf.level) / 2
  z_alpha <- qnorm(alpha)
  
  ci_label <- sprintf(c("%s-lci", "uci"),
                      fm(conf.level, fmt = "%",
                         digits = max(0L, nDec(as.character(signif(conf.level))) - 2L)))
  
  # Reference levels for factor predictors
  ref <- tryCatch(refLevel(x), error = function(e) character(0L))
  
  # Overall p-values via drop1
  anova_p <- tryCatch(
    drop1(x, test = "Chisq")[names(ref), "Pr(>Chi)"],
    error = function(e) setNamesX(rep(NA_real_, length(ref)), names(ref))
  )
  
  # --- build output matrix ---
  if (output == "hr") {
    est <- ci_mat[, "exp(coef)", drop = FALSE]
    lci <- ci_mat[, 3L, drop = FALSE]
    uci <- ci_mat[, 4L, drop = FALSE]
    if (is.null(names(est)))
      names(est) <- names(lci) <- names(uci) <- rownames(ci_mat)
    est_label <- "HR"
  } else {
    est <- coefs[, "coef"]
    lci <- est - z_alpha * coefs[, "se(coef)"]
    uci <- est + z_alpha * coefs[, "se(coef)"]
    est_label <- "log HR"
  }
  
  out <- cbind(
    fm(cbind(est, lci, uci), digits = digits),
    fm(coefs[, "Pr(>|z|)"], fmt = "p",
       eps = 10^-pdigits, digits = pdigits),
    fm(coefs[, "Pr(>|z|)"], fmt = "*")
  )
  colnames(out) <- c(est_label, ci_label, "p-val", "")
  
  # Insert variable-level summary rows and indent coefficient rows
  for (i in seq_along(ref)) {
    pat <- sprintf("^%s", gsub("[^a-zA-Z0-9_]", " ", names(ref)[i]))
    rnr <- grep(pat, gsub("[^a-zA-Z0-9_]", " ", rownames(out)))[1L]
    if (is.na(rnr)) next
    
    p <- anova_p[i]
    summary_row <- c(
      rep(".", 3L),
      fm(p, fmt = "p", eps = 10^-pdigits, digits = pdigits),
      fm(p, fmt = "*")
    )
    out <- appendX(out, rbind(summary_row), after = rnr - 1L, rows = TRUE)
    rownames(out)[rnr] <- sprintf("%s (ref: %s)", names(ref)[i], ref[i])
    dummy_rows <- grep(sprintf("^%s", names(ref)[i]), rownames(out))
    rownames(out)[dummy_rows] <- sub(
      names(ref)[i],
      paste0(names(ref)[i], " "),
      rownames(out)[dummy_rows]
    )
  }
  
  # --- header ---
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n", sep = "")
  
  cat(sprintf("\n%s:\n",
              if (output == "hr") "Hazard Ratios" else "Log Hazard Ratios"))
  print(out, quote = FALSE, right = TRUE, print.gap = 2L)
  cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  
  # --- footer ---
  n_na <- if (is.null(x$na.action)) 0L else length(x$na.action)
  cat(sprintf("\nObs (NAs): %d (%d)   Events: %d",
              x$n, n_na, x$nevent))
  cat(sprintf("   Concordance: %s",
              fm(xx$concordance["C"], digits = digits)))
  cat(sprintf("\nLog-lik: %s", fm(x$loglik[2L], digits = digits)))
  cat(sprintf("   LR \u03c7\u00b2(%d): %s   p: %s",
              as.integer(xx$logtest["df"]),
              fm(xx$logtest["test"], digits = digits),
              fm(xx$logtest["pvalue"], fmt = "p",
                 eps = 10^-pdigits, digits = pdigits)))
  cat("\n\n")
  
  invisible(xx)
}




#' @keywords internal
.print_survreg <- function(x, digits = 3, pdigits = 3,
                           conf.level = 0.95,
                           output = c("tr", "coef"), ...) {
  
  output  <- match.arg(output)
  xx      <- summary(x)
  coefs   <- xx$table
  
  # Remove Scale/Log(scale) row - reported separately in footer
  scale_rows <- grep("^Log\\(scale\\)|^Scale", rownames(coefs))
  if (length(scale_rows) > 0L)
    coefs <- coefs[-scale_rows, , drop = FALSE]
  
  alpha   <- 1 - (1 - conf.level) / 2
  z_alpha <- qnorm(alpha)
  
  ci_label <- sprintf(c("%s-lci", "uci"),
                      fm(conf.level, fmt = "%",
                         digits = max(0L, nDec(as.character(signif(conf.level))) - 2L)))
  
  # Reference levels for factor predictors
  ref <- tryCatch(refLevel(x), error = function(e) character(0L))
  
  # Overall p-values via drop1
  anova_p <- tryCatch(
    drop1(x, test = "Chisq")[names(ref), "Pr(>Chi)"],
    error = function(e) setNamesX(rep(NA_real_, length(ref)), names(ref))
  )
  
  # --- build output matrix ---
  est <- coefs[, "Value"]
  se  <- coefs[, "Std. Error"]
  
  if (output == "tr") {
    # Time Ratios = exp(coef) with CI via delta method
    est_out <- exp(est)
    lci     <- exp(est - z_alpha * se)
    uci     <- exp(est + z_alpha * se)
    est_label <- "TR"
  } else {
    # Log time ratios (raw AFT coefficients)
    est_out <- est
    lci     <- est - z_alpha * se
    uci     <- est + z_alpha * se
    est_label <- "log TR"
  }
  
  out <- cbind(
    fm(cbind(est_out, lci, uci), digits = digits),
    fm(coefs[, "p"],  fmt = "p",
       eps = 10^-pdigits, digits = pdigits),
    fm(coefs[, "p"], fmt = "*")
  )
  colnames(out) <- c(est_label, ci_label, "p-val", "")
  
  # Insert variable-level summary rows and indent coefficient rows
  for (i in seq_along(ref)) {
    pat <- sprintf("^%s", gsub("[^a-zA-Z0-9_]", " ", names(ref)[i]))
    rnr <- grep(pat, gsub("[^a-zA-Z0-9_]", " ", rownames(out)))[1L]
    if (is.na(rnr)) next
    
    p <- anova_p[i]
    summary_row <- c(
      rep(".", 3L),
      fm(p, fmt = "p", eps = 10^-pdigits, digits = pdigits),
      fm(p, fmt = "*")
    )
    out <- appendX(out, rbind(summary_row), after = rnr - 1L, rows = TRUE)
    rownames(out)[rnr] <- sprintf("%s (ref: %s)", names(ref)[i], ref[i])
    dummy_rows <- grep(sprintf("^%s", names(ref)[i]), rownames(out))
    rownames(out)[dummy_rows] <- sub(
      names(ref)[i],
      paste0(names(ref)[i], " "),
      rownames(out)[dummy_rows]
    )
  }
  
  # --- header ---
  dist_label <- switch(x$dist,
                       weibull     = "Weibull",
                       exponential = "Exponential",
                       lognormal   = "Log-normal",
                       loglogistic = "Log-logistic",
                       x$dist
  )
  cat(sprintf("\n%s AFT model\n", dist_label))
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n", sep = "")
  
  cat(sprintf("\nTime Ratios:\n") )
  print(out, quote = FALSE, right = TRUE, print.gap = 2L)
  cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  
  # --- scale and shape ---
  scale <- x$scale
  shape <- 1 / scale
  cat(sprintf("\nScale: %s   Shape (1/scale): %s",
              fm(scale, digits = digits),
              fm(shape, digits = digits)))
  
  # Shape interpretation for Weibull
  if (x$dist == "weibull") {
    interp <- if (shape > 1) "increasing hazard"
    else if (shape < 1) "decreasing hazard"
    else "constant hazard (exponential)"
    cat(sprintf("  \u2192 %s", interp))
  }
  
  # --- footer ---
  n_na <- if (is.null(x$na.action)) 0L else length(x$na.action)
  cat(sprintf("\n\nObs (NAs): %d (%d)   Events: %d",
              x$df.residual + x$df, n_na,
              sum(x$y[, ncol(x$y)])))
  
  cat(sprintf("\nLog-lik: %s", fm(x$loglik[2L], digits = digits)))
  
  # LR test vs null model
  lr_stat <- 2 * diff(x$loglik)
  lr_df   <- x$df - 1L
  lr_p    <- pchisq(lr_stat, df = lr_df, lower.tail = FALSE)
  cat(sprintf("   LR \u03c7\u00b2(%d): %s   p: %s",
              lr_df,
              fm(lr_stat, digits = digits),
              fm(lr_p, fmt = "p", eps = 10^-pdigits, digits = pdigits)))
  cat("\n\n")
  
  invisible(xx)
}




#' @keywords internal
.print_mixed <- function(x, digits = 3, pdigits = 3,
                         conf.level = 0.95,
                         output = c("coef", "or", "irr"), ...) {
  
  output <- match.arg(output)
  
  # Unwrap FitMod.lme4 wrapper
  obj   <- if (inherits(x, "FitMod.lme4")) x$model else x
  fitfn <- x$fitfn
  isGLMM <- inherits(obj, "glmerMod")
  
  # --- fixed effects ---
  
  sm    <- summary(obj)
  coef_table <- coef(sm)
  fe    <- coef_table[, "Estimate"]
  se    <- coef_table[, "Std. Error"]
  z     <- coef_table[, if ("z value" %in% colnames(coef_table)) "z value" else "t value"]
  pval  <- if ("Pr(>|z|)" %in% colnames(coef_table))
    coef_table[, "Pr(>|z|)"]
  else
    2 * pnorm(-abs(z))  # lmer has no p-values by default
  
  z_alpha <- qnorm(1 - (1 - conf.level) / 2)
  ci      <- cbind(fe - z_alpha * se, fe + z_alpha * se)
  
  ci_label <- sprintf(c("%s-lci", "uci"),
                      fm(conf.level, fmt = "%",
                         digits = max(0L, nDec(as.character(signif(conf.level))) - 2L)))
  
  # Transform to OR/IRR if requested
  est    <- fe
  ci_out <- ci
  if (output %in% c("or", "irr")) {
    est    <- exp(fe)
    ci_out <- exp(ci)
  }
  
  est_label <- switch(output,
                      coef = "estimate",
                      or   = "OR",
                      irr  = "IRR"
  )
  
  out <- cbind(
    fm(cbind(est, ci_out), digits = digits),
    fm(pval, fmt = "p", eps = 10^-pdigits, digits = pdigits),
    fm(pval, fmt = "*")
  )
  colnames(out) <- c(est_label, ci_label, "p-val", "")
  
  # Reference levels for factor predictors
  ref <- tryCatch(refLevel(obj), error = function(e) character(0L))
  
  # Overall p-values via drop1
  anova_p <- tryCatch(
    drop1(obj, test = "Chisq")[names(ref), "Pr(>Chi)"],
    error = function(e) setNamesX(rep(NA_real_, length(ref)), names(ref))
  )
  
  # Insert variable-level summary rows and indent coefficient rows
  for (i in seq_along(ref)) {
    pat <- sprintf("^%s", gsub("[^a-zA-Z0-9_]", " ", names(ref)[i]))
    rnr <- grep(pat, gsub("[^a-zA-Z0-9_]", " ", rownames(out)))[1L]
    if (is.na(rnr)) next
    
    p <- anova_p[i]
    summary_row <- c(
      rep(".", 3L),
      fm(p, fmt = "p", eps = 10^-pdigits, digits = pdigits),
      fm(p, fmt = "*")
    )
    out <- appendX(out, rbind(summary_row), after = rnr - 1L, rows = TRUE)
    rownames(out)[rnr] <- sprintf("%s (ref: %s)", names(ref)[i], ref[i])
    dummy_rows <- grep(sprintf("^%s", names(ref)[i]), rownames(out))
    rownames(out)[dummy_rows] <- sub(
      names(ref)[i],
      paste0(names(ref)[i], " "),
      rownames(out)[dummy_rows]
    )
  }
  
  # --- random effects ---
  vc    <- lme4::VarCorr(obj)
  re_df <- as.data.frame(vc)
  
  # ICC: ratio of between-group variance to total variance
  icc <- tryCatch({
    var_re  <- sum(re_df$vcov[re_df$grp != "Residual"])
    var_res <- if ("Residual" %in% re_df$grp) {
      re_df$vcov[re_df$grp == "Residual"]
    } else {
      # For GLMMs use distribution-specific residual variance
      switch(family(obj)$family,
             binomial = pi^2 / 3,
             poisson  = log(1 + 1 / exp(fe["(Intercept)"])),
             NA_real_
      )
    }
    var_re / (var_re + var_res)
  }, error = function(e) NA_real_)
  
  # --- header ---
  model_label <- switch(fitfn,
                        lmMixed      = "Linear mixed model",
                        logitMixed   = "Mixed logistic regression",
                        poissonMixed = "Mixed Poisson regression",
                        negbinMixed  = "Mixed negative binomial regression",
                        gammaMixed   = "Mixed Gamma regression",
                        "Mixed model"
  )
  cat(sprintf("\n%s\n", model_label))
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n", sep = "")
    
  # Fixed effects block
  coef_header <- switch(output,
                        coef = "\nFixed effects:\n",
                        or   = "\nFixed effects (Odds Ratios):\n",
                        irr  = "\nFixed effects (Incidence Rate Ratios):\n"
  )
  cat(coef_header)
  print(out, quote = FALSE, right = TRUE, print.gap = 2L)
  cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  
  # Random effects block
  cat("\nRandom effects:\n")
  re_out <- data.frame(
    Groups  = re_df$grp,
    Variance = fm(re_df$vcov,  digits = digits),
    Std.Dev  = fm(re_df$sdcor, digits = digits)
  )
  print(re_out, row.names = FALSE, right = FALSE)
  
  # ICC
  if (!is.na(icc))
    cat(sprintf("\nICC: %s\n", fm(icc, digits = digits)))
  
  # --- footer ---
  n_obs  <- nrow(obj@frame)
  n_grps <- sapply(lme4::ranef(obj), nrow)
  grp_str <- paste(sprintf("%s: %d", names(n_grps), n_grps),
                   collapse = ", ")
  
  cat(sprintf("\nObs: %d   Groups: %s", n_obs, grp_str))
  cat(sprintf("\nLog-lik: %s   AIC: %s",
              fm(as.numeric(logLik(obj)), digits = digits),
              fm(AIC(obj), digits = digits)))
  cat("\n\n")
  
  invisible(x)
}



#' @keywords internal
.print_ml <- function(x, digits = 3, pdigits = 3, ...) {
  
  fitfn <- x$fitfn
  
  # Unwrap FitMod wrapper
  obj <- x
  class(obj) <- class(obj)[class(obj) != "FitMod"]
  if (inherits(obj, "FitMod.xgboost")) obj <- obj$model
  
  # --- header ---
  model_label <- switch(fitfn,
                        randomForest = "Random Forest",
                        nnet         = "Neural Network",
                        rpart        = "Decision Tree",
                        C5.0         = "C5.0 Decision Tree",
                        svm          = "Support Vector Machine",
                        naiveBayes   = "Naive Bayes",
                        lda          = "Linear Discriminant Analysis",
                        qda          = "Quadratic Discriminant Analysis",
                        glmnet       = "Regularised Regression (glmnet)",
                        xgboost      = "XGBoost",
                        fitfn
  )
  
  cat(sprintf("\n%s\n", model_label))
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n", sep = "")
  
  # --- variable importance ---
  vi <- tryCatch(varImp(x, scale = "max"), error = function(e) NULL)
  if (!is.null(vi)) {
    cat("\nVariable importance:\n")
    vi_out <- setNamesX(
      fm(vi$importance, digits = digits),
      vi$variable
    )
    print(vi_out, quote = FALSE)
  }
  
  # --- confusion matrix ---
  pred <- tryCatch(
    predict(x, output = "class")$class,
    error = function(e) NULL
  )
  resp <- tryCatch(
    response(x),
    error = function(e) NULL
  )
  
  if (!is.null(pred) && !is.null(resp)) {
    cat("\nConfusion matrix (training):\n")
    cm <- conf(pred, resp)
    print(cm$table)
    cat(sprintf("\nAccuracy: %s   Kappa: %s\n",
                fm(cm$acc,   digits = digits),
                fm(cm$kappa, digits = digits)))
    
    # Brier score and c-statistic for binary
    prob <- tryCatch(predict(x, output = "prob"), error = function(e) NULL)
    if (!is.null(prob)) {
      bs <- tryCatch(
        brierScore(prob[, 2L], as.numeric(resp) - 1L),
        error = function(e) NULL
      )
      cs <- tryCatch(
        assocsXY(prob[, 2L], as.numeric(resp) - 1L,
                 which = "cstat")$cstat,
        error = function(e) NULL
      )
      if (!is.null(bs) || !is.null(cs)) {
        if (!is.null(bs))
          cat(sprintf("Brier score: %s", fm(bs, digits = digits)))
        if (!is.null(cs))
          cat(sprintf("   c-statistic: %s", fm(cs, digits = digits)))
        cat("\n")
      }
    }
  }
  
  # --- footer ---
  n_obs <- tryCatch(nrow(model.frame(obj)), error = function(e) NA_integer_)
  if (!is.na(n_obs))
    cat(sprintf("\nObs: %d\n\n", n_obs))
  
  invisible(x)
}


