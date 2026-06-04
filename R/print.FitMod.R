
#' @export
print.FitMod <- function(x, digits = 3, pdigits = 3,
                         conf.level = 0.95,
                         output = c("coef", "or", "irr", "hr", "lhr", "genuine"),
                         useProfile = FALSE,
                         vcov = NULL,
                         ...) {
  
  output <- match.arg(output)
  
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
      message("Note: vcov is not supported for multinom — standard SE used")
    .print_multinom(x, digits, pdigits, conf.level, ...)
  } else if (inherits(x, "polr")) {
    if (!is.null(vcov))
      message("Note: vcov is not supported for polr — standard SE used")
    .print_polr(x, digits, pdigits, conf.level, output = output, ...)
  } else if (inherits(x, "tobit")) {
    .print_tobit(x, digits, pdigits, conf.level, ...)
  } else if (inherits(x, "zeroinfl")) {
    .print_zeroinfl(x, digits, pdigits, conf.level, ...)
  } else if (inherits(x, "coxph")) {
    .print_coxph(x, digits, pdigits, conf.level, ...)
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
    setNames(pvals, names(ref))
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
  # xx$link refers to the zero-inflation part (logit) — count link depends on dist
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
    error = function(e) setNames(rep(NA_real_, length(ref)), names(ref))
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
