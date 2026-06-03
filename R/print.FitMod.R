
#' @export
print.FitMod <- function(x, digits = 3, pdigits = 3,
                         conf.level = 0.95,
                         output = c("coef", "or"),
                         useProfile = FALSE, ...) {
  output <- match.arg(output)
  
  if (inherits(x, "glm") || inherits(x, "lm") || inherits(x, "lmrob")) {
    .print_lm(x, digits, pdigits, conf.level, useProfile = useProfile, ...)
  } else if (inherits(x, "multinom")) {
    .print_multinom(x, digits, pdigits, conf.level, ...)
  } else if (inherits(x, "polr")) {
    .print_polr(x, digits, pdigits, conf.level, output = output, ...)
  } else {
    class(x) <- class(x)[class(x) != "FitMod"]
    print(x, ...)
  }
}



#' @keywords internal
.print_lm <- function(x, digits = 3, pdigits = 3,
                      conf.level = 0.95,
                      useProfile = FALSE, ...) {
  
  isGLM   <- inherits(x, "glm")
  isLMROB <- inherits(x, "lmrob")
  xx      <- summary(x)
  ci      <- if (useProfile && isGLM)
    confint(x, level = conf.level)
  else
    confint.default(x, level = conf.level)
  ref     <- refLevel(x)
  
  # --- overall p-values per predictor via drop1 ---
  anova_p <- if (isLMROB) {
    # lmrob has no drop1 support — use coefficient p-values directly
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
    else
      cat("\nCoefficients:\n")
    
    coefs <- xx$coefficients
    if (any(aliased <- xx$aliased)) {
      cn    <- names(aliased)
      coefs <- matrix(NA_real_, length(aliased), 4L,
                      dimnames = list(cn, colnames(coefs)))
      coefs[!aliased, ] <- xx$coefficients
    }
    
    ci_label <- sprintf(c("%s-lci", "uci"),
                        fm(conf.level, fmt = "%",
                           digits = max(0L, nDec(as.character(signif(conf.level))) - 2L)))
    out <- cbind(
      fm(cbind(coefs[, 1L], ci), digits = digits),
      gsub(" ", "", fm(coefs[, 4L], fmt = "p",
                       eps = 10^-pdigits, digits = pdigits)),
      fm(coefs[, 4L], fmt = "*")
    )
    colnames(out) <- c("estimate", ci_label, "p-val", "")
    
    for (i in seq_along(ref)) {
      pat <- sprintf("^%s", gsub("[^a-zA-Z0-9_]", " ", names(ref)[i]))
      rnr <- grep(pat, gsub("[^a-zA-Z0-9_]", " ", rownames(out)))[1L]
      if (is.na(rnr)) next
      
      p <- anova_p[i]
      summary_row <- c(
        rep(".", 3L),
        gsub(" ", "", fm(p, fmt = "p", eps = 10^-pdigits, digits = pdigits)),
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
  
  # --- footer ---
  n_na <- if (is.null(x$na.action)) 0L else length(x$na.action)
  cat(sprintf("\nObs (NAs): %d (%d)", nobs(x), n_na))
  
  if (isLMROB) {
    # lmrob: show R² and scale estimate
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

