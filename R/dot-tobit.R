#' @keywords internal
.print_tobit <- function(x, digits = 3, pdigits = 3,
                         conf.level = 0.95, ...) {
  
  xx    <- summary(x)
  coefs <- xx$coefficients
  
  # Remove Log(scale) row — technical parameter, not a predictor
  coefs <- coefs[rownames(coefs) != "Log(scale)", , drop = FALSE]
  
  # Wald CIs from SE
  alpha   <- 1 - (1 - conf.level) / 2
  z_alpha <- qnorm(alpha)
  est     <- coefs[, "Estimate"]
  se      <- coefs[, "Std. Error"]
  ci      <- cbind(est - z_alpha * se, est + z_alpha * se)
  
  # Reference levels for factor predictors
  ref <- tryCatch(refLevel(x), error = function(e) character(0L))
  
  # Overall p-values via drop1 (Wald test)
  anova_p <- tryCatch(
    drop1(x, test = "Chisq")[names(ref), "Pr(>Chi)"],
    error = function(e) setNames(rep(NA_real_, length(ref)), names(ref))
  )
  
  # Build output matrix
  ci_label <- sprintf(c("%s-lci", "uci"),
                      fm(conf.level, fmt = "%",
                         digits = max(0L, nDec(as.character(signif(conf.level))) - 2L)))
  
  out <- cbind(
    fm(cbind(est, ci), digits = digits),
    fm(coefs[, "Pr(>|z|)"], fmt = "p",
       eps = 10^-pdigits, digits = pdigits),
    fm(coefs[, "Pr(>|z|)"], fmt = "*")
  )
  colnames(out) <- c("estimate", ci_label, "p-val", "")
  
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
      paste(deparse(xx$call), sep = "\n", collapse = "\n"),
      "\n", sep = "")
  cat("\nCoefficients:\n")
  print(out, quote = FALSE, right = TRUE, print.gap = 2L)
  cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  
  # --- scale ---
  cat(sprintf("\nScale: %s\n", fm(xx$scale, digits = digits)))
  
  # --- footer ---
  n       <- xx$n
  n_left  <- n["Left-censored"]
  n_right <- n["Right-censored"]
  n_unc   <- n["Uncensored"]
  n_total <- n["Total"]
  
  cat(sprintf("\nObs: %d", n_total))
  if (n_left > 0L)
    cat(sprintf("  left-censored: %d", n_left))
  if (n_right > 0L)
    cat(sprintf("  right-censored: %d", n_right))
  cat(sprintf("  uncensored: %d", n_unc))
  
  cat(sprintf("\nLog-lik: %s", fm(xx$loglik[2L], digits = digits)))
  cat(sprintf("   AIC: %s", fm(AIC(x), digits = digits)))
  cat("\n\n")
  
  invisible(xx)
}


