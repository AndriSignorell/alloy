

#' @keywords internal
.summary_multinom <- function(x, conf.level = 0.95) {
  
  alpha     <- 1 - (1 - conf.level) / 2
  r.summary <- summary(x)
  
  # Build long-format coefficient table without reshape()
  coef_mat <- t(r.summary$coefficients)
  se_mat   <- t(r.summary$standard.errors)
  
  d.res <- do.call(rbind, lapply(colnames(coef_mat), function(cls) {
    est <- coef_mat[, cls]
    se  <- se_mat[, cls]
    z   <- abs(est / se)
    data.frame(
      time     = cls,
      id       = rownames(coef_mat),
      estimate = est,
      stderr   = se,
      z        = z,
      lci      = est - qnorm(alpha) * se,
      uci      = est + qnorm(alpha) * se,
      pval     = 2 * (1 - pnorm(z)),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  }))
  
  # Name CI column after conf.level
  lci_label <- sprintf("%s-lci", fm(conf.level, fmt = "%", digits = 0))
  names(d.res)[names(d.res) == "lci"] <- lci_label
  
  resp <- response(x)
  
  res <- structure(
    list(
      coefficients = d.res,
      call         = x$call,
      nobs         = nrow(x$fitted.values),
      na.action    = if (is.null(x$na.action)) 0L else length(x$na.action),
      PseudoR2     = pseudoR2(x, which = "all"),
      response     = c(attr(resp, "response"), levels(resp)[1L]),
      conf.level   = conf.level,
      results      = "Coefficients"
    ),
    class = c("SummaryFitMod", "list")
  )
  res
}


#' @keywords internal
.print_multinom <- function(x, digits = 3, pdigits = 3,
                                   conf.level = 0.95, ...) {
  
  xx <- if (inherits(x, "SummaryFitMod")) x
  else .summary_multinom(x, conf.level = conf.level)
  
  # Locate CI column name dynamically
  lci_col <- grep("-lci$", names(xx$coefficients), value = TRUE)
  
  out <- cbind(
    " "    = xx$coefficients$id,
    fm(xx$coefficients[, c("estimate", lci_col, "uci")], digits = digits),
    "pval" = fm(xx$coefficients$pval, fmt = "p",
                    digits = pdigits, eps = 10^-pdigits),
    " "    = fm(xx$coefficients$pval, fmt = "*")
  )
  
  # Insert class header rows
  g   <- which(!duplicated(xx$coefficients$time))
  out <- out[sort(c(seq(nrow(out)), g)), ]
  out[, 1L] <- paste0("  ", out[, 1L])
  
  ti          <- g + seq(0L, length(g) - 1L)
  out[ti, 1L] <- strAlign(unique(xx$coefficients$time), sep = "\\l")
  out[ti, -1L] <- ""
  
  # Print
  cat("\nCall:\n",
      paste(deparse(xx$call), sep = "\n", collapse = "\n"),
      "\n", sep = "")
  cat(sprintf("\n%s:\n", xx$results))
  cat(sprintf("(%s == %s is the base outcome)\n\n",
              xx$response[1L], xx$response[2L]))
  print(out, quote = FALSE, print.gap = 2L, row.names = FALSE)
  cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  cat(sprintf("\nObs (NAs): %d (%d)", xx$nobs, xx$na.action))
  cat("\tPseudo R\u00B2 (McFadden):",
      fm(xx$PseudoR2["McFadden"], digits = digits))
  cat("   AIC:", fm(xx$PseudoR2["AIC"], digits = digits))
  cat("\n\n")
  
  invisible(xx)
}



# not needed anymore, we have output = "or"
#' #' @export
#' OddsRatio.multinom <- function(x, conf.level = 0.95, digits = 3, ...) {
#'   xx <- .summary_multinom(x, conf.level = conf.level)
#'   
#'   lci_col <- grep("-lci$", names(xx$coefficients), value = TRUE)
#'   cols    <- c("estimate", lci_col, "uci")
#'   xx$coefficients[cols] <- exp(xx$coefficients[cols])
#'   xx$results <- "OddsRatio"
#'   
#'   xx
#' }
