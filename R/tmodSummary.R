
#' Extract model summaries for model comparison
#'
#' @description
#' S3 generic used internally by \code{\link{tMod}} to extract coefficients
#' and model statistics from fitted models in a standardized format.
#'
#' @details
#' Each method returns a list with components \code{coef} (a data frame of
#' coefficients and confidence intervals) and \code{statsx} (a named numeric
#' vector of model statistics). These are combined by \code{tMod()} to enable
#' comparison across different model types.
#'
#' @name tModSummary
#' 
#' @param x A fitted model object
#' @param ... Additional arguments passed to methods
#'
#' @return
#' A list with components:
#' \itemize{
#'   \item \code{coef}: data frame with columns \code{name, est, se, stat, p, lci, uci}
#'   \item \code{statsx}: named numeric vector of model statistics
#' }
#'
#' @seealso \code{\link{tMod}}
#'


#' @param conf.level Confidence level for intervals
#' @param useProfile Logical; use profile likelihood for CI (glm only)

#' @family model.comparison  
#' @concept model-evaluation  
#' @concept regression
#'
#'
#' @export
tModSummary <- function(x, ...){
  UseMethod("tModSummary")
}

#' @export
#' @method tModSummary default
#' @keywords internal
tModSummary.default <- function(x, ...){
  stop("No tModSummary method for class: ", class(x)[1])
}


#' @export
#' @method tModSummary lm
#' @rdname tModSummary
tModSummary.lm <- function(x, conf.level = 0.95, ...){
  
  sm <- summary(x)
  
  cf <- cbind(
    sm$coefficients,
    confint(x, level = conf.level)
  )
  
  coef <- data.frame(
    name = rownames(cf),
    est  = cf[,1],
    se   = cf[,2],
    stat = cf[,3],
    p    = cf[,4],
    lci  = cf[,5],
    uci  = cf[,6],
    row.names = NULL
  )
  
  fit <- fitted(x)
  y   <- model.response(model.frame(x))
  
  fstats <- sm$fstatistic
  if(!is.null(fstats)){
    Fval  <- fstats[1]
    numdf <- fstats[2]
    dendf <- fstats[3]
    pF    <- pf(Fval, numdf, dendf, lower.tail = FALSE)
  } else {
    Fval <- numdf <- dendf <- pF <- NA
  }
  
  statsx <- c(
    sigma         = sm$sigma,
    r.squared     = sm$r.squared,
    adj.r.squared = sm$adj.r.squared,
    "n vars"      = length(attr(x$terms, "term.labels")),
    "n coef"      = nrow(sm$coefficients),
    
    F     = Fval,
    numdf = numdf,
    dendf = dendf,
    p     = pF,
    
    N     = nobs(x),
    NAs   = ifelse(is.null(x$na.action), 0, length(x$na.action)),
    logLik   = as.numeric(logLik(x)),
    deviance = deviance(x),
    AIC      = AIC(x),
    BIC      = BIC(x),
    
    MAE  = DescToolsX::mae(y, fit),
    MAPE = DescToolsX::mape(y, fit),
    MSE  = DescToolsX::mse(y, fit),
    RMSE = DescToolsX::rmse(y, fit)
  )
  
  list(
    coef   = coef,
    statsx = statsx
  )
}


#' @export
#' @method tModSummary lmrob
#' @rdname tModSummary
tModSummary.lmrob <- function(x, conf.level = 0.95, ...){
  
  sm <- summary(x)
  
  cf <- sm$coefficients
  
  ci <- confint(x, level = conf.level)
  
  if(is.null(dim(ci)))
    ci <- matrix(ci, nrow = 1)
  
  coef <- data.frame(
    name = rownames(cf),
    est  = cf[,1],
    se   = cf[,2],
    stat = cf[,3],
    p    = cf[,4],
    lci  = ci[,1],
    uci  = ci[,2],
    row.names = NULL
  )
  
  fit <- fitted(x)
  y   <- model.response(model.frame(x))
  
  # F-statistics (lmrob uses different structure!)
  df <- sm$df
  if(!is.null(df) && length(df) >= 3){
    Fval  <- df[1]
    numdf <- df[3]
    dendf <- df[2]
    pF    <- pf(Fval, dendf, numdf, lower.tail = FALSE)
  } else {
    Fval <- numdf <- dendf <- pF <- NA
  }
  
  statsx <- c(
    sigma         = sm$sigma,
    r.squared     = sm$r.squared,
    adj.r.squared = sm$adj.r.squared,
    
    F     = Fval,
    numdf = numdf,
    dendf = dendf,
    p     = pF,
    
    N     = nobs(x),
    NAs   = ifelse(is.null(x$na.action), 0, length(x$na.action)),
    
    # explicitly NA (as in DescTools)
    logLik   = NA,
    deviance = NA,
    AIC      = NA,
    BIC      = NA,
    
    "n vars" = length(attr(x$terms, "term.labels")),
    "n coef" = nrow(cf),
    
    MAE  = DescToolsX::mae(y, fit),
    MAPE = DescToolsX::mape(y, fit),
    MSE  = DescToolsX::mse(y, fit),
    RMSE = DescToolsX::rmse(y, fit)
  )
  
  list(
    coef   = coef,
    statsx = statsx
  )
}



#' @export
#' @method tModSummary glm
#' @rdname tModSummary
tModSummary.glm <- function(x, conf.level = 0.95, useProfile = TRUE, ...){
  
  sm <- summary(x)
  
  # --- CI ---
  ci <- if(useProfile) {
    suppressMessages(confint(x, level = conf.level))
  } else {
    confint.default(x, level = conf.level)
  }
  
  if(is.null(dim(ci)))
    ci <- matrix(ci, nrow = 1)
  
  cf <- sm$coefficients
  
  coef <- data.frame(
    name = rownames(cf),
    est  = cf[,1],
    se   = cf[,2],
    stat = cf[,3],
    p    = cf[,4],
    lci  = ci[,1],
    uci  = ci[,2],
    row.names = NULL
  )
  
  pred <- fitted(x)
  y    <- model.response(model.frame(x))
  
  # --- N ---
  N <- if(!is.null(weights(x))) {
    sum(weights(x), na.rm = TRUE)
  } else {
    nobs(x)
  }
  
  # --- Likelihood / deviance ---
  degf <- sm$df.null - sm$df.residual
  LR   <- sm$null.deviance - sm$deviance
  pLR  <- pchisq(LR, degf, lower.tail = FALSE)
  
  # ============================
  # BINOMIAL CASE
  # ============================
  if(x$family$family == "binomial"){
    
    statsx <- pseudoRSq(x, which = "all")
    
    # Associations - one pass over the concordant/discordant pairs yields
    # all ordinal measures; [1] takes each point estimate. Keys are the
    # ordAssocs() list names (tauA, somers, gamma, cstat).
    statsy <- sapply(ordAssocs(pred, y), "[", 1)
    
    statsx <- c(
      statsx,
      
      N     = nobs(x),
      NAs   = ifelse(is.null(x$na.action), 0, length(x$na.action)),
      
      "n vars" = length(attr(x$terms, "term.labels")),
      "n coef" = length(x$coefficients),
      
      numdf = attr(logLik(x), "df"),
      
      "Kendall Tau-a" = unname(statsy["tauA"]),
      "Somers Delta"  = unname(statsy["somers"]),
      "Gamma"         = unname(statsy["gamma"]),
      
      "Brier"         = brierScore(x),
      "C"             = unname(statsy["cstat"])
    )
    
  } else {
    
    # ============================
    # GENERAL GLM
    # ============================
    
    statsx <- pseudoRSq(
      x,
      which = c("McFadden","McFaddenAdj","Nagelkerke","CoxSnell",
                "AIC","BIC","logLik","logLik0","G2")
    )
    
    statsx <- c(
      statsx,
      
      N     = nobs(x),
      NAs   = ifelse(is.null(x$na.action), 0, length(x$na.action)),
      
      "n vars" = length(attr(x$terms, "term.labels")),
      "n coef" = length(x$coefficients),
      
      numdf = attr(logLik(x), "df"),
      
      MAE  = DescToolsX::mae(y, pred),
      MAPE = DescToolsX::mape(y, pred),
      MSE  = DescToolsX::mse(y, pred),
      RMSE = DescToolsX::rmse(y, pred)
    )
  }
  
  list(
    coef   = coef,
    statsx = statsx
  )
}


#' @export
#' @method tModSummary betareg
#' @rdname tModSummary
tModSummary.betareg <- function(x, conf.level = 0.95, ...){
  
  # coefficients of all model parts in one table, the rows of the
  # precision model prefixed with "(phi)_" as in coef()
  cf <- .waldTable.betareg(x)
  
  z_alpha <- qnorm(1 - (1 - conf.level) / 2)
  
  coef <- data.frame(
    name = rownames(cf),
    est  = cf[,1],
    se   = cf[,2],
    stat = cf[,3],
    p    = cf[,4],
    lci  = cf[,1] - z_alpha * cf[,2],
    uci  = cf[,1] + z_alpha * cf[,2],
    row.names = NULL
  )
  
  mf   <- model.frame(x)
  pred <- fitted(x)
  y    <- model.response(mf)
  
  statsx <- c(
    # CoxSnell, Efron, Ferrari and the likelihood based statistics;
    # McFadden et al. are not defined for a continuous response
    pseudoRSq(x, which = "all"),
    
    N     = nobs(x),
    # betareg keeps the omitted observations with the model frame only
    NAs   = length(attr(mf, "na.action")),
    
    # regressors of the mean and of the precision model together
    "n vars" = length(attr(terms(x, model = "full"), "term.labels")),
    "n coef" = nrow(cf),
    
    numdf = attr(logLik(x), "df"),
    
    MAE  = DescToolsX::mae(y, pred),
    MAPE = DescToolsX::mape(y, pred),
    MSE  = DescToolsX::mse(y, pred),
    RMSE = DescToolsX::rmse(y, pred)
  )
  
  list(
    coef   = coef,
    statsx = statsx
  )
}


#' @export
#' @method tModSummary coxph
#' @rdname tModSummary
tModSummary.coxph <- function(x, conf.level = 0.95, ...){
  
  sm <- summary(x)
  
  cf <- sm$coefficients
  
  # HR + CI
  est <- exp(cf[,1])
  se  <- cf[,3]
  stat <- cf[,4]
  p   <- cf[,5]
  
  ci <- exp(confint(x, level = conf.level))
  if(is.null(dim(ci)))
    ci <- matrix(ci, nrow = 1)
  
  coef <- data.frame(
    name = rownames(cf),
    est  = est,
    se   = se,
    stat = stat,
    p    = p,
    lci  = ci[,1],
    uci  = ci[,2],
    row.names = NULL
  )
  
  statsx <- c(
    logLik = as.numeric(logLik(x)),
    AIC    = AIC(x),
    BIC    = BIC(x),
    
    "Concordance" = sm$concordance[1],
    
    N   = sm$n,
    NAs = ifelse(is.null(x$na.action), 0, length(x$na.action)),
    
    "n vars" = length(attr(x$terms, "term.labels")),
    "n coef" = length(coef(x))
  )
  
  list(
    coef   = coef,
    statsx = statsx
  )
}


#' @export
#' @method tModSummary gam
#' @rdname tModSummary
tModSummary.gam <- function(x, conf.level = 0.95, ...){
  
  sm <- summary(x)
  
  # --- parametric ---
  cf <- sm$p.table
  
  ci <- confint(x, level = conf.level)
  if(is.null(dim(ci)))
    ci <- matrix(ci, nrow = 1)
  
  coef_param <- data.frame(
    name = rownames(cf),
    est  = cf[,1],
    se   = cf[,2],
    stat = cf[,3],
    p    = cf[,4],
    lci  = ci[,1],
    uci  = ci[,2],
    row.names = NULL
  )
  
  # --- smooth terms ---
  if(!is.null(sm$s.table)){
    st <- sm$s.table
    
    coef_smooth <- data.frame(
      name = rownames(st),
      est  = st[,1],   # edf
      se   = NA,
      stat = st[,3],
      p    = st[,4],
      lci  = NA,
      uci  = NA,
      row.names = NULL
    )
    
    coef <- rbind(coef_param, coef_smooth)
  } else {
    coef <- coef_param
  }
  
  fit <- fitted(x)
  y   <- model.response(model.frame(x))
  
  statsx <- c(
    r.squared     = sm$r.sq,
    adj.r.squared = sm$adj.r.sq,
    
    logLik = as.numeric(logLik(x)),
    AIC    = AIC(x),
    BIC    = BIC(x),
    
    N   = nobs(x),
    NAs = ifelse(is.null(x$na.action), 0, length(x$na.action)),
    
    "n vars" = length(attr(x$terms, "term.labels")),
    "n coef" = length(coef(x)),
    
    MAE  = DescToolsX::mae(y, fit),
    MAPE = DescToolsX::mape(y, fit),
    MSE  = DescToolsX::mse(y, fit),
    RMSE = DescToolsX::rmse(y, fit)
  )
  
  list(
    coef   = coef,
    statsx = statsx
  )
}


#' @export
#' @method tModSummary lmer
#' @rdname tModSummary
tModSummary.lmer <- function(x, conf.level = 0.95, ...){
  
  sm <- summary(x)
  
  cf <- sm$coefficients
  
  ci <- suppressMessages(confint(x, level = conf.level, method = "Wald"))
  ci <- ci[rownames(cf), , drop = FALSE]
  
  coef <- data.frame(
    name = rownames(cf),
    est  = cf[,1],
    se   = cf[,2],
    stat = cf[,3],
    p    = 2 * (1 - pnorm(abs(cf[,3]))),  # approx
    lci  = ci[,1],
    uci  = ci[,2],
    row.names = NULL
  )
  
  # R2 (optional, falls MuMIn vorhanden)
  if(requireNamespace("MuMIn", quietly = TRUE)){
    r2 <- MuMIn::r.squaredGLMM(x)
    r2m <- r2[1]
    r2c <- r2[2]
  } else {
    r2m <- r2c <- NA
  }
  
  statsx <- c(
    "R2 (marginal)"   = r2m,
    "R2 (conditional)"= r2c,
    
    logLik = as.numeric(logLik(x)),
    AIC    = AIC(x),
    BIC    = BIC(x),
    
    N   = nobs(x),
    NAs = ifelse(is.null(x$na.action), 0, length(x$na.action)),
    
    "n vars" = length(attr(x@frame, "terms")[[3]]),
    "n coef" = length(lme4::fixef(x))
  )
  
  list(
    coef   = coef,
    statsx = statsx
  )
}



