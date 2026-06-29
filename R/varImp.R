
#' Variable importance for machine learning models
#'
#' Extracts and normalises variable importance scores from fitted machine
#' learning models.  Returns a \code{data.frame} sorted by importance,
#' scaled to a 0--100 range.
#'
#' @param x A fitted model of class \code{"FitMod"}.  Supported fitting
#'   functions: \code{"rpart"}, \code{"C5.0"}, \code{"randomForest"},
#'   \code{"nnet"}, \code{"glmnet"}, \code{"xgboost"}.
#' @param scale Character string controlling the scaling of importance scores.
#'   One of \code{"max"} (default: best predictor = 100, others relative),
#'   \code{"sum"} (scores sum to 100, interpretable as percentage share), or
#'   \code{"none"} (raw scores as returned by the underlying method).
#' @param sort Logical.  If \code{TRUE} (default), rows are sorted in
#'   descending order of importance.
#' @param ... Further arguments passed to the underlying importance method.
#'
#' @return A \code{data.frame} of class \code{c("varImp", "data.frame")}
#'   with columns:
#'   \describe{
#'     \item{variable}{Character: predictor name.}
#'     \item{importance}{Numeric: importance score (0--100 if
#'       \code{scale = TRUE}).}
#'   }
#'
#' @examples
#' fitRf <- fitMod(ice_cream ~ video + puzzle + female,
#'                 IceCream, fitfn = "randomForest")
#' vi <- varImp(fitRf)
#' vi
#' plot(vi)
#'
#' @seealso \code{\link{plot.varImp}}

#' @family regression.utils  
#' @concept feature-selection  
#' @concept modelling
#'
#'
#' @export
varImp <- function(x, scale = c("max", "sum", "none"), sort = TRUE, ...) {
  
  scale <- match.arg(scale)
  
  if (!inherits(x, "FitMod"))
    stop("x must be a FitMod object")
  
  fitfn <- x$fitfn
  obj   <- if (inherits(x, "FitMod.xgboost")) x$model else {
    o <- x
    class(o) <- class(o)[class(o) != "FitMod"]
    o
  }
  
  imp <- switch(fitfn,
                
                rpart = {
                  vi <- obj$variable.importance
                  if (is.null(vi))
                    stop("No variable importance available - tree may be a stump")
                  data.frame(variable   = names(vi),
                             importance = as.numeric(vi),
                             row.names  = NULL,
                             stringsAsFactors = FALSE)
                },
                
                C5.0 = {
                  vi <- C50::C5imp(obj, metric = "usage")
                  data.frame(variable   = rownames(vi),
                             importance = vi[, 1L],
                             row.names  = NULL,
                             stringsAsFactors = FALSE)
                },
                
                randomForest = {
                  vi  <- randomForest::importance(obj)
                  col <- if ("MeanDecreaseGini" %in% colnames(vi)) "MeanDecreaseGini"
                  else if ("%IncMSE"     %in% colnames(vi)) "%IncMSE"
                  else colnames(vi)[1L]
                  data.frame(variable   = rownames(vi),
                             importance = vi[, col],
                             row.names  = NULL,
                             stringsAsFactors = FALSE)
                },
                
                nnet = {
                  if (!requireNamespace("NeuralNetTools", quietly = TRUE))
                    stop("Package 'NeuralNetTools' must be installed for nnet importance")
                  n_out <- length(obj$lev)
                  if (n_out > 2L) {
                    # Multinomial: average absolute importance across all output classes
                    vi_list <- lapply(seq_len(n_out), function(i) {
                      suppressWarnings(
                        NeuralNetTools::olden(obj, bar_plot = FALSE,
                                              out_var = obj$lev[i])
                      )[, 1L]
                    })
                    vi_mat <- do.call(cbind, vi_list)
                    vi     <- rowMeans(abs(vi_mat))
                    nms    <- rownames(suppressWarnings(
                      NeuralNetTools::olden(obj, bar_plot = FALSE, out_var = obj$lev[1L])
                    ))
                  } else {
                    vi_raw <- suppressWarnings(NeuralNetTools::olden(obj, bar_plot = FALSE))
                    vi     <- abs(vi_raw[, 1L])
                    nms    <- rownames(vi_raw)
                  }
                  data.frame(variable   = nms,
                             importance = vi,
                             row.names  = NULL,
                             stringsAsFactors = FALSE)
                },
                
                glmnet = {
                  co <- coef(obj, s = "lambda.1se")
                  if (is.list(co)) {
                    mat <- do.call(cbind, lapply(co, function(m) abs(as.numeric(m))))
                    vi  <- rowMeans(mat)
                    nms <- rownames(co[[1L]])
                  } else {
                    vi  <- abs(as.numeric(co))
                    nms <- rownames(co)
                  }
                  keep <- nms != "(Intercept)"
                  data.frame(variable   = nms[keep],
                             importance = vi[keep],
                             row.names  = NULL,
                             stringsAsFactors = FALSE)
                },
                
                xgboost = {
                  vi <- xgboost::xgb.importance(model = obj)
                  data.frame(variable   = vi$Feature,
                             importance = vi$Gain,
                             row.names  = NULL,
                             stringsAsFactors = FALSE)
                },
                
                stop(sprintf(
                  "varImp is not supported for fitfn = '%s'. Supported: rpart, C5.0, randomForest, nnet, glmnet, xgboost.",
                  fitfn
                ))
  )
  
  # Scale importance scores
  imp$importance <- switch(scale,
                           max  = 100 * imp$importance / max(imp$importance,  na.rm = TRUE),
                           sum  = 100 * imp$importance / sum(imp$importance,  na.rm = TRUE),
                           none = imp$importance
  )
  
  # Sort descending
  if (sort)
    imp <- imp[order(-imp$importance), ]
  
  rownames(imp) <- NULL
  structure(imp, class = c("varImp", "data.frame"))
}




#' Cleveland dot plot for variable importance
#'
#' Displays a Cleveland dot plot of variable importance scores as returned
#' by \code{\link{varImp}}.  Variables are ordered by importance
#' (most important at the top).
#'
#' @param x An object of class \code{"varImp"} as returned by
#'   \code{\link{varImp}}.
#' @param main Character string for the plot title.  Default is
#'   \code{"Variable Importance"}.
#' @param xlab Character string for the x-axis label.  Default is
#'   \code{"Importance"}.
#' @param pch Plotting character.  Default is \code{16} (filled circle).
#' @param col Colour of the points and reference lines.  Default is
#'   \code{"steelblue"}.
#' @param cex Numeric: point size.  Default is \code{1.2}.
#' @param ... Further arguments passed to \code{\link[graphics]{dotchart}}.
#'
#' @return Invisibly returns \code{x}.
#'
#' @examples
#' fitRf <- fitMod(ice_cream ~ video + puzzle + female,
#'                 IceCream, fitfn = "randomForest")
#' plot(varImp(fitRf))
#'
#' @seealso \code{\link{varImp}}
#' @export
plot.varImp <- function(x, main = "Variable Importance",
                        xlab = "Importance",
                        pch  = 16,
                        col  = "steelblue",
                        cex  = 1.2,
                        ...) {
  # Order: least important at top of dotchart (dotchart plots bottom-to-top)
  ord <- order(x$importance)
  plotDot(x$importance[ord],
           labels = x$variable[ord],
           main   = main,
           xlab   = xlab,
           pch    = pch,
           col  = col,
           cex    = cex,
           ...)
  invisible(x)
}

