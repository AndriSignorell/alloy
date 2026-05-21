
#' Split Data into Training and Test Sets
#'
#' Splits a vector, matrix or data frame into training and test subsets.
#'
#' @param x An object to split. Typically a vector, matrix or data frame.
#' @param p Proportion of observations assigned to the test set.
#'   Must be between 0 and 1. Default is `0.1`.
#' @param seed Optional integer used to initialize the random number generator
#'   for reproducible splits.
#' @param logical Logical; if `TRUE`, a logical index vector is returned instead
#'   of the split data objects. `TRUE` indicates observations belonging to the
#'   test set.
#'
#' @return
#' If `logical = FALSE`, a list with elements:
#' \describe{
#'   \item{train}{Training subset.}
#'   \item{test}{Test subset.}
#' }
#'
#' If `logical = TRUE`, a logical vector of length `n`.
#'
#' @details
#' Splitting data into training and test subsets is a common strategy for
#' evaluating predictive models. The training set is used to fit the model,
#' while the test set is used to assess predictive performance on unseen data.
#'
#' @examples
#' splitTrainTest(iris)
#'
#' d <- splitTrainTest(iris, p = 0.2, seed = 123)
#'
#' str(d$train)
#' str(d$test)
#'
#' idx <- splitTrainTest(iris, logical = TRUE)
#' table(idx)
#'


#' @export
splitTrainTest <- function(x, p = 0.1, seed = NULL, logical = FALSE) {
  
  if (!is.null(seed))
    set.seed(seed)
  
  if (!is.numeric(p) || length(p) != 1L || p <= 0 || p >= 1)
    stop("'p' must be a single number between 0 and 1.")
  
  n <- if (is.atomic(x)) length(x) else nrow(x)
  
  ntest <- floor(n * p)
  
  if (ntest == 0L)
    stop("Test set is empty. Increase 'p' or use a larger dataset.")
  
  idx <- seq_len(n)
  
  itest <- sample(
    idx,
    size = ntest,
    replace = FALSE
  )
  
  if (logical) {
    
    res <- unwhich(itest, n)
    
  } else {
    
    if (is.atomic(x)) {
      
      res <- list(
        train = x[-itest],
        test = x[itest]
      )
      
    } else {
      
      res <- list(
        train = x[-itest, , drop = FALSE],
        test = x[itest, , drop = FALSE]
      )
      
    }
    
  }
  
  res
  
}

