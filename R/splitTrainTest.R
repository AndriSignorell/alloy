
#' Split Data into Training, Validation and Test Sets
#'
#' Splits a vector, matrix or data frame into two or more random subsets, as
#' used for training, validation and testing predictive models. The split can be
#' stratified, so that the composition of a grouping variable is retained in
#' every subset.
#'
#' @param x an object to split, typically a vector, matrix or data frame
#' @param p the proportions of the subsets. A single number is the proportion
#'   assigned to the test set, the remainder going to the training set. A vector
#'   of length two or more gives the proportion of each subset in turn. The
#'   values need not sum to 1 and are rescaled if they do not, so ratios such as
#'   `c(6, 2, 2)` can be given directly. Names, where present, are used as subset
#'   names; otherwise `c("train", "test")` and `c("train", "validation", "test")`
#'   are supplied for length two and three.
#' @param strata an optional vector of the same length (or number of rows) as
#'   `x`, giving the strata to sample within. A data frame or list of several
#'   vectors is reduced to their interaction. Missing values form a stratum of
#'   their own.
#' @param output the value to return, either `"data"` (default) for the subsets
#'   themselves or `"group"` for a factor of subset memberships
#'
#' @return
#' For `output = "data"` a named list with one element per subset, in the order
#' given by `p`, each a subset of `x` of the same type.
#'
#' For `output = "group"` a factor of length `n` whose levels are the subset
#' names, giving the subset each observation was assigned to.
#'
#' @details
#' Splitting data into disjoint subsets is a common strategy for evaluating
#' predictive models. The training set is used to fit the model, the test set to
#' assess predictive performance on unseen data. A three-way split adds a
#' validation set for tuning and model selection, keeping the test set untouched
#' until the final assessment.
#'
#' Subset sizes are `n * p` rounded by the largest remainder method, so they sum
#' to `n` exactly and each subset is as close to its requested proportion as
#' integer sizes allow. A subset that would remain empty is an error.
#'
#' Stratified sampling draws within each stratum separately and so reproduces
#' the composition of `strata` in every subset. It is worth the effort whenever
#' a stratum is small enough that an unstratified draw could leave it thinly
#' represented, or absent, in one of the subsets: rare classes in a
#' classification problem, an unbalanced treatment variable, few observations
#' per site or period.
#'
#' Rounding within a stratum is not independent of the other strata. The
#' fractional parts of `n * p` repeat across strata, so rounding each stratum on
#' its own would hand the leftover observation to the same subset every time and
#' bias the smallest subset downwards. The remainders are therefore carried from
#' one stratum to the next, and the strata are visited in random order. A stratum
#' smaller than the number of subsets is not an error, it simply cannot
#' contribute to all of them.
#'
#' The assignment is drawn from R's global random number generator, whose state
#' the function does not modify. Call [set.seed()] before the function for
#' reproducible results.
#'
#' The subsets are disjoint and exhaustive, so `output = "group"` is a complete
#' description of the split. It can be stored alongside the data, tabulated, or
#' passed to [split()] to obtain the data subsets later.
#'
#' @examples
#' splitTrainTest(iris)
#'
#' set.seed(123)
#' d <- splitTrainTest(iris, p = 0.2)
#'
#' str(d$train)
#' str(d$test)
#'
#' # train, validation and test in a 6:2:2 ratio
#' d <- splitTrainTest(iris, p = c(6, 2, 2))
#' sapply(d, nrow)
#' ##      train validation       test
#' ##         90         30         30
#'
#' # the same, with the species composition retained in every subset
#' d <- splitTrainTest(iris, p = c(6, 2, 2), strata = iris$Species)
#' sapply(d, function(z) table(z$Species))
#' ##            train validation test
#' ## setosa        30         10   10
#' ## versicolor    30         10   10
#' ## virginica     30         10   10
#'
#' # the memberships instead of the data
#' grp <- splitTrainTest(iris, p = c(6, 2, 2), output = "group")
#' table(grp)
#'
#' # any number of subsets, named as you like
#' d <- splitTrainTest(1:100, p = c(fit = 0.5, calib = 0.25, holdout = 0.25))
#' lengths(d)
#' ##     fit   calib holdout
#' ##      50      25      25
#'
#' @family data.split
#' @concept modelling
#'
#' @export
splitTrainTest <- function(x, p = 0.1, strata = NULL,
                           output = c("data", "group")) {

  output <- match.arg(output)

  if (!is.numeric(p) || length(p) < 1L || !all(is.finite(p)) || any(p <= 0))
    stop("'p' must be a numeric vector of positive proportions.")

  if (length(p) == 1L) {
    if (p >= 1)
      stop("a single 'p' is the test proportion and must be below 1.")
    p <- c(train = 1 - p, test = p)
  }

  if (is.null(names(p))) {
    if (length(p) == 2L)
      names(p) <- c("train", "test")
    else if (length(p) == 3L)
      names(p) <- c("train", "validation", "test")
    else
      stop("'p' must be named for more than three subsets.")
  }

  if (anyNA(names(p)) || any(names(p) == "") || anyDuplicated(names(p)))
    stop("the names of 'p' must be non-empty and unique.")

  p <- p / sum(p)

  n <- if (is.null(dim(x))) length(x) else nrow(x)

  # the strata are visited in random order and the rounding remainders are
  # carried along, so that no subset is systematically favoured
  pool <- split(seq_len(n), .splitStrata(strata, n))
  pool <- pool[lengths(pool) > 0L]
  pool <- pool[sample.int(length(pool))]

  grp <- integer(n)
  carry <- numeric(length(p))

  for (i in pool) {

    alloc <- .splitSizes(length(i), p, carry)
    carry <- alloc$carry

    grp[i[sample.int(length(i))]] <- rep(seq_along(p), alloc$size)

  }

  grp <- factor(grp, levels = seq_along(p), labels = names(p))

  empty <- names(p)[tabulate(grp, nbins = length(p)) == 0L]

  if (length(empty) > 0L)
    stop(gettextf(
      "subset(s) %s would be empty. Adjust 'p' or use a larger data set.",
      paste(sQuote(empty), collapse = ", ")
    ))

  if (output == "group")
    return(grp)

  idx <- split(seq_len(n), grp)

  if (is.null(dim(x)))
    lapply(idx, function(i) x[i])
  else
    lapply(idx, function(i) x[i, , drop = FALSE])

}


#' Strata as a factor
#'
#' Checks the strata against the number of observations and reduces several
#' vectors to their interaction. `NULL` yields a single stratum, so that the
#' unstratified case runs through the same code path.
#'
#' @param strata the strata, `NULL`, a vector, or a list of vectors.
#' @param n the number of observations.
#'
#' @return a factor of length `n`, missing values kept as a level of their own.
#'
#' @noRd
.splitStrata <- function(strata, n) {

  if (is.null(strata))
    return(factor(rep(1L, n)))

  nStrata <- if (is.data.frame(strata)) nrow(strata) else length(strata)

  if (nStrata != n)
    stop(gettextf("'strata' must be of length %d, not %d.", n, nStrata))

  if (is.list(strata))
    strata <- interaction(strata, drop = TRUE, sep = ":")

  factor(strata, exclude = NULL)

}


#' Integer subset sizes by the largest remainder method
#'
#' Distributes `n` observations over the subsets given by the proportions `p`,
#' taking over the fractional parts left by the previous stratum and passing on
#' its own.
#'
#' @param n the number of observations in the stratum.
#' @param p the subset proportions, summing to 1.
#' @param carry the fractional parts left by the previous stratum, summing to 0.
#'
#' @return a list with the integer sizes, summing to `n`, and the carry for the
#'   next stratum.
#'
#' @noRd
.splitSizes <- function(n, p, carry = numeric(length(p))) {

  quota <- n * p + carry
  size <- pmax(floor(quota), 0)

  # a carried remainder can push a quota beyond what the stratum holds
  while (sum(size) > n) {
    pos <- which(size > 0)
    take <- pos[which.min((quota - size)[pos])]
    size[take] <- size[take] - 1
  }

  rest <- n - sum(size)

  if (rest > 0) {
    take <- order(quota - size, decreasing = TRUE)[seq_len(rest)]
    size[take] <- size[take] + 1
  }

  list(size = as.integer(size), carry = quota - size)

}
