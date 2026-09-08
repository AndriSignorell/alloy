
# tests for splitTrainTest() ---------------------------------------------------

test_that("a single p keeps the old train/test semantics", {

  set.seed(1)
  d <- splitTrainTest(1:100, p = 0.2)

  expect_named(d, c("train", "test"))
  expect_equal(lengths(d), c(train = 80L, test = 20L))

  # the default
  expect_equal(lengths(splitTrainTest(1:100)), c(train = 90L, test = 10L))

})


test_that("p may be a vector of proportions or of ratios", {

  set.seed(1)

  expect_equal(
    lengths(splitTrainTest(1:100, p = c(0.6, 0.2, 0.2))),
    c(train = 60L, validation = 20L, test = 20L)
  )

  # not summing to 1, rescaled
  expect_equal(
    lengths(splitTrainTest(1:100, p = c(6, 2, 2))),
    c(train = 60L, validation = 20L, test = 20L)
  )

  # names are taken from p, in the order given
  d <- splitTrainTest(1:100, p = c(fit = 0.5, calib = 0.25, holdout = 0.25))
  expect_named(d, c("fit", "calib", "holdout"))
  expect_equal(unname(lengths(d)), c(50L, 25L, 25L))

  # order follows p, not the default names
  expect_named(splitTrainTest(1:10, p = c(test = 0.5, train = 0.5)),
               c("test", "train"))

})


test_that("the subsets are disjoint, exhaustive and in the original order", {

  set.seed(2)

  for (p in list(0.1, c(6, 2, 2), c(a = 1, b = 1, c = 1, d = 1))) {

    d <- splitTrainTest(1:97, p = p)

    expect_equal(sum(lengths(d)), 97L)
    expect_equal(sort(unlist(d, use.names = FALSE)), 1:97)

    # observations keep their order within a subset, they are not shuffled
    expect_false(any(vapply(d, is.unsorted, logical(1))))

  }

})


test_that("sizes are as close to n * p as integers allow", {

  set.seed(3)

  for (n in c(7, 13, 50, 101, 999)) {
    for (p in list(c(0.6, 0.2, 0.2), c(0.7, 0.3), c(0.5, 0.3, 0.15, 0.05))) {

      names(p) <- letters[seq_along(p)]

      # a subset that cannot hold a single observation is an error of its own
      if (n * min(p) < 1)
        next

      size <- lengths(splitTrainTest(seq_len(n), p = p))

      expect_equal(sum(size), n)
      expect_true(all(abs(size - n * p) < 1))

    }
  }

})


test_that("vectors, matrices and data frames keep their type", {

  set.seed(4)

  # a matrix must not be flattened, and a single column must not be dropped
  m <- matrix(1:40, ncol = 4)
  d <- splitTrainTest(m, p = 0.2)

  expect_true(all(vapply(d, is.matrix, logical(1))))
  expect_equal(vapply(d, ncol, integer(1)), c(train = 4L, test = 4L))
  expect_equal(vapply(d, nrow, integer(1)), c(train = 8L, test = 2L))

  expect_equal(ncol(splitTrainTest(matrix(1:10, ncol = 1), p = 0.2)$train), 1L)

  # a data frame stays a data frame, columns untouched
  d <- splitTrainTest(iris, p = 0.2)
  expect_true(all(vapply(d, is.data.frame, logical(1))))
  expect_equal(vapply(d, ncol, integer(1)), c(train = 5L, test = 5L))
  expect_equal(vapply(d, nrow, integer(1)), c(train = 120L, test = 30L))

  # a plain vector stays a vector
  expect_true(is.character(splitTrainTest(letters, p = 0.2)$train))

})


test_that("output = 'group' describes the same split", {

  set.seed(5)
  d <- splitTrainTest(iris, p = c(6, 2, 2))

  set.seed(5)
  grp <- splitTrainTest(iris, p = c(6, 2, 2), output = "group")

  expect_s3_class(grp, "factor")
  expect_length(grp, nrow(iris))
  expect_equal(levels(grp), c("train", "validation", "test"))
  expect_equal(as.vector(table(grp)), c(90L, 30L, 30L))
  expect_equal(vapply(d, nrow, integer(1)),
               c(train = 90L, validation = 30L, test = 30L))

  # split() on the memberships reproduces the data subsets
  expect_equal(split(iris, grp), d)

})


test_that("strata are reproduced in every subset", {

  set.seed(6)
  d <- splitTrainTest(iris, p = c(6, 2, 2), strata = iris$Species)

  # 50 per species, split 30/10/10 exactly; one column per subset
  expect_equal(
    vapply(d, function(z) as.vector(table(z$Species)), integer(3)),
    matrix(c(30L, 30L, 30L, 10L, 10L, 10L, 10L, 10L, 10L), nrow = 3,
           dimnames = list(NULL, c("train", "validation", "test")))
  )

  # several strata variables are reduced to their interaction
  a <- rep(1:2, each = 50)
  b <- rep(1:2, times = 50)
  grp <- splitTrainTest(seq_len(100), p = c(0.5, 0.5),
                        strata = data.frame(a, b), output = "group")

  expect_equal(as.vector(table(grp)), c(50L, 50L))
  expect_true(all(abs(table(interaction(a, b), grp) - 12.5) <= 0.5))

})


test_that("rounding remainders are carried across strata", {

  # 40 strata of 3 observations: rounding each stratum on its own would give
  # the leftover to 'train' every time and leave 'test' empty
  s <- rep(1:40, each = 3)

  for (seed in 1:5) {
    set.seed(seed)
    grp <- splitTrainTest(seq_along(s), p = c(6, 2, 2), strata = s,
                          output = "group")
    expect_equal(as.vector(table(grp)), c(72L, 24L, 24L))
  }

  # a stratum smaller than the number of subsets is not an error
  set.seed(7)
  grp <- splitTrainTest(1:10, p = c(6, 2, 2), strata = 1:10, output = "group")
  expect_equal(as.vector(table(grp)), c(6L, 2L, 2L))

})


test_that("missing values form a stratum of their own", {

  set.seed(8)
  s <- c(rep("a", 5), rep(NA, 5))
  grp <- splitTrainTest(1:10, p = c(0.5, 0.5), strata = s, output = "group")

  # nothing is dropped
  expect_false(anyNA(grp))
  expect_equal(as.vector(table(grp)), c(5L, 5L))

  # and the NA rows are spread over both subsets
  expect_true(all(table(addNA(s), grp) > 0))

})


test_that("the split is reproducible via set.seed()", {

  set.seed(9)
  a <- splitTrainTest(1:100, p = c(6, 2, 2), strata = rep(1:4, 25))

  set.seed(9)
  b <- splitTrainTest(1:100, p = c(6, 2, 2), strata = rep(1:4, 25))

  expect_identical(a, b)

  set.seed(10)
  expect_false(identical(a, splitTrainTest(1:100, p = c(6, 2, 2),
                                           strata = rep(1:4, 25))))

})


test_that("invalid p is rejected", {

  expect_error(splitTrainTest(1:10, p = 1), "'p'")
  expect_error(splitTrainTest(1:10, p = 0), "'p'")
  expect_error(splitTrainTest(1:10, p = -0.2), "'p'")
  expect_error(splitTrainTest(1:10, p = NA), "'p'")
  expect_error(splitTrainTest(1:10, p = Inf), "'p'")
  expect_error(splitTrainTest(1:10, p = "0.2"), "'p'")
  expect_error(splitTrainTest(1:10, p = numeric(0)), "'p'")
  expect_error(splitTrainTest(1:10, p = c(0.5, 0)), "'p'")

  # more than three subsets need names
  expect_error(splitTrainTest(1:10, p = rep(0.25, 4)), "'p'")

  # names must be usable
  expect_error(splitTrainTest(1:10, p = c(a = 0.5, a = 0.5)), "'p'")
  expect_error(splitTrainTest(1:10, p = c(a = 0.5, 0.5)), "'p'")

})


test_that("an empty subset and a wrong strata length are errors", {

  expect_error(splitTrainTest(1:5, p = c(0.98, 0.02)), "'test'")
  expect_error(splitTrainTest(1:5, p = c(a = 1, b = 1, c = 1, d = 1, e = 1,
                                         f = 1)), "'f'")

  expect_error(splitTrainTest(1:10, strata = 1:9), "'strata'")
  expect_error(splitTrainTest(iris, strata = 1:10), "'strata'")

})
