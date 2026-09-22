# Fixtures for the pseudoR2 tests
#
# One grouped binomial data set, deterministic, with the situations that have
# historically broken aggregation invariance: cells of very different size, a
# cell with no success at all, a cell with no failure and an empty cell.


# grouped: k successes out of n trials per cell of a 5 x 4 design
groupedBinom <- local({

  set.seed(163)

  d <- expand.grid(
    location = factor(1:5),
    period = factor(c(4, 7, 8, 11))
  )

  eta <- 2.4 -
    c(0, 0.4, 1.2, 0.9, 4.6)[as.integer(d$location)] -
    c(0, 1.1, 1.3, 1.5)[as.integer(d$period)]

  d$total <- c(94, 108, 123, 104, 93, 98, 106, 130, 97, 113,
               86, 96, 119, 99, 88, 155, 122, 125, 132, 138)

  d$survive <- rbinom(nrow(d), d$total, plogis(eta))

  d$survive[4] <- d$total[4]   # no failure
  d$survive[20] <- 0           # no success
  d$survive[10] <- 0           # empty cell, see below
  d$total[10] <- 0

  d$prop <- ifelse(d$total > 0, d$survive / d$total, 0)

  d
})


# one row per single Bernoulli trial
expandedBinom <- local({

  d <- groupedBinom

  out <- do.call(rbind, lapply(seq_len(nrow(d)), function(i) {

    if (d$total[i] == 0L) {
      return(NULL)
    }

    data.frame(
      location = d$location[i],
      period = d$period[i],
      survive = rep(c(1L, 0L), c(d$survive[i], d$total[i] - d$survive[i]))
    )
  }))

  rownames(out) <- NULL

  out
})


# the four ways of writing the same binomial model down
binomFits <- function(link = "logit") {

  fam <- stats::binomial(link)

  list(
    matrix = stats::glm(cbind(survive, total - survive) ~ location + period,
                        family = fam, data = groupedBinom),

    single = stats::glm(survive ~ location + period,
                        family = fam, data = expandedBinom),

    weights = stats::glm(prop ~ location + period, family = fam,
                         data = groupedBinom, weights = total),

    noResponse = stats::glm(cbind(survive, total - survive) ~ location + period,
                            family = fam, data = groupedBinom, y = FALSE)
  )
}


# binary responses, the case where no aggregation is involved
#
# the family object is put into the call rather than a link name, so that
# update() inside a test does not have to find 'link' again
binaryFit <- function(link = "logit", ...) {

  args <- c(
    list(quote(vs ~ mpg + wt), data = quote(mtcars),
         family = stats::binomial(link)),
    list(...)
  )

  do.call(stats::glm, args)
}


# the same data with missing values, kept here so that update() finds them
mtcarsNA <- local({
  d <- mtcars
  d$mpg[c(3, 11, 29)] <- NA
  d
})


# log-likelihood of an intercept-only fit, the naive way round
nullLogLikByUpdate <- function(fit) {
  as.numeric(stats::logLik(stats::update(fit, . ~ 1)))
}
