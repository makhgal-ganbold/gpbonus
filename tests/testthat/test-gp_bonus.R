
context("gp_bonus() function test")

test_that(desc = "pure numerical grade points", code = {

  gp.na <- c(43, 72, 88, 60, 77, 51, 69, 61, 60, 86, 65)
  bonus.points <- 0:15
  disallow.A <- TRUE
  gp.na.len <- length(gp.na)
  gp.na.mu <- mean(gp.na)
  na.gp.sigma <- stats::sd(gp.na)

  bonus.point <- bonus.points[which.min(sapply(X = bonus.points, FUN = function (x) {
    x <- gp.na + x
    # fix invalid grade points
    x[x > 100] <- 100
    # fix grade points for the "disallow.A" parameter
    if (disallow.A) {
      x <- ifelse(gp.na < 90 & x >= 90, 89, x)
    }
    # leave untouched F points which are lower than 60 after the bonus was added
    x <- ifelse(x < 60, gp.na, x)
    # prevent for ties
    x <- x - abs(stats::rnorm(n = gp.na.len, mean = 0, sd = 0.01))
    # Kolmogorov distance
    abs(stats::ks.test(x = x, y = stats::pnorm, exact = FALSE, mean = gp.na.mu, sd = na.gp.sigma)$p.value - 0.05)
  }))]

  new.gp <- c(43, 82, 89, 70, 87, 61, 79, 71, 70, 89, 75) # +10 (bonus.point == 10)

  expect_silent(res <- gp_bonus(gp = gp.na, max.bonus = max(bonus.points), min.bonus = min(bonus.points), disallow.A = disallow.A))
  expect_is(res, "list")
  expect_identical(object = res$old.grade.points, expected = gp.na)
  expect_length(object = res$new.grade.points, n = length(gp.na))
  expect_identical(object = res$new.grade.points, expected = new.gp)
  expect_equivalent(object = res$bonus.point, expected = bonus.point)

})

test_that(desc = "grade points with letters", code = {

  gp.letter <- c(72, "E", 51, 69, "WF", 81, 61, 75, 54, "W")
  gp.na <- c(72, 51, 69, 81, 61, 75, 54)
  bonus.points <- 0:25
  disallow.A <- FALSE
  gp.na.len <- length(gp.na)
  gp.na.mu <- mean(gp.na)
  na.gp.sigma <- stats::sd(gp.na)

  bonus.point <- bonus.points[which.min(sapply(X = bonus.points, FUN = function (x) {
    x <- gp.na + x
    # fix invalid grade points
    x[x > 100] <- 100
    # fix grade points for the "disallow.A" parameter
    if (disallow.A) {
      x <- ifelse(gp.na < 90 & x >= 90, 89, x)
    }
    # leave untouched F points which are lower than 60 after the bonus was added
    x <- ifelse(x < 60, gp.na, x)
    # prevent for ties
    x <- x - abs(stats::rnorm(n = gp.na.len, mean = 0, sd = 0.01))
    # Kolmogorov distance
    abs(stats::ks.test(x = x, y = stats::pnorm, exact = FALSE, mean = gp.na.mu, sd = na.gp.sigma)$p.value - 0.05)
  }))]

  new.gp.letter <- c(86, "E", 65, 83, "WF", 95, 75, 89, 68, "W") # +14 (bonus.point == 14)

  expect_silent(res <- gp_bonus(gp = gp.letter, max.bonus = max(bonus.points), min.bonus = min(bonus.points), disallow.A = disallow.A))
  expect_is(res, "list")
  expect_identical(object = res$old.grade.points, expected = gp.letter)
  expect_length(object = res$new.grade.points, n = length(gp.letter))
  expect_equivalent(object = res$bonus.point, expected = bonus.point)
  expect_identical(object = res$new.grade.points, expected = new.gp.letter)

})
