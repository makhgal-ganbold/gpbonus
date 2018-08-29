#' @title Compute New Grade Points with Bonus
#'
#' @description Computes student grade points with a bonus by minimizing absolute difference between the fixed significant level 0.05 and a p-value of Kolmogorov-Smirnov test which compares a distribution of grade points with the fixed normal distribution.
#'
#' @param gp a vector; student grade points
#' @param max.bonus a positive numeric; upper bound of the bonus point
#' @param min.bonus a non-negative numeric; lower bound of the bonus point
#' @param disallow.A a logical; If \code{TRUE} (default), grade point "A" won't be allowed.
#'
#' @return list; a bonus point and new grade points
#'
#' @details It leaves untouched F points which are lower than 60 after the bonus was added.
#'
#' @seealso \code{\link{gp_summary}}
#'
#' @examples
#' ## Example 1
#' gp <- c(43, 72, 88, 60, 77, 51, 69, 61, 60, 86, 65)
#' gpbonus::gp_bonus(gp)
#'
#' ## Example 2
#' gp <- c(72, "E", 51, 69, "WF", 81, 61, 75, 54, "W")
#' gpbonus::gp_bonus(gp)
#'
#' @export

gp_bonus <- function (gp, max.bonus = 15, min.bonus = 0, disallow.A = TRUE) {
  # reserve current grade points
  old.gp <- gp
  # convert to numerical values
  gp <- as.numeric(gsub("[^0-9.]+", NA, gp))
  # exclude non-numeric grade points
  gp.na <- as.numeric(stats::na.omit(gp))
  # parameters
  gp.na.len <- length(gp.na)
  gp.na.mu <- mean(gp.na)
  na.gp.sigma <- stats::sd(gp.na)
  # search for the optimized bonus point
  bonus.point <- round(stats::optimize(f = function (x) {
    # grade points with a temporary bonus point
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
  }, lower = min.bonus, upper = max.bonus)$minimum)
  # fix grade points for the "disallow.A" parameter
  if (disallow.A) {
    gp <- ifelse(gp < 90 & gp + bonus.point >= 90, 89, gp + bonus.point)
  } else {
    gp <- gp + bonus.point
  }
  # leave untouched F points which are lower than 60 after the bonus was added
  gp <- ifelse(gp < 60, gp - bonus.point, gp)
  # fix invalid grade points
  gp[gp > 100] <- 100
  # merge with non-numeric points
  gp <- ifelse(is.na(gp), old.gp, gp)
  # result
  list("old.grade.points" = old.gp, "bonus.point" = bonus.point, "new.grade.points" = gp)
}

#' @title Grade Point Summary
#'
#' @description Plots a histogram and returns a frequency of grade points.
#'
#' @param gp a vector; student grade points.
#' @param plot a logical; If \code{TRUE} (default), a histogram is plotted.
#'
#' @return vector; grade point frequency
#'
#' @seealso \code{\link{gp_bonus}}
#'
#' @examples
#' ## Example 1
#' gp <- c(43, 72, 88, 60, 77, 51, 69, 61, 60, 86, 65)
#' new.gp <- gpbonus::gp_bonus(gp)$new.grade.points
#' gpbonus::gp_summary(new.gp)
#'
#' ## Example 2
#' gp <- c(72, "E", 51, 69, "WF", 81, 61, 75, 54, "W")
#' new.gp <- gpbonus::gp_bonus(gp)$new.grade.points
#' gpbonus::gp_summary(new.gp)
#'
#' @export

gp_summary <- function (gp, plot = TRUE) {
  # convert to numerical values
  gp <- as.numeric(gsub("[^0-9.]+", NA, gp))
  # exclude non-numeric grade points
  gp.na <- as.numeric(stats::na.omit(gp))
  # check if a histogram will be plotted
  if (plot) {
    # draw a histogram and get a frequency
    h <- graphics::hist(gp.na, breaks = seq(from = 0, to = 100, by = 10), main = "Histogram of Grade Points", xlab = "Grade Points")
    # add a normal density line
    xfit <- seq(from = 0, to = 100, by = 1)
    yfit <- stats::dnorm(xfit, mean = mean(gp.na), sd = stats::sd(gp.na))
    yfit <- yfit * diff(h$mids[1:2]) * length(gp.na)
    graphics::lines(xfit, yfit, col = "blue", lwd = 2)
  } else {
    # get a frequency only
    h <- graphics::hist(gp.na, breaks = seq(from = 0, to = 100, by = 10), plot = FALSE)
  }
  # return frequency
  c("other" = length(gp) - length(gp.na), "F" = sum(h$counts[1:6]), "D" = h$counts[7], "C" = h$counts[8], "B" = h$counts[9], "A" = h$counts[10])
}
