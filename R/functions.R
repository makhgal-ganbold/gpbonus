#' @title Compute New Grade Points with Bonus
#'
#' @description Computes student grade points with a bonus by minimizing the absolute difference between the fixed significant level 0.05 and a p-value of Kolmogorov-Smirnov test compares a distribution of grade points with the fixed normal distribution.
#'
#' @param gp a vector; student grade points
#' @param max.bonus a positive numeric; the upper bound of the bonus point
#' @param min.bonus a non-negative numeric; lower bound of the bonus point
#' @param threshold a positive numeric; the upper bound of grade points with bonus
#'
#' @return list; new and old grade points, a bonus point
#'
#' @details It doesn't increase grade points to be lower than 60 with the bonus points.
#'
#' Grade points, which are greater than the \code{threshold}, won't be changed. Also, it won't increase grade points over the upper limit is defined by the parameter \code{threshold}.
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
#' gpbonus::gp_bonus(gp, max.bonus = 30, threshold = Inf)
#'
#' @importFrom magrittr %>%
#' @export

gp_bonus <- function (gp, max.bonus = 15, min.bonus = 0, threshold = 90) {
  # validation
  if (threshold <= 60 || max.bonus <= min.bonus || min.bonus < 0) {
    stop("Invalid parameter(s).")
  }
  # reserve current grade points
  old.gp <- gp
  # convert to numerical values
  gp <- gp %>% gsub(pattern = "[^0-9.]+", replacement = NA) %>% as.numeric()
  # exclude non-numeric grade points
  gp.na <- gp %>% stats::na.omit() %>% as.numeric()
  # parameters
  gp.na.len <- gp.na %>% length()
  gp.na.mu <- gp.na %>% mean()
  na.gp.sigma <- gp.na %>% stats::sd()
  # search for the optimized bonus point
  bonus.point <- gp.na %>% stats::optimize(f = function (x, gp.na) {
    # grade points with a temporary bonus point
    x <- gp.na %>% magrittr::add(x)
    # fix grade points for the threshold
    x <- ifelse(gp.na >= threshold, gp.na, ifelse(x >= threshold, threshold - 1, x))
    # don't touch F points are lower than 60 with the bonus
    x <- ifelse(x < 60, gp.na, x)
    # fix invalid grade points
    x[x > 100] <- 100 # x <- x %>% magrittr::is_greater_than(100) %>% replace(x = x, list = ., values = 100)
    # prevent for ties
    x <- stats::rnorm(n = gp.na.len, mean = 0, sd = 0.01) %>% abs() %>% magrittr::subtract(x) %>% abs()
    # Kolmogorov distance
    x %>%
      stats::ks.test(y = stats::pnorm, exact = FALSE, mean = gp.na.mu, sd = na.gp.sigma) %>%
      magrittr::use_series("p.value") %>%
      magrittr::subtract(0.05) %>%
      abs()
  }, gp.na, lower = min.bonus, upper = max.bonus) %>% magrittr::use_series("minimum") %>% round()
  # fix grade points for the threshold
  gp <- ifelse(gp >= threshold, gp, ifelse(gp + bonus.point >= threshold, threshold - 1, gp + bonus.point))
  # don't touch F points are lower than 60 with the bonus
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
#' @importFrom magrittr %>%
#' @export

gp_summary <- function (gp, plot = TRUE) {
  # convert to numerical values
  gp <- gp %>% gsub(pattern = "[^0-9.]+", replacement = NA) %>% as.numeric()
  # exclude non-numeric grade points
  gp.na <- gp %>% stats::na.omit() %>% as.numeric()
  # check if a histogram will be plotted
  if (plot) {
    # draw a histogram and get a frequency
    h <- gp.na %>% graphics::hist(breaks = seq(from = 0, to = 100, by = 10), right = FALSE, main = "Histogram of Grade Points", xlab = "Grade Points")
    # add a normal density line
    xfit <- 0:100
    yfit <- xfit %>% stats::dnorm(mean = gp.na %>% mean(), sd = gp.na %>% stats::sd())
    yfit <- h %>% magrittr::use_series("mids") %>% magrittr::extract(1:2) %>% diff() * yfit * gp.na %>% length()
    graphics::lines(xfit, yfit, col = "blue", lwd = 2)
  } else {
    # get a frequency only
    h <- gp.na %>% graphics::hist(breaks = seq(from = 0, to = 100, by = 10), right = FALSE, plot = FALSE)
  }
  # return frequency
  c(
    gp %>% length() - gp.na %>% length(),
    h %>% magrittr::use_series("counts") %>% magrittr::extract(1:6) %>% sum(),
    h %>% magrittr::use_series("counts") %>% magrittr::extract(7:10)
  ) %>% magrittr::set_names(value = c("other", "F", "D", "C", "B", "A"))
}
