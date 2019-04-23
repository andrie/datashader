#' Generate strange attractor.
#'
#' @param a Numeric vector of length 14
#' @param n Number of points to generate
#' @param x0 Initial value for x
#' @param y0 Initial value for y
#'
#' @return Data frame with columns `x` and `y`, and `n` rows.
#' @export
#'
strange_attractor <- function(a, n, x0 = 1, y0 = 1){
  assertthat::assert_that(is.vector(a))
  assertthat::assert_that(all(is.numeric(a)))
  assertthat::assert_that(length(a) == 14)
  assertthat::assert_that(is.numeric(n))
  assertthat::assert_that(length(n) == 1)
  assertthat::assert_that(n > 0)
  assertthat::assert_that(is.numeric(x0))
  assertthat::assert_that(length(x0) == 1)
  assertthat::assert_that(is.numeric(y0))
  assertthat::assert_that(length(y0) == 1)

  strange_attractor_cpp(a, n, x0, y0)
}
