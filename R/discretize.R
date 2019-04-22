
#' Discretize two numeric columns of a data frame into a matrix.
#'
#' @param .data Data frame
#' @param dims Dimensions of matrix
#' @param x Name of x column in the data frame
#' @param y Name of y column in the data frame
#'
#' @return A matrix with dimensions `dims`
#' @export
#'
discretize <- function(.data, dims = c(600, 600), x = "x", y = "y"){

  assertthat::assert_that(is.data.frame(.data))
  assertthat::assert_that(is.numeric(.data[[x]]))
  assertthat::assert_that(is.numeric(.data[[y]]))
  assertthat::assert_that(is.numeric(dims))
  assertthat::assert_that(all(dims > 0))
  assertthat::assert_that(length(dims) == 2)

  range_x <- range(.data[[x]], na.rm = TRUE)
  range_y <- range(.data[[y]], na.rm = TRUE)
  discretize_cpp(.data[, c(x, y)], dims, range_x, range_y)
}


#' Generate strange attractor.
#'
#' @param a Numeric vector of length 14
#' @param n Number of points to generate
#' @param x0 Initial value for x
#' @param y0 Initial value for y
#'
#' @return
#' @export
#'
strange_attractor <- function(a, n, x0 = 0, y0 = 0){
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
