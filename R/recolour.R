#' Flip image by 180 degrees
#'
#' @param x Matrix
#'
#' @return Matrix
#' @export
flip_180 <- function(x){
  x[rev(seq_len(nrow(x))), ]
}

#' Invert colours
#'
#' @param x Matrix
#'
#' @return Matrix
#' @export
invert <- function(x) {
  1 - x / max(x, na.rm = TRUE)
}

#' Use palette from ColorBrewer to recolour the image
#'
#' @param x Matrix
#' @param palette ColorBrewer palette
#' @param zero_colour Colour to use for zero and NA values
#'
#' @return Matrix
#' @export
#' @importFrom scales col_numeric
#' @importFrom grDevices as.raster
recolour <- function(x, palette = "Blues", zero_colour = "#FFFFFF", invert = FALSE){
  dims <- dim(x)
  x[x == 0] <- NA
  if (invert) x  <- invert(x)
  col_range <- range(x, na.rm = TRUE)
  col_custom <- scales::col_numeric(palette = palette, domain = col_range, na.color = zero_colour)
  as.raster(
    matrix(
      col_custom(x),
      nrow = dims[1], ncol = dims[2])
  )
}

#' @export
recolor <- recolour
