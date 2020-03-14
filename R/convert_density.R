#' convert_density
#'
#' @param x A vector of numbers to be converted.
#' @param from A character vector of the units x is in. Must be length 1 or same length as x.
#' @param to A character vector of the units into which x will be converted. Must be length 1 or same length as x.
#'
#' @return A vector of numbers converted FROM old units TO new units.
#' @export
#'
#' @examples
#' convert_density(1, "kg/m3", "lb/ft3")
#' convert_density(c(1, 10, 100), "kg/m3", "lb/ft3")
#' convert_density(c(1, 10, 100), c("g/l", "lb/gal", "lb/ft3"), c("lb/ft3", "g/l", "lb/gal"))
convert_density <- function(x, from, to) {
  ## A function for converting between density units (kg/m3, g/L, mg/L, lb/gal,
  ## lb/ft3)

  ## List of allowable units
  legal_units <- c("kg/m3", "g/l", "mg/l", "lb/gal", "lb/ft3")
  legal_units_message <- paste(legal_units, collapse = ", ")

  ## List divisors for legal_units to convert to kg/m3
  unit_div <- c(1, 1, 1000, 0.008345406, 0.062428)


  ## Give an error message in the from and to inputs have invalid units
  if ( sum(from %in% legal_units) < length(from) ) stop(paste("Input variable 'from' must be", legal_units_message,"and must be lowercase."))

  if (sum(to %in% legal_units) < length(to) ) stop(paste("Input variable 'to' must be", legal_units_message,"and must be lowercase."))


  ## Give an error message if the from and to inputs are not of valid length.
  ## They should be either length 1L or equal to the length of the x vector.
  if ( !(length(from) == 1L | length(from) == length(x)) ) stop("Input variable 'from' must have a single element or be of equal length to 'x'.")

  if ( !(length(to) == 1L | length(to) == length(x)) ) stop("Input variable 'to' must have a single element or be of equal lengtb to 'x'.")


  ## Modify the structure of the input variables such that they all have the
  ## same length as 'x'.
  if (length(to)   == 1L) to <- rep(to, length(x))
  if (length(from) == 1L) from <- rep(from, length(x))


  ## Apply the unit conversion to all elements of the x vector.
  result <- mapply(
    function(a, b, c) a / unit_div[which(legal_units == b)] * unit_div[which(legal_units == c)],
    x, from, to)

  return(result)

}
