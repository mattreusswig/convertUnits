#' convert_salinity
#'
#' @param x A vector of numbers to be converted.
#' @param from A character vector of the units x is in. Must be length 1 or same length as x.
#' @param to A character vector of the units into which x will be converted. Must be length 1 or same length as x.
#'
#' @return A vector of numbers converted FROM old units TO new units.
#' @export
#'
#' @examples
convert_salinity <- function(x, from, to) {
  ## A function for converting between salinity units (ppt, psu)
  # source: http://www.teos-10.org/pubs/TEOS-10_Primer.pdf

  # List of allowable units
  legal_units <- c("ppt", "psu")
  legal_units_message <- paste(legal_units, collapse = ", ")


  ## Give an error message in the from and to inputs have invalid units
  if ( sum(from %in% legal_units) < length(from) ) stop(paste("Input variable 'from' must be", legal_units_message,"and must be lowercase."))

  if (sum(to %in% legal_units) < length(to) ) stop(paste("Input variable 'to' must be", legal_units_message,"and must be lowercase."))


  ## Give an error message if the from and to inputs are not of valid length.
  ## They should be either length 1L or equal to the length of the x vector.
  if ( !(length(from) == 1L | length(from) == length(x)) ) stop("Input variable 'from' must have a single element or be of equal length to 'x'.")

  if ( !(length(to) == 1L | length(to) == length(x)) ) stop("Input variable 'to' must have a single element or be of equal lengtb to 'x'.")


  ## Define a table with the legal units the value they must be multiplied by
  ## to get to ppt unit
  conversions <- data.frame(prefix = c("ppt", "psu"),
                            mult =   c(1, 1.004715))

  ## Modify the structure of the input variables such that they all have the
  ## same length as 'x'.
  if (length(to)   == 1L) to <- rep(to, length(x))
  if (length(from) == 1L) from <- rep(from, length(x))


  ## This function converts from unit to meters, then converts meters to target units
  convert_helper <- function(input, a, b) {

    fmult <- conversions[conversions$prefix == a, "mult"]
    tdiv  <- conversions[conversions$prefix == b, "mult"]

    out <- input * fmult / tdiv

    return(out)

  }

  ## Apply the unit conversion to all elements of the x vector.
  result <- mapply(convert_helper, x, from, to)

  return(result)

}
