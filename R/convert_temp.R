#' Convert temperature units--C, F, K, and R
#'
#' @param x A vector of numbers to be converted.
#' @param from A character vector of the units x is in. Must be length 1 or same length as x.
#' @param to A character vector of the units into which x will be converted. Must be length 1 or same length as x.
#'
#' @return A vector of numbers converted FROM old units TO new units.
#' @export
#'
#' @examples
convert_temp <- function(x, from, to) {
  ## A function for converting temperature units (C, F, K, and R)


  ## Give an error message in the from and to inputs have invalid units
  if ( sum(from %in% c("C", "F", "K", "R")) < length(from) ) stop("Input variable 'from' must be a C, F, K, or R and the letter must be capitalized.")

  if (sum(to %in% c("C", "F", "K", "R")) < length(to) ) stop("Input variable 'to' must be a C, F, K, or R and the letter must be capitalized.")


  ## Give an error message if the from and to inputs are not of valid length.
  ## They should be either length 1L or equal to the length of the x vector.
  if ( !(length(from) == 1L | length(from) == length(x)) ) stop("Input variable 'from' must have a single element or be of equal lenght to 'x'.")

  if ( !(length(to) == 1L | length(to) == length(x)) ) stop("Input variable 'to' must have a single element or be of equal lenght to 'x'.")


  ## Define a function for doing temp conversion.
  convert <- function(input, f, t) {

    if (f == "C") {
      if (t == "F") out <- (9/5) * input + 32
      if (t == "K") out <- input + 273.15
      if (t == "R") out <- (9/5) * input + 491.67
    }

    if (f == "F") {
      if (t == "C") out <- (5/9) * (input - 32)
      if (t == "K") out <- (5/9) * (input - 255.37)
      if (t == "R") out <- input + 459.67
    }

    if (f == "K") {
      if (t == "C") out <- input - 273.15
      if (t == "F") out <- (9/5) * (input - 255.37)
      if (t == "R") out <- (9/5) * input
    }

    if (f == "R") {
      if (t == "C") out <- (5/9) * input
      if (t == "F") out <- input - 459.67
      if (t == "K") out <- (5/9) * input
    }

    return(out)  ## Exit internal helper function
  }


  ## Modify the structure of the input variables such that they all have the
  ## same length as 'x'.
  if (length(to)   == 1L) to <- rep(to, length(x))
  if (length(from) == 1L) from <- rep(from, length(x))


  ## Apply the convert function to the elements of x, from, and to in
  ## sequential order.
  result <- mapply(convert, x, from, to)

  return(result)  ## Exit function
}
