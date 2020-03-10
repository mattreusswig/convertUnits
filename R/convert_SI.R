convert_SI <- function(x, from, to) {
  ## A function for converting SI units (kilo, base, deci, centi, milli, 
  ## micro, nano, pico)
  
  
  # List of allowable units (base = unmodified 10^0 unit)
  legal_units <- c("kilo", "base", "deci", "centi", "milli",
                   "micro", "nano", "pico")
  legal_units_message <- paste(legal_units, collapse = ", ")
  
  
  ## Give an error message in the from and to inputs have invalid units
  if ( sum(from %in% legal_units) < length(from) ) stop(paste("Input variable 'from' must be", legal_units_message,"and must be lowercase."))
  
  if (sum(to %in% legal_units) < length(to) ) stop(paste("Input variable 'to' must be", legal_units_message,"and must be lowercase."))
  
  
  ## Give an error message if the from and to inputs are not of valid length.
  ## They should be either length 1L or equal to the length of the x vector.
  if ( !(length(from) == 1L | length(from) == length(x)) ) stop("Input variable 'from' must have a single element or be of equal length to 'x'.")
  
  if ( !(length(to) == 1L | length(to) == length(x)) ) stop("Input variable 'to' must have a single element or be of equal lengtb to 'x'.")
  
  
  ## Define a table with the SI prefixes the value they must be multiplied by 
  ## to get to base/no-prefix unit (e.g., kg * 1000 = grams)
  conversions <- data.frame(prefix = c("kilo", "base", "deci", "centi", "milli",
                                       "micro", "nano", "pico"),
                            mult = c(1000, 1, 0.1, 0.01, 1e-3, 1e-6, 1e-9, 1e-12))
  
  ## Modify the structure of the input variables such that they all have the
  ## same length as 'x'.
  if (length(to)   == 1L) to <- rep(to, length(x))
  if (length(from) == 1L) from <- rep(from, length(x))
  
  
  convert_helper <- function(input, f, t) {
    
    fmult <- conversions[conversions$prefix == f, "mult"]
    tdiv  <- conversions[conversions$prefix == t, "mult"]
    
    out <- input * fmult / tdiv
    
    return(out)
    
  }
  
  ## Apply the unit conversion to all elements of the x vector.
  result <- mapply(convert_helper, x, from, to)
  
  return(result)
  
}
