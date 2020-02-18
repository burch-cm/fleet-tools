
vin_transliterate <- function(vin) {
  vin <- toupper(trimws(vin))
  ltr <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
           "A", "B", "C", "D", "E", "F", "G", "H",
           "J", "K", "L", "M", "N",      "P",      "R",
           "S", "T", "U", "V", "W", "X", "Y", "Z")
  num <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
           1, 2, 3, 4, 5, 6, 7, 8,
           1, 2, 3, 4, 5,    7,    9,
           2, 3, 4, 5, 6, 7, 8, 9)
  translit <- data.frame(ltr, num)
  vin_l <- unlist(strsplit(vin, ""))
  res <- vector(mode = "character", length = length(vin_l))
  for (i in 1:length(vin_l)) {
    res[i] <- translit[translit$ltr == vin_l[i], "num"]  
  }
  # replace check digit
  # note that check digit has no weight 
  # so it doesn't matter if it's not transliterated
  res[9] <- vin_l[9]
  return(paste(res, collapse = ""))
}

vin_check_digit <- function(vin, return_digit = FALSE) {
  vin_tl <- vin_transliterate(vin)
  # VIN weights given by DOT
  weights_1 <- c(8, 7, 6, 5, 4, 3, 2, 10)
  weights_2 <- c(9, 8, 7, 6, 5, 4, 3, 2)
  bits <- unlist(strsplit(vin_tl, ""))
  vin_bit_1 <- as.numeric(bits[1:8])
  vin_bit_2 <- as.numeric(bits[10:17])
  check_bit <- ifelse(bits[9] %in% LETTERS, bits[9], as.numeric(bits[9]))
  calc_digit <- (sum(vin_bit_1 * weights_1) + sum(vin_bit_2 * weights_2)) %% 11
  if (return_digit) return(calc_digit)
  if (calc_digit == check_bit) {
    return(TRUE)
  } else if (calc_digit == 10 & check_bit == "X"){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

vin_validate <- function(vin,
                         check_digit = TRUE, 
                         warnings = FALSE,
                         verbose = FALSE) {
  # VIN must be 17 characters long
  if (verbose) cat("Checking VIN length...")
  if (nchar(trimws(vin)) != 17) {
    if (warnings) warning("Incorrect VIN length: VIN must be 17 characters long")
    return(FALSE)
  }
  if (verbose) cat("VIN length OK")
  # VIN must not contain Q, O, I
  if (verbose) cat("Checking VIN characters...")
  if (grepl("[OoQqIi]", vin)) {
    if (warnings) warning("Incorrect VIN characters: letters 'O/o', 'Q/q', 'I/i' are not valid VIN characters")
    return(FALSE)
  }
  if (verbose) cat("VIN characters OK")
  # calculate check digit if required
  # all vehicles sold in the US must have a valid check digit
  if (check_digit) {
    if (verbose) cat("Checking check digit...")
    cd <- vin_check_digit(vin)
    if (cd == FALSE) {
      if (warnings) warning("Check digit is incorrect, VIN may be invalid")
      return(FALSE)
    }
    if (verbose) cat("Check digit OK")
  }
  return(TRUE)
}

vin_validate_vec <- Vectorize(vin_validate)
