#' Encrypts a message using RSA encryption.
#'
#' This function takes a message as input and encrypts it using the RSA encryption
#' algorithm with the provided parameters.
#'
#' @param m The message to be encrypted.
#' @param e The public exponent used for encryption.
#' @param n The modulus used for encryption.
#' @param bits The number of bits used for precision in the encrypted message.
#'
#' @return A mpfr list representing the encrypted message.
#' @importFrom Rmpfr mpfr
#' @importFrom openssl bignum bignum_mod_exp
encrypt_message = function(m, e, n, bits) {

  # Convert each character of the message to raw
  m = lapply(as.character(m), charToRaw)

  # Convert each raw character to a bignum
  message_bignum <- lapply(m, bignum)

  # Encrypt each bignum using bignum_mod_exp function with given parameters e and n
  encrypted_message <- lapply(message_bignum, function(x) bignum_mod_exp(x, e, n))

  # read the encrypted values to char first and then to big floats
  as_character = lapply(encrypted_message, as.character)
  as_numerical = mpfr(unlist(as_character), bits)

  # Return the encrypted message
  return(as_numerical)
}



#' Encrypts the data using RSA encryption and scales the encrypted values to [0, 1].
#'
#' This function takes a data frame as input, encrypts each column using RSA encryption
#' with the provided number of bits and scales the encrypted values to the [0, 1] range.
#'
#' @param data The data frame to be encrypted and scaled.
#' @param bits The number of bits used for precision in the RSA encryption.
#' @param my_key Logical parameter: whether your own key from .ssh folder should be used or not
#' @param charAsNum Logical parameter: Whether to keep character columns as character or not.
#' @param shuffle Logical parameter: whether the resulting data will be shuffled before returning. Warning if FALSE.
#'
#' @return A scaled data frame where each column contains the encrypted and scaled values.
#'
#' @importFrom openssl rsa_keygen my_key
#' @importFrom Rmpfr mpfr
#'
#' @examples
#' data <- data.frame(x = c(10, 20, 30), y = c(5, 15, 25))
#' encrypted_data <- encrypt(data, bits = 2048)
#'
#' @export
encrypt <- function(data, my_key = FALSE, bits = 2048, charAsNum = FALSE, shuffle = FALSE) {

  if(!shuffle) warning("Shuffle is FALSE. Use 'anon::shuffle()' before publishing data.\n")

  # Get non-numeric column ids
  non_numeric_cols = which(sapply(data, function(x) !is.numeric(x)))

  # Use existing key or generate a new RSA key
  if(my_key){
    key = my_key()
  } else{
    key = rsa_keygen(bits)
    }

  # Generate the matching public key
  pubkey = as.list(key)$pubkey

  # parameters for rsa
  e <- pubkey$data$e
  n <- pubkey$data$n

  # Encrypt the data
  encrypted = lapply(data, function(x) encrypt_message(x, e, n, bits))

  # Make an empty dataframe of the original size to store the minmax scaled values
  col = ncol(data)
  row = nrow(data)
  scaled = data.frame(matrix(nrow = row, ncol = col))
  colnames(scaled) = colnames(data)

  # Amount of unique values before conversion to double
  uniques_enc = c()

  for(i in seq_len(col)){
    min = min(encrypted[[i]])
    max = max(encrypted[[i]])
    diff = max - min

    uniques_enc[i] = length(unique(encrypted[[i]]))

    # For a given column, either minmax scale it, or if it has diff = 0, return 0
    if(diff != 0){
      for(j in seq_len(row)){
        scaled[j,i] = as.double((encrypted[[i]][[j]] - min) / diff)
      }
    }
    else{
      for(j in seq_len(row)){
        scaled[j,i] = 0
      }
    }
  }

  # Amount of unique values after conversion to double
  uniques_double = sapply(scaled, function(x) length(unique(x)))

  # Geometric mean for the column wise data loss vectors
  geometric_mean_result <- prod(uniques_double / uniques_enc)^(1/length(uniques_double / uniques_enc))

  cat("Information loss on conversion to double:",1-geometric_mean_result,"\n")

  # Turn the non-numeric columns back to character
  if(!charAsNum) scaled[, non_numeric_cols] = sapply(scaled[, non_numeric_cols], as.character)

  # Return the encrypted and scaled data
  if(shuffle) scaled = shuffle(scaled)
  return(scaled)

}

