#' Randomly shuffle the values in each column of a data frame.
#'
#' This function takes a data frame as input and returns a new data frame
#' with the same dimensions as the original data, but with the values in each
#' column randomly shuffled.
#'
#' @param data A data frame.
#'
#' @return A new data frame with the same dimensions as data, but with
#'         the values in each column randomly shuffled.
#'
#' @examples
#' data <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' shuffled_data <- cell_swap(data)
#' shuffled_data
#'
#' @export
cell_swap = function(data){
  # Create a new data frame with the same dimensions as the original data
  shuffled = as.data.frame(matrix(nrow = nrow(data), ncol = ncol(data)))
  colnames(shuffled) = colnames(data)

  # Take a random permutation of the rows for each column
  for(i in seq(1, ncol(data))){
    shuffled[,i] = sample(data[,i])
  }

  return(shuffled)
}

#---------------------------------------------

#' Add Laplace-distributed noise to each column of the data.
#'
#' This function adds Laplace-distributed noise to each column of the input data.
#' The Laplace noise is controlled by the privacy parameter \code{epsilon}.
#' \code{epsilon} determines the scale
#' of the Laplace distribution. Larger \code{epsilon} means less noise.
#'
#' @param data A data frame or matrix containing the original data.
#' @param epsilon The privacy parameter controlling the amount of noise added.
#'
#' @return A data frame or matrix with Laplace-distributed noise added to each column.
#'
#' @examples
#' # Create a sample data frame
#' data <- data.frame(A = c(10, 12, 14), B = c(0, 5, 8))
#' # Add Laplace noise with epsilon = 0.1
#' sensitive_noise(data, epsilon = 0.1)
#'
#' @export
sensitive_noise = function(data, epsilon = 1){
  # add noise to each column of the data according to the maximum difference in the given column from laplace distribution with mean 0 and scale=diff/epsilon
  for(i in seq(ncol(data))){
    diff = max(data[,i]) - min(data[,i])
    data[,i] = data[,i] + rlaplace(nrow(data), 0, diff/epsilon)
  }

  return(data)
}




#---------------------------------------

#' Anonymize a data frame using spectral anonymization.
#'
#' This function takes a data frame M as input and performs singular value decomposition on it, M = UDV'.
#' Then the user defined anonymization function is performed on some matrix (or matrices) of the decomposition.
#' M is reconstructed using the anonymized SVD and returned. Built in functions that work with spectral are
#' "cell_swap" and "sensitive_noise".
#'
#'
#' @param data A data frame to be anonymized and decentered.
#'
#' @param anonymizer A function used to anonymize part of the singluar
#'                    value decomposition. The function should
#'                   take a data frame as input and return an anonymized data frame of the same size.
#'
#' @param sample Logical: Whether to sample a value for a categorical variable represented in the probabilistic state or take the value with the most weight.
#'
#' @param cat_as_num Logical: Whether categorical variables should be returned
#'                    in their probabilistic state or not.
#'
#' @param on_matrices A character string indicating which matrix to anonymize.
#'                    Possible values are "U", "V", "UD", and "DV".
#'                    The default is "U". Note that the transposition is not
#'                    shown here on V for ease of parametrization, but the
#'                    modifications will be made on it's transpose, V'.
#'
#' @param full Logical: Whether to use the full SVD or the reduced SVD (default).
#'
#' @param shuffle Logical: Whether to shuffle the dataset before returning. Warning if FALSE, used to calculate empirical reidentification rate.
#'
#' @param preserveMeans Extract colmeans before svd or not, and add them back afte svd anoymization. Use for cell swap at least, might be usefull for other functions as well. Defaults TRUE.
#'
#' @return A new data frame containing the anonymized data.
#'
#' @importFrom onehot onehot
#'
#' @export
spectral = function(data, anonymizer, on_matrices = "U", preserveMeans = TRUE,  full = FALSE, sample = FALSE, cat_as_num = FALSE, shuffle = FALSE){

  if(!shuffle) warning("Shuffle is FALSE. Use 'anon::shuffle()' before publishing data.\n")

  # Store the original ordering of the columns for later use (reordeding)
  colnames = colnames(data)

  # Make a one-hot object of the data using the 'onehot' package
  oh = onehot(data, stringsAsFactors = TRUE, max_levels = Inf)

  # Identify and store the names of factor columns for later use (recombining)
  names = c()
  for(item in oh){
    if(item$type == "factor") names = append(names, item$name)
  }

  # Perform the one-hot encoding of the data using the previous one-hot object
  encoded = predict(oh, data)

  # Convert categorical columns to {-1, 1} encoding
  for(name in names){
    cols = startsWith(colnames(encoded), name)
    workingset = encoded[,cols]
    withnegatives = t(apply(workingset, 1, function(x) ifelse(x == 0, -1, 1)))
    encoded[,cols] = withnegatives
  }

  # Calculate the column means for centering the data
  if(preserveMeans) means = colMeans(encoded)
  else means = rep(0, ncol(encoded))

  # Center the data by subtracting the column means from each column
  if(preserveMeans) encoded = scale(encoded, center = TRUE, scale = FALSE)

  # Perform Singular Value Decomposition (SVD) on the centered data, M = UDV'
  if(!full){
    svd = svd(encoded)
  } else {
    svd = svd(encoded, nu = nrow(encoded), nv = ncol(encoded))
  }

  # Create a diagonal matrix from the singular values vector
  d_matrix = diag(svd$d, nrow = ncol(svd$u), ncol = ncol(svd$v))

  # Anonymize the specified matrix from the SVD using the provided anonymization function
  if(on_matrices == "U"){
    u_anon = anonymizer(as.data.frame(svd$u))
    udv_anon = as.matrix(u_anon) %*% d_matrix %*% t(svd$v)
  } else if(on_matrices == "V"){
    v_anon = anonymizer(as.data.frame(t(svd$v)))
    udv_anon = svd$u %*% d_matrix %*% as.matrix(v_anon)
  } else if(on_matrices == "UD"){
    ud_anon = anonymizer(as.data.frame(svd$u %*% d_matrix))
    udv_anon = as.matrix(ud_anon) %*% t(svd$v)
  } else if(on_matrices == "DV"){
    dv_anon = anonymizer(as.data.frame(d_matrix %*% t(svd$v)))
    udv_anon = svd$u %*% as.matrix(dv_anon)
  } else {
    stop("Invalid value for 'on_matrices' parameter.")
  }

  # Reconstruct the anonymized data using UDV' form and adding the col means back in
  data_anon = as.data.frame(udv_anon)
  colnames(data_anon) = colnames(encoded)
  for(i in seq(ncol(data_anon))){
    data_anon[,i] = data_anon[,i] + means[i]
  }

  # Normalize the factor columns using normalized inverse logit
  for(name in names){
    cols = startsWith(colnames(data_anon), name)
    workingset = data_anon[,cols]
    normalized = t(apply(workingset, 1, function(x)  (exp(x) / (1 + exp(x)))/sum(exp(x) / (1 + exp(x))) ))
    data_anon[,cols] = normalized
  }

  # Return the categorical values as numerical weights if cat_as_num = TRUE, or as categoricals if FALSE.
  # Use sampling on picking the categories if sampling = TRUE
  if(!cat_as_num){
    inverse = inverse_onehot(data_anon, names, sample)
    colnames_now = colnames(inverse)
    # reorder the columns to match the original data
    inverse = inverse[,match(colnames, colnames_now)]
    # Check whether an exact
    if(shuffle) inverse = shuffle(inverse)
    return(inverse)
  }

  # Return the decentered and anonymized data
  if(shuffle) data_anon = shuffle(data_anon)
  return(data_anon)
}




