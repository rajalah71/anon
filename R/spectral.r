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
#' Higher \code{epsilon} values allow for less noise and provide lower privacy guarantees.
#' The privacy guarantees depend on the choice of \code{epsilon}, which determines the scale
#' of the Laplace distribution.
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



#---------------------------------------------

#' Anonymize a data frame using spectral decomposition.
#'
#' This function takes a data frame as input and performs spectral decomposition
#' to anonymize the data by altering its left singular vectors. The anonymization
#' is done by applying a user-defined anonymizer function to vectors.
#' The function then reconstructs the decentered, anonymized data
#' and returns it as a new data frame.
#'
#' @param data A data frame to be anonymized and decentered.
#'
#' @param anonymizer A function used to anonymize the U matrix from singluar
#'                    value decomposition. The function should
#'                   take a data frame as input and return an anonymized data frame.
#'                   For example, the anonymizer function could perform a random
#'                   permutation of rows to achieve anonymization.
#'
#' @param sample Logical: Whether to sample from the numerical columns
#'                or take the max
#'
#' @param cat_as_num Logical: Whether categorical variables should be returned
#'                    as numerical or not
#'
#' @return A new data frame containing the anonymized data.
#'
#' @importFrom onehot onehot
#'
spectral_legacy = function(data, anonymizer, sample = FALSE, cat_as_num = FALSE){

  # One-hot encode the data using the 'onehot' package
  oh = onehot(data, stringsAsFactors = TRUE, max_levels = Inf)

  # Initialize an empty vector to store the names of factor columns
  names = c()

  # Identify and store the names of factor columns
  for(item in oh){
    if(item$type == "factor") names = append(names, item$name)
  }

  # Perform the one-hot encoding of the data using the trained model
  encoded = predict(oh, data)

  # Convert categorical columns to {-1, 1} encoding
  for(name in names){
    cols = startsWith(colnames(encoded), name)
    workingset = encoded[,cols]
    withnegatives = t(apply(workingset, 1, function(x) ifelse(x == 0, -1, 1)))
    encoded[,cols] = withnegatives
  }

  # Store the {-1, 1} encoded data as 'onehot'
  onehot = encoded

  # Make a new empty dataframe of the original size and column names to store the centered data
  centered = emptydf(onehot)

  # Calculate the column means for centering the data
  means = sapply(onehot, mean)

  # Center the data by subtracting the column means from each column
  for(i in seq(ncol(onehot))){
    centered[,i] = onehot[,i] - means[i]
  }

  # Perform Singular Value Decomposition (SVD) on the centered data, M = UDV'
  svd = svd(centered)

  # Anonymize matrix U from the SVD using the provided anonymization function
  u_anon = anonymizer(as.data.frame(svd$u))

  # Reconstruct the anonymized centered data using UDV' form
  data_anon = as.data.frame(as.matrix(u_anon) %*% diag(svd$d) %*% t(svd$v))
  colnames(data_anon) = colnames(data)

  # Make a new empty dataframe of the original size and column names to store the decentered, anonymized data
  decentered_anon = emptydf(onehot)

  # Add the column means back to the anonymized data to decenter it
  for(i in seq(ncol(onehot))){
    decentered_anon[,i] = data_anon[,i] + means[i]
  }

  # Normalize the factor columns using softmax
  for(name in names){
    cols = startsWith(colnames(decentered_anon), name)
    workingset = decentered_anon[,cols]
    normalized = t(apply(workingset, 1, function(x)  (exp(x) / (1 + exp(x)))/sum(exp(x) / (1 + exp(x))) ))
    decentered_anon[,cols] = normalized
  }

  # Return the categorical values as categorical if wanted, using sampling if TRUE
  if(!cat_as_num){
    return(inverse_onehot(decentered_anon, names, sample))
  }

  # Return the decentered and anonymized data
  return(decentered_anon)
}

#---------------------------------------

#' Anonymize a data frame using spectral decomposition.
#'
#' This function takes a data frame as input and performs spectral decomposition
#' to anonymize the data by altering its left singular vectors. The anonymization
#' is done by applying a user-defined anonymizer function to vectors.
#' The function then reconstructs the decentered, anonymized data
#' and returns it as a new data frame.
#'
#' @param data A data frame to be anonymized and decentered.
#'
#' @param anonymizer A function used to anonymize the U matrix from singluar
#'                    value decomposition. The function should
#'                   take a data frame as input and return an anonymized data frame.
#'                   For example, the anonymizer function could perform a random
#'                   permutation of rows to achieve anonymization.
#'
#' @param sample Logical: Whether to sample from the numerical columns
#'                or take the max
#'
#' @param cat_as_num Logical: Whether categorical variables should be returned
#'                    as numerical or not
#'
#' @param on_matrices A character string indicating which matrix to anonymize.
#'                    Possible values are "U", "D", "V", "UD", and "DV".
#'                    The default is "U". Note that the transposition is not
#'                    shown here on V for ease of parametrization, but the
#'                    modifications will be made on it's transpose.
#'
#' @return A new data frame containing the anonymized data.
#'
#' @importFrom onehot onehot
#'
spectral_legacy2 = function(data, anonymizer, on_matrices = "U", sample = FALSE, cat_as_num = FALSE){

  # One-hot encode the data using the 'onehot' package
  oh = onehot(data, stringsAsFactors = TRUE, max_levels = Inf)

  # Initialize an empty vector to store the names of factor columns
  names = c()

  # Identify and store the names of factor columns
  for(item in oh){
    if(item$type == "factor") names = append(names, item$name)
  }

  # Perform the one-hot encoding of the data using the trained model
  encoded = predict(oh, data)

  # Convert categorical columns to {-1, 1} encoding
  for(name in names){
    cols = startsWith(colnames(encoded), name)
    workingset = encoded[,cols]
    withnegatives = t(apply(workingset, 1, function(x) ifelse(x == 0, -1, 1)))
    encoded[,cols] = withnegatives
  }

  # Store the {-1, 1} encoded data as 'onehot'
  onehot = encoded

  # Make a new empty dataframe of the original size and column names to store the centered data
  centered = emptydf(onehot)

  # Calculate the column means for centering the data
  means = sapply(onehot, mean)

  # Center the data by subtracting the column means from each column
  for(i in seq(ncol(onehot))){
    centered[,i] = onehot[,i] - means[i]
  }

  # Perform Singular Value Decomposition (SVD) on the centered data, M = UDV'
  svd = svd(centered)

  # Anonymize the specified matrix from the SVD using the provided anonymization function
  if(on_matrices == "U"){
    u_anon = anonymizer(as.data.frame(svd$u))
    udv_anon = as.matrix(u_anon) %*% diag(svd$d) %*% t(svd$v)
  } else if(on_matrices == "D"){
    d_anon = anonymizer(as.data.frame(diag(svd$d)))
    udv_anon = svd$u %*% as.matrix(d_anon) %*% t(svd$v)
  } else if(on_matrices == "V"){
    v_anon = anonymizer(as.data.frame(svd$v))
    udv_anon = svd$u %*% diag(svd$d) %*% as.matrix(t(v_anon))
  } else if(on_matrices == "UD"){
    ud_anon = anonymizer(as.data.frame(svd$u %*% diag(svd$d)))
    udv_anon = as.matrix(ud_anon) %*% t(svd$v)
  } else if(on_matrices == "DV"){
    dv_anon = anonymizer(as.data.frame(diag(svd$d) %*% t(svd$v)))
    udv_anon = svd$u %*% as.matrix(dv_anon)
  } else {
    stop("Invalid value for 'on_matrices' parameter.")
  }

  # Reconstruct the anonymized centered data using UDV' form
  data_anon = as.data.frame(udv_anon)
  colnames(data_anon) = colnames(data)

  # Make a new empty dataframe of the original size and column names to store the decentered, anonymized data
  decentered_anon = emptydf(onehot)

  # Add the column means back to the anonymized data to decenter it
  for(i in seq(ncol(onehot))){
    decentered_anon[,i] = data_anon[,i] + means[i]
  }

  # Normalize the factor columns using normalized inverse logit
  for(name in names){
    cols = startsWith(colnames(decentered_anon), name)
    workingset = decentered_anon[,cols]
    normalized = t(apply(workingset, 1, function(x)  (exp(x) / (1 + exp(x)))/sum(exp(x) / (1 + exp(x))) ))
    decentered_anon[,cols] = normalized
  }

  # Return the categorical values as categorical if wanted, using sampling if TRUE
  if(!cat_as_num){
    return(inverse_onehot(decentered_anon, names, sample))
  }

  # Return the decentered and anonymized data
  return(decentered_anon)
}

#---------------------------------------

#' Anonymize a data frame using spectral decomposition.
#'
#' This function takes a data frame as input and performs spectral decomposition
#' to anonymize the data by altering its left singular vectors. The anonymization
#' is done by applying a user-defined anonymizer function to vectors.
#' The function then reconstructs the decentered, anonymized data
#' and returns it as a new data frame.
#'
#' @param data A data frame to be anonymized and decentered.
#'
#' @param anonymizer A function used to anonymize the U matrix from singluar
#'                    value decomposition. The function should
#'                   take a data frame as input and return an anonymized data frame.
#'                   For example, the anonymizer function could perform a random
#'                   permutation of rows to achieve anonymization.
#'
#' @param sample Logical: Whether to sample from the numerical columns
#'                or take the max
#'
#' @param cat_as_num Logical: Whether categorical variables should be returned
#'                    as numerical or not
#'
#' @param on_matrices A character string indicating which matrix to anonymize.
#'                    Possible values are "U", "V", "UD", and "DV".
#'                    The default is "U". Note that the transposition is not
#'                    shown here on V for ease of parametrization, but the
#'                    modifications will be made on it's transpose.
#'
#' @param approx Logical: Whether to use the truncated SVD (default) or the full SVD.
#'
#' @return A new data frame containing the anonymized data.
#'
#' @importFrom onehot onehot
#'
#' @export
spectral = function(data, anonymizer, on_matrices = "U", approx = TRUE, sample = FALSE, cat_as_num = FALSE){

  # One-hot encode the data using the 'onehot' package
  oh = onehot(data, stringsAsFactors = TRUE, max_levels = Inf)

  # Initialize an empty vector to store the names of factor columns
  names = c()

  # Identify and store the names of factor columns
  for(item in oh){
    if(item$type == "factor") names = append(names, item$name)
  }

  # Perform the one-hot encoding of the data using the trained model
  encoded = predict(oh, data)

  # Convert categorical columns to {-1, 1} encoding
  for(name in names){
    cols = startsWith(colnames(encoded), name)
    workingset = encoded[,cols]
    withnegatives = t(apply(workingset, 1, function(x) ifelse(x == 0, -1, 1)))
    encoded[,cols] = withnegatives
  }

  # Store the {-1, 1} encoded data as 'onehot'
  onehot = encoded

  # Make a new empty dataframe of the original size and column names to store the centered data
  centered = emptydf(onehot)

  # Calculate the column means for centering the data
  means = sapply(onehot, mean)

  # Center the data by subtracting the column means from each column
  for(i in seq(ncol(onehot))){
    centered[,i] = onehot[,i] - means[i]
  }

  # Perform Singular Value Decomposition (SVD) on the centered data, M = UDV'
  if(approx){
    svd = svd(centered)
  } else {
    svd = svd(centered, nu = nrow(centered), nv = ncol(centered))
  }

  # Create a diagonal matrix from the singular values vector
  # d_matrix = diag(svd$d, nrow = ncol(svd$u), ncol = ncol(svd$v))

  print(d_matrix)

  # Anonymize the specified matrix from the SVD using the provided anonymization function
  if(on_matrices == "U"){
    u_anon = anonymizer(as.data.frame(svd$u))
    udv_anon = as.matrix(u_anon) %*% d_matrix %*% t(svd$v)
  } else if(on_matrices == "V"){
    v_anon = anonymizer(as.data.frame(svd$v))
    udv_anon = svd$u %*% d_matrix %*% as.matrix(t(v_anon))
  } else if(on_matrices == "UD"){
    ud_anon = anonymizer(as.data.frame(svd$u %*% d_matrix))
    udv_anon = as.matrix(ud_anon) %*% t(svd$v)
  } else if(on_matrices == "DV"){
    dv_anon = anonymizer(as.data.frame(d_matrix %*% t(svd$v)))
    udv_anon = svd$u %*% as.matrix(dv_anon)
  } else {
    stop("Invalid value for 'on_matrices' parameter.")
  }

  # Reconstruct the anonymized centered data using UDV' form
  data_anon = as.data.frame(udv_anon)
  colnames(data_anon) = colnames(data)

  # Make a new empty dataframe of the original size and column names to store the decentered, anonymized data
  decentered_anon = emptydf(onehot)

  # Add the column means back to the anonymized data to decenter it
  for(i in seq(ncol(onehot))){
    decentered_anon[,i] = data_anon[,i] + means[i]
  }

  # Normalize the factor columns using normalized inverse logit
  for(name in names){
    cols = startsWith(colnames(decentered_anon), name)
    workingset = decentered_anon[,cols]
    normalized = t(apply(workingset, 1, function(x)  (exp(x) / (1 + exp(x)))/sum(exp(x) / (1 + exp(x))) ))
    decentered_anon[,cols] = normalized
  }

  # Return the categorical values as categorical if wanted, using sampling if TRUE
  if(!cat_as_num){
    return(inverse_onehot(decentered_anon, names, sample))
  }

  # Return the decentered and anonymized data
  return(decentered_anon)
}

#---------------------------------------

#' Anonymize a data frame using spectral decomposition.
#'
#' This function takes a data frame as input and performs spectral decomposition
#' to anonymize the data by altering its left singular vectors. The anonymization
#' is done by applying a user-defined anonymizer function to vectors.
#' The function then reconstructs the decentered, anonymized data
#' and returns it as a new data frame.
#'
#' @param data A data frame to be anonymized and decentered.
#'
#' @param anonymizer A function used to anonymize the U matrix from singluar
#'                    value decomposition. The function should
#'                   take a data frame as input and return an anonymized data frame.
#'                   For example, the anonymizer function could perform a random
#'                   permutation of rows to achieve anonymization.
#'
#' @param sample Logical: Whether to sample from the numerical columns
#'                or take the max
#'
#' @param cat_as_num Logical: Whether categorical variables should be returned
#'                    as numerical or not
#'
#' @param on_matrices A character string indicating which matrix to anonymize.
#'                    Possible values are "U", "D", "V", "UD", and "DV".
#'                    The default is "U".
#'
#' @return A new data frame containing the anonymized data.
#'
#' @importFrom onehot onehot
#'
spectral_short = function(data, anonymizer, on_matrices = "U", sample = FALSE, cat_as_num = FALSE){

  # One-hot encode the data using the 'onehot' package
  oh = predict(onehot(data, stringsAsFactors = TRUE, max_levels = Inf), data)

  # Identify and store the names of factor columns
  names = colnames(data)[sapply(data, is.factor)]

  # Convert categorical columns to {-1, 1} encoding
  oh[, names] = 2 * as.matrix(oh[, names]) - 1

  # Center the data by subtracting the column means from each column
  centered = scale(oh, center = TRUE, scale = FALSE)

  # Perform Singular Value Decomposition (SVD) on the centered data, M = UDV'
  svd = svd(centered)

  # Anonymize the specified matrix from the SVD using the provided anonymization function
  matrices = list(U = svd$u, D = diag(svd$d), V = svd$v, UD = svd$u %*% diag(svd$d), DV = diag(svd$d) %*% t(svd$v))
  matrices[[on_matrices]] = anonymizer(as.data.frame(matrices[[on_matrices]]))
  udv_anon = Reduce("%*%", matrices)

  # Reconstruct the anonymized centered data using UDV' form
  data_anon = as.data.frame(udv_anon)
  colnames(data_anon) = colnames(data)

  # Add the column means back to the anonymized data to decenter it
  decentered_anon = scale(data_anon, center = means(oh), scale = FALSE)

  # Normalize the factor columns using softmax
  decentered_anon[, names] = t(apply(decentered_anon[, names], 1, function(x) softmax(x)))

  # Return the categorical values as categorical if wanted, using sampling if TRUE
  if(!cat_as_num){
    return(inverse_onehot(decentered_anon, names, sample))
  }

  # Return the decentered and anonymized data
  return(decentered_anon)
}

#---------------------------------------

onehot_helper = function(data){
  oh = onehot::onehot(data, stringsAsFactors = TRUE, max_levels = Inf)

  names = c()

  for(item in oh){
    if(item$type == "factor") names = append(names, item$name)
  }

  encoded = predict(oh, data)

  for(name in names){
    cols = startsWith(colnames(encoded), name)
    workingset = encoded[,cols]
    #print(workingset)
    withnegatives = (t(apply(workingset, 1, function(x) ifelse(x == 0, -1, 1))))
    #print(withnegatives)
    encoded[,cols] = withnegatives
  }

  return(encoded)
}





