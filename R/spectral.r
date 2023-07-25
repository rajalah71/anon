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

#' Anonymize and decenter a data frame using spectral decomposition.
#'
#' This function takes a data frame as input and performs spectral decomposition
#' to anonymize the data by altering its principal components. The anonymization
#' is done by applying a user-defined anonymizer function to the principal
#' components. The function then reconstructs the decentered, anonymized data
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
#' @return A new data frame of the same size as data, containing the
#'         decentered and anonymized data.
#' @export
spectral = function(data, anonymizer){

  # Make a new empty dataframe of original size and column names to store the centered data
  centered = as.data.frame(matrix(nrow = nrow(data), ncol = ncol(data)))
  colnames(centered) = colnames(data)

  # Get the colmneans
  means = sapply(data, mean)

  # Subtract the colmeans from the data
  for(i in seq(ncol(data))){
    centered[,i] = data[,i] - means[i]
  }

  # Get the singular value decomposition of the centered data, M = UDV'
  svd = svd(centered)

  # Anonymize U with the given function
  u_anon = anonymizer(as.data.frame(svd$u))

  # Recombine the anonymized centered data
  data_anon = as.data.frame(as.matrix(u_anon) %*% diag(svd$d) %*% t(svd$v))
  colnames(data_anon) = colnames(data)

  # Make a new empty dataframe of original size and column names to store the decentered, anonymized data
  decentered_anon = as.data.frame(matrix(nrow = nrow(data), ncol = ncol(data)))
  colnames(decentered_anon) = colnames(data)

  # Add the colmeans back to the anonymized data
  for(i in seq(ncol(data))){
    decentered_anon[,i] = data_anon[,i] + means[i]
  }

  return(decentered_anon)
}

# fun_list <- list(
#   Sepal.Length = mean,
#   Sepal.Width = mean,
#   Petal.Length = mean,
#   Petal.Width = mean
# )
#
# anon::makeLdiverse(data2[1:100,], c("age", "zip_code", "gender"), "disease", fun_list_4, 2)
#
# fun = function(x){
#   anon::makeLdiverse(x, colnames(x)[-5], "Species", fun_list, 2)
# }
#
# spectral(iris, fun)
#
# anon::makeLdiverse(iris, colnames(iris)[-5], "Species", fun_list, 2)

# ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7, 0.3))
# train <- iris[ind==1,]
# test <- iris[ind==2,]
#
# anon = spectral(iris[,1:4], cell_swap)
# spectral_train = cbind(anon[ind==1,], iris[ind==1,5])
# colnames(spectral_train) = colnames(iris)
# spectral_test = cbind(anon[ind==2,], iris[ind==2,5])
# colnames(spectral_test) = colnames(iris)
#
# rf <- randomForest(as.factor(Species)~., data=train)
# mean(predict(rf, test) == test[,5])
#
# rf_anon = randomForest(as.factor(Species)~., data=spectral_train)
# mean(predict(rf_anon, spectral_test) == spectral_test[,5])

