#' Min-Max scaling of a numeric vector.
#'
#' This function performs Min-Max scaling on a numeric vector (column), scaling
#' its values to a range between 0 and 1. Min-Max scaling is a common technique
#' used to transform data to a specific range, preserving the relative differences
#' between values.
#'
#' @param column A numeric vector to be scaled.
#'
#' @return A scaled numeric vector with values in the range [0, 1].
#'
#' @examples
#' column <- c(10, 20, 30, 40)
#' scaled_column <- minmax_scaler(column)
#' scaled_column
#' @export
minmax_scaler = function(column){
  # get the minimum and maximum values of the column
  min = min(column)
  max = max(column)
  # scale the column
  column = (column - min)/(max - min)
  return(column)
}

#--------------------------------------

#' Create an empty data frame with the same dimensions and column names as the input data.
#'
#' This function takes a data frame as input and returns a new empty data frame
#' with the same dimensions and column names as the input data. The returned data frame
#' will have no data but can be populated later with relevant information.
#'
#' @param data A data frame to replicate the structure from.
#'
#' @return An empty data frame with the same dimensions and column names as \code{data}.
#'
#' @examples
#' data <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' empty_data <- emptydf(data)
#' empty_data
#' @export
emptydf = function(data){
  new = as.data.frame(matrix(nrow = nrow(data), ncol = ncol(data)))
  colnames(new) = colnames(data)
  return(new)
}

#----------------------------------------

#' Shuffle the values in each column of a data frame.
#'
#' This function takes a data frame as input and returns a new data frame with
#' the same dimensions as the original data. The values in each column of the
#' returned data frame are shuffled randomly using the \code{sample} function,
#' effectively creating a random permutation of each column.
#'
#' @param data A data frame to be shuffled.
#'
#' @return A new data frame with the same dimensions as data, but with the
#'         values in each column randomly shuffled.
#'
#' @examples
#' data <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' shuffled_data <- shuffle(data)
#' shuffled_data
#' @export
shuffle = function(data){
  # return a random permutation of each column of the data
  for(i in seq(ncol(data))){
    data[,i] = sample(data[,i])
  }

  return(data)

}

#---------------------------

#' Generate random numbers from a Laplace distribution.
#'
#' This function generates random numbers from a Laplace distribution
#' with given location and scale parameters.
#'
#' @param n The number of random samples to generate.
#' @param location The location parameter (mean) of the Laplace distribution.
#' @param scale The scale parameter (spread) of the Laplace distribution.
#'
#' @return A numeric vector containing \code{n} random numbers following
#' the Laplace distribution.
#'
#' @importFrom stats runif
#'
#' @examples
#' rlaplace(10, 0, 1)
#'
#' @export
rlaplace <- function(n, location, scale) {
  u <- runif(n, min = 0, max = 1) # Generate n uniform random numbers
  laplace_numbers <- location - scale * sign(u - 0.5) * log(1 - 2 * abs(u - 0.5))
  return(laplace_numbers)
}

#-------------------------------------------------------

# inverse_onehot = function(data, names){
#   # the inverse operation to one hot encoding, i.e. from one hot encoded data to the original data
#   # names: the names of the categorical variables in the original data
#
#   # iterate over the names of the categorical variables and combine the one hot encoded columns into one column
#   for(name in names){
#     # get the columns that are one hot encoded
#     one_hot_columns = grep(name, names(data))
#
#     # get the names of the grepped columns
#     one_hot_columns_names = names(data)[one_hot_columns]
#
#     # remove the first part of the column names, as in the one hot encoding the column names are of the form "name=level". We only need the level part
#     one_hot_columns_names = gsub(paste0(name, "="), "", one_hot_columns_names)
#
#
#     print(one_hot_columns_names)
#
#
#     # combine the columns such that the value of the combined column is the column which has the highest value
#     data[, name] = apply(data[, one_hot_columns], 1, function(x) paste(which(x == max(x))))
#
#     # data[, name] = apply(data[, one_hot_columns], 1, function(x) paste(which(x == 1)))
#     # remove the one hot encoded columns
#     data = data[, -one_hot_columns]
#   }
#
#   return(data)
#
# }

#-------------------------------------------------------

#' Inverse One-Hot Encoding
#'
#' Perform the inverse operation to one-hot encoding, i.e.,
#'  from one-hot encoded data to the original data.
#'
#' @param data The one-hot encoded dataset.
#' @param names The names of the categorical variables in the original data.
#'                Names in the original are assumed to be mapped to "name=level"
#'                in the one hot encoded data given. Works well with
#'                "onehot" package.
#' @param sample Logical: Whether to sample from the numerical columns or take the max
#' @return The original data with categorical variables restored from one-hot encoding.
#'
#' @export
inverse_onehot = function(data, names, sample=FALSE){
  # the inverse operation to one hot encoding, i.e. from one hot encoded data to the original data
  # names: the names of the categorical variables in the original data
  # sample: if TRUE, the value will be sampled from the one_hot_columns instead of choosing the maximum value

  # iterate over the names of the categorical variables and combine the one hot encoded columns into one column
  for(name in names){
    # get the columns that are one hot encoded
    one_hot_columns = grep(name, names(data))

    # get the names of the grepped columns
    one_hot_columns_names = names(data)[one_hot_columns]

    # remove the first part of the column names, as in the one hot encoding the column names are of the form "name=level". We only need the level part
    one_hot_columns_names = gsub(paste0(name, "="), "", one_hot_columns_names)

    # combine the columns such that the value of the combined column is either the column with the maximum value or sampled using softmax probabilities
    if (sample) {
      combined_column = apply(data[, one_hot_columns], 1, function(x) {
        sample(one_hot_columns_names, 1, prob = x)
      })
    } else {
      combined_column = apply(data[, one_hot_columns], 1, function(x) {
        one_hot_columns_names[which(x == max(x))]
      })
    }

    # remove the one hot encoded columns
    data = data[, -one_hot_columns]

    # replace the numerical column with the combined categorical column
    data[, name] = combined_column
  }

  return(data)
}


#-------------------------------------------------------

#' Inverse Logit Function
#'
#' Calculate the inverse logit function.
#'
#' @param x The input value.
#' @return The result of the inverse logit function.
#'
#' @examples
#' inverse_logit(0)
#' inverse_logit(-2)
#' inverse_logit(2)
#' @export
inverse_logit = function(x){
  # inverse logit function
  return(exp(x) / (1 + exp(x)))
}
