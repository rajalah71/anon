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
