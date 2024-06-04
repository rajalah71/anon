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
emptydf = function(data){
  new = as.data.frame(matrix(nrow = nrow(data), ncol = ncol(data)))
  colnames(new) = colnames(data)
  return(new)
}

#----------------------------------------

#' Shuffle the rows of a data frame.
#'
#' This function takes a data frame as input and returns a new data frame with
#' the same dimensions as the original data. The rows of the
#' returned data frame are shuffled randomly using the \code{sample} function,
#' effectively re-ordering the dataframe, as in shuffling a deck of cards.
#'
#' @param data A data frame to be shuffled.
#'
#' @return A new data frame with the same dimensions as data, but with the
#'         rows shuffled.
#'
#' @examples
#' data <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' shuffled_data <- shuffle(data)
#' shuffled_data
#' @export
shuffle = function(data){

  data = data[sample(nrow(data)),]
  rownames(data) = seq(nrow(data))

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

  # order the columns in the original order using colmn reordered
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

#-------------------------------------------------------

#' Get the minimum and maximum values of a numeric vector
#'
#' This function takes a numeric vector as input and returns the minimum and maximum values.
#'
#' @param numbers A numeric vector.
#'
#' @return A character string representing the minimum and maximum values in the format [min, max].
#'
#' @examples
#' numbers <- c(1, 2, 3)
#' getMinMax(numbers)
#'
#' @export
getMinMax <- function(numbers) {
  result <- range(numbers)
  output <- paste0("[", result[1], ",", result[2], "]")
  return(output)
}

#-------------------------------------------------------


#' Combine the lowest classes
#'
#' This function iteratively combines the fewest occuring categorical values in a column (k) times.
#'
#' @param column The column in which to combine the lowest classes.
#' @param k The maximum number of fewest occurances to combine.
#'
#' @return The column with the categorical values combined.
#'
#' @examples
#' column <- c("A", "B", "A", "C", "B", "D", "A", "C", "B")
#' combine_lowest_classes(column, 2)
#' @export
combine_lowest_classes <- function(column, k=1) {

  # Find the unique values in the column and raise error if k is too large
  unique_vals <- unique(column)
  if(k+1 > length(unique_vals)){
    return(column)
  }

  for (i in seq_len(k)) {

    # Count the number of occurrences of each unique value
    counts <- table(column)

    # Sort the counts in ascending order
    sorted_counts <- sort(counts)

    # Find the two lowest classes
    lowest_class1 <- names(sorted_counts)[1]
    lowest_class2 <- names(sorted_counts)[2]

    # Combine the two lowest classes
    new_class <- paste(lowest_class1, "or", lowest_class2)

    # Update the column with the new class
    column[column == lowest_class1 | column == lowest_class2] <- new_class

    # Update the counts with the new class
    counts[new_class] <- counts[lowest_class1] + counts[lowest_class2]

  }

  return(column)
}

#-------------------------------------------------------

#' Column Inserter
#'
#' Insert a column into a data frame at a specified index.
#'
#' @param data The data frame to insert the column into.
#' @param column The column to insert.
#' @param colname The name of the column to insert.
#' @param index The index at which to insert the column.
#' @return The data frame with the new column inserted at the specified index.
#'
column_inserter = function(data, column, colname, index){
  # insert a column into a data frame at a specified index
  # data: the data frame to insert the column into
  # column: the column to insert
  # colname: the name of the column to insert
  # index: the index at which to insert the column

  # get the column names of the data frame
  colnames = names(data)

  # insert the column name at the specified index
  colnames = c(colnames[1:index-1], colname, colnames[(index):length(colnames)])

  # insert the column at the specified index
  data = cbind(data[, 1:(index - 1)], column, data[, index:length(colnames(data))])

  # set the column names
  names(data) = colnames

  return(data)
}

#-------------------------------------------------------

#' Column Reordered
#'
#' Reorder the data frame columns according to the specified column names and indices.
#'
#' @param data The data frame to reorder.
#' @param names The column names in the desired order.
#' @param indicies The target indices for the corresponding column names.
#' @return The data frame with columns reordered as specified.
#'
column_reordered = function(data, names, indicies){
  # reorder the data frame so that names_location[i] column will be at index indicies[i] for all i
  for(i in seq_along(names)){
    # get the location of the column
    location = match(names[i], names(data))

    # if the location is not the same as the index, reorder the data frame
    if(location != indicies[i]){
      tobereplaced = data[, indicies[i]]
      data[, indicies[i]] = data[, location]
      data[, location] = tobereplaced
    }
  }

 return(data)

}

#------------------------------------------------------

#' Row Checker
#'
#' Check whether any row in the reference data is in the original data. If so, stop and raise an error.
#' Could be used to detect failure to anonymize data in some methods.
#'
#' @param original_data The original dataset.
#' @param reference_data The reference dataset.
#'
row_checker = function(original_data, reference_data){
  # check whether any row in the reference data are in the original data, is so stop
  for(i in seq(nrow(reference_data))){
    # checke whether the row is in the original data
    if(any(apply(original_data, 1, function(x) all(x == reference_data[i,])))){
      stop("An exact match was produced with the method. Try again (if random) or try another method alltogether.")
    }

  }
}

#------------------------------------------------------

#' Most Common Value
#'
#' Find the most common value in a column.
#'
#' @param column The input column.
#' @return A vector with the most common value repeated for the length of the input column.
#'
#' @examples
#' data <- c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4)
#' most_common(data)
#' @export
most_common = function(column){

  common = table(column)
  common = names(common)[which.max(common)]
  repeated = rep_len(common, length(column))
  return(repeated)

}

# --------------------------------------------------

#' Reorder Rows Based on Row Names
#'
#' @param df The input dataframe.
#' @return A dataframe with rows reordered based on the extracted row numbers.
#'
#'
reorder_rownames <- function(df) {
  # Extract the numbers from rownames and convert them to integers
  row_numbers <- as.integer(sub(".*\\.(\\d+)$", "\\1", rownames(df)))

  # Order the dataframe based on the extracted numbers
  df <- df[order(row_numbers), ]

  # Remove the rownames
  rownames(df) <- NULL

  return(df)
}

#-----------------------------------------------------

#' Get Mode
#'
#' Find the mode (most frequent value) in a vector.
#'
#' @param x The input vector.
#' @return The mode of the input vector.
#'
#' @examples
#' data <- c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4)
#' get_mode(data)
#'
#' @export
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

#-----------------------------------------------------

#' Sensitive Column Generalizer
#'
#' Divide a sensitive column into subsets of a specified size or into a number of subsets and generalize each subset.
#'
#' @param sensitiveColumn The input sensitive column.
#' @param subsetSize The size of subsets to create.
#' @param subsets The number of subsets to create.
#' @return A generalized version of the sensitive column with subsets replaced by their means (for numeric columns) or modes (for non-numeric columns).
#'
#' @examples
#' numeric_data <- c(1, 2, 3, 4, 5, 6)
#' sensitive_generalizer(numeric_data, subsetSize = 2)
#'
#' categorical_data <- c("A", "B", "B", "C", "C", "D")
#' sensitive_generalizer(categorical_data, subsetSize = 2)
#'
#'
#' @export
sensitive_generalizer = function(sensitiveColumn, subsetSize = NULL, subsets = NULL){

  # only one parameter is allowed as non null
  if(!is.null(subsetSize) && !is.null(subsets)) stop("Choose either individual subsetSize or the number of subsets.")

  # Divide the column into subsets to sizes of "subsetSize"

  if(!is.null(subsetSize)){
      # If numerical, store the original order, sort, divide into subsets of size "subsetSize" and then replace every value in every subset with its mean
    if(is.numeric(sensitiveColumn)){
      originalOrder = order(sensitiveColumn)
      sortedColumn = sort(sensitiveColumn)
      subsets = split(sortedColumn, ceiling(seq_along(sortedColumn)/subsetSize))
      for(i in 1:length(subsets)){
        subset_mean = mean(subsets[[i]])
        subsets[[i]] = rep(subset_mean, length(subsets[[i]]))
      }
      sortedColumn = unlist(subsets)
      sortedColumn = sortedColumn[order(originalOrder)]
      return(sortedColumn)
    }

    # If not numerical, store the original order, sort, divide into subsets of size "subsetSize" and then replace every value in every subset with its mode (function called "get_mode")
    else{
      originalOrder = order(sensitiveColumn)
      sortedColumn = sort(sensitiveColumn)
      subsets = split(sortedColumn, ceiling(seq_along(sortedColumn)/subsetSize))
      for(i in 1:length(subsets)){
        subset_mode = get_mode(subsets[[i]])
        subsets[[i]] = rep(subset_mode, length(subsets[[i]]))
      }
      sortedColumn = unlist(subsets)
      sortedColumn = sortedColumn[order(originalOrder)]
      return(sortedColumn)
    }
  }

  if(!is.null(subsets)){

    # Calculate subsetsize
    subsetSize = ceiling(length(sensitiveColumn) / subsets)

    # If numerical, store the original order, sort, divide into subsets of size "subsetSize" and then replace every value in every subset with its mean
    if(is.numeric(sensitiveColumn)){
      originalOrder = order(sensitiveColumn)
      sortedColumn = sort(sensitiveColumn)
      subsets = split(sortedColumn, ceiling(seq_along(sortedColumn)/subsetSize))
      for(i in 1:length(subsets)){
        subset_mean = mean(subsets[[i]])
        subsets[[i]] = rep(subset_mean, length(subsets[[i]]))
      }
      sortedColumn = unlist(subsets)
      sortedColumn = sortedColumn[order(originalOrder)]
      return(sortedColumn)
    }

    # If not numerical, store the original order, sort, divide into subsets of size "subsetSize" and then replace every value in every subset with its mode (function called "get_mode")
    else{
      originalOrder = order(sensitiveColumn)
      sortedColumn = sort(sensitiveColumn)
      subsets = split(sortedColumn, ceiling(seq_along(sortedColumn)/subsetSize))
      for(i in 1:length(subsets)){
        subset_mode = get_mode(subsets[[i]])
        subsets[[i]] = rep(subset_mode, length(subsets[[i]]))
      }
      sortedColumn = unlist(subsets)
      sortedColumn = sortedColumn[order(originalOrder)]
      return(sortedColumn)
    }

  }

}






#colVars-------------------------------------------------

#' Calculate Column Variances
#'
#' Calculate the variances of columns in a data frame.
#'
#' @param df A data frame.
#' @param na.rm Logical. Should missing values (NA) be removed?
#'
#' @return A numeric vector of variances for each column.
#'
#' @examples
#' \dontrun{
#' variances <- colVars(data_frame)
#' }
#' @export
colVars <- function(df, na.rm = FALSE) {
  apply(df, 2, var, na.rm = na.rm)
}

#' Calculate Column standard deviations
#'
#' Calculate the standard deviations of columns in a data frame.
#'
#' @param df A data frame.
#' @param na.rm Logical. Should missing values (NA) be removed?
#'
#' @return A numeric vector of standard deviations for each column.
#'
#' @examples
#' \dontrun{
#' variances <- ColSds(data_frame)
#' }
#' @export
colSds <- function(df, na.rm = FALSE) {
  apply(df, 2, sd, na.rm = na.rm)
}

#sig_num-----------------------

#' Significant Digits
#' @param x A numeric vector.
#' @param n The number of significant digits to round to.
#' @return A numeric vector rounded to n significant digits.
#'
#' @examples
#' \dontrun{
#' a = 3.141
#' sig_num(a, 2)
#' }
#' @export
signum = function(x,n){
  # Stop if n is not larger of equal to 1
  if(n < 1){
    stop("n must be larger or equal to 1")
  }

 num = formatC(signif(x, digits=n), digits=n, format="fg", flag="#")
 # remove point if n=1
 ifelse(n==1, gsub("\\.", "", num), num)
}

#euc_dist-----------------------

#' Calculate the Euclidean distance between two vectors normalized by vector length.
#'
#' This function calculates the Euclidean distance between two vectors \code{x} and \code{y},
#' which are normalized by the length of the vectors. The Euclidean distance is a measure
#' of the straight-line distance between two points in a space. Normalizing the distance
#' by the vector length ensures that the magnitude of the distance is independent of the
#' vector length, allowing for fair comparisons between vectors of different lengths.
#'
#' @param x A numeric vector.
#'
#' @param y A numeric vector of the same length as \code{x}.
#'
#' @return The Euclidean distance between \code{x} and \code{y} normalized by vector length.
#'
#' @examples
#' x <- c(1, 2, 3)
#' y <- c(4, 5, 6)
#' distance <- euc_dist(x, y)
#' distance
#'
#' @export
euc_dist = function(x,y){
  # calculate the distance between two vectors nomalized by the length of the vectors
  return(sqrt(sum((x-y)^2)/length(x)))
}

