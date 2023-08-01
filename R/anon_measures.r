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

#----------------------------------------------------------

#' Calculate the minimum distance between rows of two datasets.
#'
#' This function calculates the minimum distance between each row of the
#' \code{original_data} and every row of the \code{reference_data} using a specified
#' distance function (\code{dist}). The datasets are first one-hot encoded to enable
#' distance calculations. The function then scales the datasets to have mean 0 and
#' standard deviation 1. If \code{cross_val} is set to \code{TRUE}, the function
#' performs leave-one-out cross-validation to calculate the distance between each row
#' of the \code{original_data} against the rest of the \code{original_data}. If
#' \code{cross_val} is \code{FALSE}, the function calculates the distance between
#' each row of the \code{original_data} and every row of the \code{reference_data}.
#'
#' @param original_data A data frame representing the original dataset.
#'
#' @param reference_data A data frame representing the reference dataset.
#'
#' @param dist A function that calculates the distance between two rows of a data frame.
#'
#'
#' @return A numeric vector containing the minimum distances between rows of the
#'         \code{original_data} and \code{reference_data}.
#'
#' @importFrom onehot onehot
#' @export
distances = function(original_data, reference_data = NULL, dist = euc_dist){
  # onehot encode the datasets to enable distance calculations
  original_data = predict(onehot(original_data, stringsAsFactors = TRUE, max_levels = Inf), original_data)
  if(!is.null(reference_data)) reference_data = predict(onehot(reference_data, stringsAsFactors = TRUE, max_levels = Inf), reference_data)

  # check if the dataframes have the same number columns and stop if not
  # if(ncol(original_data) != ncol(reference_data)){
  #   stop("The dataframes do not have the same number of columns. Onehot encode them both first.")
  # }

  # scale the datasets to have mean 0 and standard deviation 1
  original_data = scale(original_data)
  if(!is.null(reference_data)) reference_data = scale(reference_data)

  # calculate the distance between each row of the original data against every row of the reference data and return the sorted for each row
  # if cross_val is TRUE, the distance is calculated by leaving one row out of the original data and calculating the distance between the left out row and the rest of the original data

  if(is.null(reference_data)){
    sorted_distances = as.data.frame(matrix(NA, nrow(original_data), nrow(original_data)-1))
    for(i in seq(nrow(original_data))){
      distances = c()
      for(j in seq(nrow(original_data))){
        if(j != i){
          distances = append(distances, dist(original_data[i,], original_data[j,]))
        }
      }
      sorted_distances[i,] = sort(distances)
    }
  } else {
    sorted_distances = as.data.frame(matrix(NA, nrow(original_data), nrow(reference_data)))
    for(i in seq(nrow(original_data))){
      distances = rep_len(NA, nrow(original_data))
      for(j in seq(nrow(reference_data))){
        distances[j] = dist(original_data[i,], reference_data[j,])
      }
      sorted_distances[i,] = sort(distances)
    }
  }
  return(sorted_distances)
}

#-----------------------------------------------------------------


prediction_distance = function(distances){
  # return the first column of the distances
  return(distances[,1])
}



#-----------------------------------------------------------------


prediction_ambiguity = function(distances, k){
  # Prediction ambiguity gives the relative distance from the record Aj to the nearest versus the kth-nearest record in the set distances
  # return the first column of the distances divided by the kth column of the distances
  return(distances[,1]/distances[,k])
}


#-----------------------------------------------------------------

prediction_ambiguity = function(distances, k){
  # Prediction ambiguity gives the relative distance from the record Aj to the nearest versus the kth-nearest record in the set distances
  # return the first column of the distances divided by the kth column of the distances
  return(distances[,1]/distances[,k])
}

prediction_uncertainty = function(original_data, reference_data, k, dist = euc_dist){
  # Prediction uncertainty gives the variation among the k best matches for each row of the original data in the reference data
  # for each row of the original data, find the k best matches in the reference data using the dist function

  # onehot encode the datasets to enable distance calculations
  numeric_original = predict(onehot(original_data, stringsAsFactors = TRUE, max_levels = Inf), original_data)
  numeric_reference = predict(onehot(reference_data, stringsAsFactors = TRUE, max_levels = Inf), reference_data)



  sorted_by_distance = emptydf()


}
