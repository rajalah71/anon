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
#' standard deviation 1. If reference_data is NULL, calculate the distance between each row
#' of the \code{original_data} against the rest of the \code{original_data}. Otherwise
#' the function calculates the distance between
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
  # if reference_data is null, the distance is calculated by leaving one row out of the original data and calculating the distance between the left out row and the rest of the original data

  if(is.null(reference_data)){
    sorted_distances = as.data.frame(matrix(NA, nrow(original_data), nrow(original_data)-1))
    for(i in seq(nrow(original_data))){
      distances = c()
      for(j in seq(nrow(original_data))){
        if(j != i){
          distances = append(distances, dist(original_data[i,], original_data[j,]))
        }
      }
      sorted_distances[i,] = (distances)
    }
  } else {
    sorted_distances = as.data.frame(matrix(NA, nrow(original_data), nrow(reference_data)))
    for(i in seq(nrow(original_data))){
      distances = rep_len(NA, nrow(original_data))
      for(j in seq(nrow(reference_data))){
        distances[j] = dist(original_data[i,], reference_data[j,])
      }
      sorted_distances[i,] = (distances)
    }
  }
  return(sorted_distances)
}

#-----------------------------------------------------------------

#' Prediction Distance
#'
#' Calculate distances between each row of the original_data and the reference_data using the specified distance function.
#'
#' @param original_data The original dataset.
#' @param reference_data The reference dataset (default: NULL).
#' @param dist Distance function to use (default: euc_dist).
#' @return A numeric vector containing the distances between each row of the original_data and the reference_data.
#' @export
prediction_distance = function(original_data, reference_data = NULL, dist = euc_dist){
  # calculate distances
  distances = distances(original_data, reference_data, dist)

  # sort each row in ascending order
  distances = t(apply(distances, 1, sort))

  # return the first column of the distances
  return(distances[,1])
}



#-----------------------------------------------------------------

#' Prediction Ambiguity
#'
#' Calculates the prediction ambiguity by comparing the relative distance from each record to its nearest neighbor versus its kth nearest neighbor in the set of distances.
#'
#' @param original_data The original dataset for which prediction ambiguity needs to be calculated.
#' @param k The number of nearest neighbors to consider for calculating the ambiguity.
#' @param reference_data (Optional) The reference dataset used for calculating the distances. If not provided, the distances will be computed within the original_data.
#' @param dist The distance function to be used. Default is euclidean distance (euc_dist).
#'
#' @return A numeric vector representing the prediction ambiguity for each record in the original_data.
#'
#' @examples
#' prediction_ambiguity(original_data = iris, k = 3)
#'
#'
#' @export
prediction_ambiguity = function(original_data, k, reference_data = NULL, dist = euc_dist){
  # Prediction ambiguity gives the relative distance from the record Aj to the nearest versus the kth-nearest record in the set distances
  distances = distances(original_data, reference_data, dist)

  # sort each row in ascending order
  distances = t(apply(distances, 1, sort))

  # return the first column of the distances divided by the kth column of the distances
  return(distances[,1]/distances[,k])
}


#-----------------------------------------------------------------

#' Prediction Uncertainty
#'
#' Prediction uncertainty gives the variation among the k best matches for each row of the original data in the reference data for each row of the original data, find the k best matches in the reference data using the dist function. If the reference data is null, the function performs leave-one-out cross-validation to calculate the distance between each row of the original data against the rest of the original data. Onehot encode the datasets to enable distance calculations. Only run the numeric version if it is not null.
#'
#' @param original_data The original dataset.
#' @param reference_data The reference dataset of the same column space (default: NULL).
#' @param k Number of best matches to find.
#' @param dist Distance function to use (default: euc_dist).
#' @return A numeric vector containing the variances for each row of the original_data.
#'
#' @examples
#' prediction_uncertainty(original_data = iris, k = 3)
#'
#' @importFrom onehot onehot
#' @export
prediction_uncertainty = function(original_data, k, reference_data = NULL, dist = euc_dist){
  # Prediction uncertainty gives the variation among the k best matches for each row of the original data in the reference data
  # for each row of the original data, find the k best matches in the reference data using the dist function

  # calculate distances between each row of the original data and the reference data using the specified distance function
  distances = distances(original_data, reference_data, dist)

  # onehot encode and scale both datasets
  original_data = predict(onehot(original_data, stringsAsFactors = TRUE, max_levels = Inf), original_data)
  if(!is.null(reference_data)) reference_data = predict(onehot(reference_data, stringsAsFactors = TRUE, max_levels = Inf), reference_data)
  original_data = scale(original_data)
  if(!is.null(reference_data)) reference_data = scale(reference_data)

  # Empty vector to store variances
  variances = rep_len(NA, nrow(original_data)-1)

  # if reference data is null, operate on the original data
  if(is.null(reference_data)){
    # iterate over each row of the original data
    for(i in seq(nrow(original_data))){

      # find the indices of the lowest k distances in the ith row of the distances dataset
      k_best_matches = order(as.matrix(distances[i,]))[1:k]

      # exclude the ith row from the original_data
      original_data_without_i = original_data[-i,]

      # pick the k best matches from the original_data_without_i
      k_best_matches_rows = original_data_without_i[k_best_matches,]

      # calculate the mean variance of the k best matches rows
      variances[i] = mean(apply(k_best_matches_rows, 2, var))
    }
  } else{
    # iterate over each row of the original data
    for(i in seq(nrow(original_data))){

      # find the indices of the lowest k distances in the ith row of the distances dataset
      k_best_matches = order(as.matrix(distances[i,]))[1:k]

      # pick the k best matches from the reference_data
      k_best_matches_rows = reference_data[k_best_matches,]

      # calculate the mean variance of the k best matches rows
      variances[i] = mean(apply(k_best_matches_rows, 2, var))
    }
  }

  # return the variances
  return(variances)

}


#-----------------------------------------------------------------


#' Prediction Uncertainty
#'
#' Prediction uncertainty gives the variation among the k best matches for each row of the original data in the reference data for each row of the original data, find the k best matches in the reference data using the dist function. If the reference data is null, the function performs leave-one-out cross-validation to calculate the distance between each row of the original data against the rest of the original data. Onehot encode the datasets to enable distance calculations. Only run the numeric version if it is not null.
#'
#' @param original_data The original dataset.
#' @param reference_data The reference dataset of the same column space (default: NULL).
#' @param k Number of best matches to find.
#' @param dist Distance function to use (default: euc_dist).
#' @return A numeric vector containing the variances for each row of the original_data.
#'
#' @examples
#' prediction_uncertainty(original_data = iris, k = 3)
#'
#' @importFrom onehot onehot
prediction_uncertainty_legacy = function(original_data, k, reference_data = NULL, dist = euc_dist){
  # Prediction uncertainty gives the variation among the k best matches for each row of the original data in the reference data
  # for each row of the original data, find the k best matches in the reference data using the dist function

  # If the reference data is null, the function performs leave-one-out cross-validation to calculate the distance between each row of the original data against the rest of the original data

  # onehot encode the datasets to enable distance calculations
  original_data = predict(onehot(original_data, stringsAsFactors = TRUE, max_levels = Inf), original_data)
  cat("pass\n")
  if(!is.null(reference_data)) reference_data = predict(onehot(reference_data, stringsAsFactors = TRUE, max_levels = Inf), reference_data)

  # scale the datasets to have mean 0 and standard deviation 1
  original_data = scale(original_data)
  if(!is.null(reference_data)) reference_data = scale(reference_data)

  # empty vector to store the variances
  variances = rep_len(NA, nrow(original_data))

  cat("pass2\n")

  # find the k nearest rows in the reference data for each row of the original data. If the reference data is null, the function performs leave-one-out cross-validation to calculate the distance between each row of the original data against the rest of the original data
  if(!is.null(reference_data)){
    for(i in seq(nrow(original_data))){
      k_nearest = as.data.frame(matrix(NA, k, ncol(original_data)))

      # add rows of the reference data to the k_nearest dataframe if they are closer than the current furthest row
      for(j in seq(nrow(reference_data))){
        if(j <= k){
          k_nearest[j,] = reference_data[j,]
        } else {
          if(dist(original_data[i,], reference_data[j,]) < max(distances(k_nearest, original_data[i,], dist = dist))){
            k_nearest[which.max(distances(k_nearest, original_data[i,], dist = dist)),] = reference_data[j,]
          }
        }
      }

      # calculate the variance of the columns of the k nearest rows and take the mean of them
      variances[i] = mean(apply(k_nearest, 2, var))

    }
  } else{
    for(i in seq(nrow(original_data))){
      k_nearest = as.data.frame(matrix(NA, k, ncol(original_data)))

      # add rows of the reference data to the k_nearest dataframe if they are closer than the current furthest row
      for(j in seq(nrow(original_data))){
        # skip the row if it is the same as the row of the original data
        if(j == i) next

        if(anyNA(k_nearest)){
          k_nearest[j,] = original_data[j,]
        } else {
          if(dist(original_data[i,], original_data[j,]) < max(distances(k_nearest, original_data[i,], dist = dist))){
            k_nearest[which.max(distances(k_nearest, original_data[i,], dist = dist)),] = original_data[j,]
          }
        }
      }

      # calculate the variance of the columns of the k nearest rows and take the mean of them
      variances[i] = mean(apply(k_nearest, 2, var))

    }
  }

  return(variances)

}


