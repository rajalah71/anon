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

#' Calculate the distances between rows of two datasets.
#'
#' This function calculates the distances between each row of the
#' \code{original_data} and every row of the \code{reference_data} using a specified
#' distance function (\code{dist}). The datasets are first one-hot encoded to enable
#' distance calculations. The function then scales the datasets to have mean 0 and
#' standard deviation 1. If reference_data is NULL, calculate the distance between each row
#' of the \code{original_data} against the rest of the \code{original_data}. Otherwise
#' the function calculates the distance between
#' each row of the \code{original_data} against every row of the \code{reference_data}.
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

  # check if the dataframes have the same number columns and stop if not (only if reference_data != NULL)
  if(!is.null(reference_data)){
    if(ncol(original_data) != ncol(reference_data)){
      stop("The dataframes do not have the same number of columns.")
    }
  }

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
#' Prediction uncertainty gives the variation among the k best matches for each
#' row of the original data in the reference data for each row of the original
#' data, find the k best matches in the reference data using the dist function.
#' If the reference data is null, the function performs leave-one-out method to
#' calculate the distance between each row of the original data against the rest
#' nof the original data. Onehot encode and scale the datasets to
#' enable distance calculations.
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
#' @importFrom stats var
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

#' Prediction All Measures
#'
#' Wrapper function to calculate all the measures for reference and original data,
#' store them into a list in the order of prediction distance,
#' prediction ambiguity, and prediction uncertainty,
#' and return the list.
#'
#' @param original_data The original dataset.
#' @param k Number of kth-nearest records to consider.
#' @param reference_data The reference dataset.
#' @param dist Distance function to use (default: euc_dist).
#' @return A list containing two sub-lists:
#'   - original_list: Contains the measures for the original_data without a reference_data.
#'   - reference_list: Contains the measures for the original_data with the provided reference_data.
#'
#' @examples
#' original_data <- as.data.frame(matrix(1:6, ncol = 2))
#' reference_data <- as.data.frame(matrix(7:12, ncol = 2))
#' prediction_all(original_data, k = 2, reference_data)
#'
#' @export
prediction_all = function(original_data, k, reference_data, dist = euc_dist){
  # Calculate all the measures and store them in a list
  reference_list = list()
  reference_list$prediction_distance = prediction_distance(original_data, reference_data, dist)
  reference_list$prediction_ambiguity = prediction_ambiguity(original_data, k, reference_data, dist)
  reference_list$prediction_uncertainty = prediction_uncertainty(original_data, k, reference_data, dist)

  original_list = list()
  original_list$prediction_distance = prediction_distance(original_data, reference_data = NULL, dist)
  original_list$prediction_ambiguity = prediction_ambiguity(original_data, k, reference_data = NULL, dist)
  original_list$prediction_uncertainty = prediction_uncertainty(original_data, k, reference_data = NULL, dist)

  return(list(original = original_list, reference = reference_list))
}



#-----------------------------------------------------------------

#' Prediction Plot
#'
#' Plot the measures of predictive disclosure risk in an nonoverlapping sample to the general populace against the measures of the same type in the reference data.
#'
#'
#' @param original_data The original data
#' @param k The amount of neighbouring points to consider (recommended range: from 5 to 10)
#' @param reference_data The anonymized data
#' @param dist The distance measure to use. (Defaults to eucledian distance)
#'
#' @importFrom graphics plot legend lines par title
#' @examples
#' original_data <- as.data.frame(matrix(1:6, ncol = 2))
#' reference_data <- as.data.frame(matrix(7:12, ncol = 2))
#' prediction_plot(original_data, k = 2, reference_data)
#'
#' @export
prediction_plot = function(original_data, k, reference_data, dist = euc_dist){
  # plot the measures in original against measures of the same type in reference make par(mfrow = c(3,1)) before calling this function and revert it after calling this function
  par(mfrow = c(3,1))

  prediction_all_output = prediction_all(original_data, k, reference_data, dist)

  # cumulative sum of distances normalized by the sum of distances
  y_scaler = function(data){
    data = cumsum(sort(data))/sum(data)
    # if data has any NaNs, replace the data with a sequence from 0 to 1, with length equal to the length of data
    if(any(is.nan(data))) data = seq(0, 1, length.out = length(data))
    return(data)
  }

  # prediction_distance
  plot(sort(prediction_all_output$original$prediction_distance), y_scaler(prediction_all_output$original$prediction_distance), type = "l",  xlab = "Prediction distance", ylab = "Cumulative sum of prediction distance", xlim = c(0, max(prediction_all_output$original$prediction_distance, prediction_all_output$reference$prediction_distance)))
  lines(sort(prediction_all_output$reference$prediction_distance), y_scaler(prediction_all_output$reference$prediction_distance), type = "l", col = "red")
  legend("topleft", legend = c("Non overlapping sample", "Anonymized data"), col = c("black", "red"), lty = 1, cex = 0.8, bg = "transparent")
  title("Prediction distance")

  # prediction_ambiguity
  plot(sort(prediction_all_output$original$prediction_ambiguity), y_scaler(prediction_all_output$original$prediction_ambiguity), type = "l",  xlab = "Prediction ambiguity", ylab = "Cumulative sum of prediction ambiguity", xlim = c(0, max(prediction_all_output$original$prediction_ambiguity, prediction_all_output$reference$prediction_ambiguity)))
  lines(sort(prediction_all_output$reference$prediction_ambiguity), y_scaler(prediction_all_output$reference$prediction_ambiguity), type = "l", col = "red")
  legend("topleft", legend = c("Non overlapping sample", "Anonymized data"), col = c("black", "red"), lty = 1, cex = 0.8, bg = "transparent")
  title("Prediction ambiguity")

  # prediction_uncertainty
  plot(sort(prediction_all_output$original$prediction_uncertainty), y_scaler(prediction_all_output$original$prediction_uncertainty), type = "l",  xlab = "Prediction uncertainty", ylab = "Cumulative sum of prediction uncertainty", xlim = c(0, max(prediction_all_output$original$prediction_uncertainty, prediction_all_output$reference$prediction_uncertainty)))
  lines(sort(prediction_all_output$reference$prediction_uncertainty), y_scaler(prediction_all_output$reference$prediction_uncertainty), type = "l", col = "red")
  legend("topleft", legend = c("Non overlapping sample", "Anonymized data"), col = c("black", "red"), lty = 1, cex = 0.8, bg = "transparent")
  title("Prediction uncertainty")

  par(mfrow = c(1,1))
}


#-----------------------------------------------------------------

#' Prediction Plot (ggplot)
#'
#' Plot the measures of predictive disclosure risk in a nonoverlapping sample to the general populace against the measures of the same type in the reference data.
#'
#'
#' @param original_data The original data
#' @param k The amount of neighbouring points to consider (recommended range: from 5 to 10)
#' @param reference_data The anonymized data
#' @param dist The distance measure to use. (Defaults to eucledian distance)
#'
#' @importFrom ggplot2 ggplot aes geom_line labs theme
#' @importFrom gridExtra grid.arrange
#' @examples
#' original_data <- as.data.frame(matrix(1:6, ncol = 2))
#' reference_data <- as.data.frame(matrix(7:12, ncol = 2))
#' prediction_plot(original_data, k = 2, reference_data)
#'
#' @export
prediction_plot_gg = function(original_data, k, reference_data, dist = euc_dist) {

  prediction_all_output = prediction_all(original_data, k, reference_data, dist)

  # cumulative sum of distances normalized by the sum of distances
  y_scaler = function(data) {
    data = cumsum(sort(data))/sum(data)
    # if data has any NaNs, replace the data with a sequence from 0 to 1, with length equal to the length of data
    if(any(is.nan(data))) data = seq(0, 1, length.out = length(data))
    return(data)
  }

  # prediction_distance
  p1 <- ggplot() +
    geom_line(aes(x = sort(prediction_all_output$original$prediction_distance), y = y_scaler(prediction_all_output$original$prediction_distance)), color = "black") +
    geom_line(aes(x = sort(prediction_all_output$reference$prediction_distance), y = y_scaler(prediction_all_output$reference$prediction_distance)), color = "red") +
    xlab("Prediction distance") +
    ylab("Cumulative sum of prediction distance") +
    xlim(0, max(prediction_all_output$original$prediction_distance, prediction_all_output$reference$prediction_distance)) +
    ggtitle("Prediction distance") +
    theme_minimal()

  # prediction_ambiguity
  p2 <- ggplot() +
    geom_line(aes(x = sort(prediction_all_output$original$prediction_ambiguity), y = y_scaler(prediction_all_output$original$prediction_ambiguity)), color = "black") +
    geom_line(aes(x = sort(prediction_all_output$reference$prediction_ambiguity), y = y_scaler(prediction_all_output$reference$prediction_ambiguity)), color = "red") +
    xlab("Prediction ambiguity") +
    ylab("Cumulative sum of prediction ambiguity") +
    xlim(0, max(prediction_all_output$original$prediction_ambiguity, prediction_all_output$reference$prediction_ambiguity)) +
    ggtitle("Prediction ambiguity") +
    theme_minimal()

  # prediction_uncertainty
  p3 <- ggplot() +
    geom_line(aes(x = sort(prediction_all_output$original$prediction_uncertainty), y = y_scaler(prediction_all_output$original$prediction_uncertainty)), color = "black") +
    geom_line(aes(x = sort(prediction_all_output$reference$prediction_uncertainty), y = y_scaler(prediction_all_output$reference$prediction_uncertainty)), color = "red") +
    xlab("Prediction uncertainty") +
    ylab("Cumulative sum of prediction uncertainty") +
    xlim(0, max(prediction_all_output$original$prediction_uncertainty, prediction_all_output$reference$prediction_uncertainty)) +
    ggtitle("Prediction uncertainty") +
    theme_minimal()

  grid.arrange(p1, p2, p3, ncol = 1)
}


#-----------------------------------------------------------------

#' Calculate the reidentification rate of a dataset after anonymization
#'
#' This function calculates the reidentification rate of a dataset after it has been anonymized. The function one-hot encodes both the original and reference datasets to enable distance calculations, scales the datasets to have mean 0 and standard deviation 1, and then finds the nearest row in the reference data for each row of the original data. The function returns the proportion of correct guesses.
#'
#' @param original_data A data frame containing the original dataset
#' @param reference_data A data frame containing the reference dataset
#' @importFrom onehot onehot
#' @param dist An optional distance function to use for calculating distances between rows of the datasets. Defaults to the Euclidean distance function.
#' @return A numeric value representing the proportion of correct guesses
#' @export
#' @examples
#' original_data <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' reference_data <- data.frame(x = c(2, 3, 4), y = c(5, 6, 7))
#' reidentification_rate(original_data, reference_data)
reidentification_rate = function(original_data, reference_data, dist = euc_dist){
  # Calculate distances for every row of the original data against the reference data
  distances = distances(original_data, reference_data, dist)

  # Calculate the reidentification rate, i.e. the proportion of correct guesses, when taking the smallest distance as the best match
  reidentification_rate = sum(apply(distances, 1, which.min) == seq(nrow(original_data)))/nrow(original_data)

  cat("Reidentification rate of anonymous data:",  reidentification_rate, ", versus pure guessing (approximately):", 1/nrow(original_data), ", ratio of:",reidentification_rate*nrow(original_data) ,"\n")
  return(reidentification_rate)


}

