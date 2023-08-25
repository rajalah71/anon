# function to combine values in ordered categorical variable column

combine_ordered = function(column){

  # get unique values in the column and sort them
  unique_values = unique(column)
  sorted_uniques = sort(unique_values)
  #count the number of unique values
  n = length(unique_values)

  # check whether the the amount of unique values is even or odd
  if(n %% 2 == 0){

    # make bands of sorted unique values as [sorted_uniques[i], sorted_uniques[i+1]] for all i in [1,3,5,...,n-1]
    for(i in seq(1, n-1, 2)){
      column[column == sorted_uniques[i]] = paste0("[", sorted_uniques[i], ",", sorted_uniques[i+1], "]")
      column[column == sorted_uniques[i+1]] = paste0("[", sorted_uniques[i], ",", sorted_uniques[i+1], "]")
    }
  } else {
    # Take the middle value of the sorted unique values and make it a bands of its ows while the rest of the values are banded as in the even case
    middle = sorted_uniques[(n+1)/2]
    #column[column == middle] = paste0("[", middle, ",", middle, "]")

    for(i in seq(1, middle-2, 2)){
      column[column == sorted_uniques[i]] = paste0("[", sorted_uniques[i], ",", sorted_uniques[i+1], "]")
      column[column == sorted_uniques[i+1]] = paste0("[", sorted_uniques[i], ",", sorted_uniques[i+1], "]")
    }

    for(i in seq(middle+1, n-1 , 2)){
      column[column == sorted_uniques[i]] = paste0("[", sorted_uniques[i], ",", sorted_uniques[i+1], "]")
      column[column == sorted_uniques[i+1]] = paste0("[", sorted_uniques[i], ",", sorted_uniques[i+1], "]")
    }
  }

  return(column)


  }



row_checker = function(original_data, reference_data){
  # check whether any row in the reference data are in the original data, is so stop
  for(i in seq(nrow(reference_data))){
    # checke whether the row is in the original data
    if(any(apply(original_data, 1, function(x) all(x == reference_data[i,])))){
      stop("An exact match was produced with the method. Try again (if random) or try another method alltogether.")
    }

  }
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
