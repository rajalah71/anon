#Datasetdistance----------------------------------------------------------

#' Calculate the distances between rows of two datasets.
#'
#' This function calculates the distances between each row of the
#' \code{original_data} and every row of the \code{anon_data} using a specified
#' distance function (\code{dist}). The datasets are first one-hot encoded to enable
#' distance calculations. The function then scales the datasets to have mean 0 and
#' standard deviation 1. If anon_data is NULL, calculate the distance between each row
#' of the \code{original_data} against the rest of the \code{original_data}. Otherwise
#' the function calculates the distance between
#' each row of the \code{original_data} against every row of the \code{anon_data}.
#'
#' @param original_data A data frame representing the original dataset.
#'
#' @param anon_data A data frame representing the reference dataset.
#'
#' @param dist A function that calculates the distance between two rows of a data frame.
#'
#'
#' @return A numeric vector containing the minimum distances between rows of the
#'         \code{original_data} and \code{anon_data}.
#'
#' @importFrom onehot onehot
#'
distances = function(original_data, anon_data = NULL, dist = euc_dist){
  # onehot encode the datasets to enable distance calculations
  original_data = predict(onehot(original_data, stringsAsFactors = TRUE, max_levels = Inf), original_data)
  if(!is.null(anon_data)) anon_data = predict(onehot(anon_data, stringsAsFactors = TRUE, max_levels = Inf), anon_data)

  # check if the dataframes have the same number columns and stop if not (only if anon_data != NULL)
  if(!is.null(anon_data)){
    if(ncol(original_data) != ncol(anon_data)){
      stop("The dataframes do not have the same number of columns.")
    }
  }

  # scale the datasets with the original datasets colmeans and -variances
  og_colmeans = colMeans(original_data)
  og_colvars = colVars(original_data)
  original_data = scale(original_data)
  if(!is.null(anon_data)) anon_data = scale(anon_data, og_colmeans, sqrt(og_colvars))

  # calculate the distance between each row of the original data against every row of the reference data and return the sorted for each row
  # if anon_data is null, the distance is calculated by leaving one row out of the original data and calculating the distance between the left out row and the rest of the original data

  if(is.null(anon_data)){
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
    sorted_distances = as.data.frame(matrix(NA, nrow(original_data), nrow(anon_data)))
    for(i in seq(nrow(original_data))){
      distances = rep_len(NA, nrow(original_data))
      for(j in seq(nrow(anon_data))){
        distances[j] = dist(original_data[i,], anon_data[j,])
      }
      sorted_distances[i,] = (distances)
    }
  }
  return(sorted_distances)
}

#Prediction calcs-----------------------------------------------------------------

#' Prediction Distance
#'
#' Calculate distances between each row of the original_data and the anon_data using the specified distance function.
#'
#' @param original_data The original dataset.
#' @param anon_data The reference dataset (default: NULL).
#' @param dist Distance function to use (default: euc_dist).
#' @param distances precalculated distances (optional)
#' @return A numeric vector containing the distances between each row of the original_data and the anon_data.
#'
prediction_distance = function(original_data, anon_data = NULL, dist = euc_dist, distances = NULL){
  # calculate distances
  if(is.null(distances)) distances = distances(original_data, anon_data, dist)

  # sort each row in ascending order
  distances = t(apply(distances, 1, sort))

  # return the first column of the distances
  return(distances[,1])
}




#' Prediction Ambiguity
#'
#' Calculates the prediction ambiguity by comparing the relative distance from each record to its nearest neighbor versus its kth nearest neighbor in the set of distances.
#'
#' @param original_data The original dataset for which prediction ambiguity needs to be calculated.
#' @param k The number of nearest neighbors to consider for calculating the ambiguity.
#' @param anon_data (Optional) The reference dataset used for calculating the distances. If not provided, the distances will be computed within the original_data.
#' @param dist The distance function to be used. Default is euclidean distance (euc_dist).
#' @param distances precalculated distances (optional)
#'
#' @return A numeric vector representing the prediction ambiguity for each record in the original_data.
#'
#'
prediction_ambiguity = function(original_data, k, anon_data = NULL, dist = euc_dist, distances = NULL){
  # Prediction ambiguity gives the relative distance from the record Aj to the nearest versus the kth-nearest record in the set distances
  if(is.null(distances)) distances = distances(original_data, anon_data, dist)

  # sort each row in ascending order
  distances = t(apply(distances, 1, sort))

  # return the first column of the distances divided by the kth column of the distances
  return(distances[,1]/distances[,k])
}



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
#' @param anon_data The reference dataset of the same column space (default: NULL).
#' @param k Number of best matches to find.
#' @param dist Distance function to use (default: euc_dist).
#' @param distances precalculated distances (optional)
#' @return A numeric vector containing the variances for each row of the original_data.
#'
#'
#' @importFrom onehot onehot
#' @importFrom stats var
#'
prediction_uncertainty = function(original_data, k, anon_data = NULL, dist = euc_dist, distances = NULL){
  # Prediction uncertainty gives the variation among the k best matches for each row of the original data in the reference data
  # for each row of the original data, find the k best matches in the reference data using the dist function

  # calculate distances between each row of the original data and the reference data using the specified distance function
  if(is.null(distances)) distances = distances(original_data, anon_data, dist)

  # onehot encode and scale both datasets
  original_data = predict(onehot(original_data, stringsAsFactors = TRUE, max_levels = Inf), original_data)
  if(!is.null(anon_data)) anon_data = predict(onehot(anon_data, stringsAsFactors = TRUE, max_levels = Inf), anon_data)
  original_data = scale(original_data)
  if(!is.null(anon_data)) anon_data = scale(anon_data)

  # Empty vector to store variances
  variances = rep_len(NA, nrow(original_data)-1)

  # if reference data is null, operate on the original data
  if(is.null(anon_data)){
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

      # pick the k best matches from the anon_data
      k_best_matches_rows = anon_data[k_best_matches,]

      # calculate the mean variance of the k best matches rows
      variances[i] = mean(apply(k_best_matches_rows, 2, var))
    }
  }

  # return the variances
  return(variances)

}


#' Prediction All Measures
#'
#' Wrapper function to calculate all the measures for anonymous and original data,
#' store them into a list in the order of prediction distance,
#' prediction ambiguity, and prediction uncertainty,
#' and return the list.
#'
#' @param original_data The original dataset.
#' @param k Number of kth-nearest records to consider.
#' @param anon_data The anonymous dataset.
#' @param dist Distance function to use (default: euc_dist).
#' @return A list containing two sub-lists:
#'   - original_list: Contains the measures for the original_data without a anon_data.
#'   - anon_list: Contains the measures for the original_data with the provided anon_data.
#'
#' @examples
#' \dontrun{
#' original_data <- as.data.frame(matrix(1:6, ncol = 2))
#' anon_data <- as.data.frame(matrix(7:12, ncol = 2))
#' prediction_all(original_data, k = 2, anon_data)
#' }
#' @export
prediction_all = function(original_data, k, anon_data, dist = euc_dist){
  # Calculate all the measures and store them in a list

  # distances 1
  distances1 = distances(original_data, anon_data, dist)

  anon_list = list()
  anon_list$prediction_distance = prediction_distance(original_data, anon_data, dist, distances = distances1)
  anon_list$prediction_ambiguity = prediction_ambiguity(original_data, k, anon_data, dist, distances = distances1)
  anon_list$prediction_uncertainty = prediction_uncertainty(original_data, k, anon_data, dist, distances = distances1)

  # distances 2
  distances2 = distances(original_data, NULL, dist)

  original_list = list()
  original_list$prediction_distance = prediction_distance(original_data, anon_data = NULL, dist, distances = distances2)
  original_list$prediction_ambiguity = prediction_ambiguity(original_data, k, anon_data = NULL, dist, distances = distances2)
  original_list$prediction_uncertainty = prediction_uncertainty(original_data, k, anon_data = NULL, dist, distances = distances2)

  return(list(original = original_list, reference = anon_list))
}

#' Prediction All Measures list version
#'
#' Wrapper function to calculate all the measures for anonymous and original data,
#' store them into a list in the order of prediction distance,
#' prediction ambiguity, and prediction uncertainty,
#' and return the list.
#'
#' @param datalist A list containing original and anonymous data
#' @param k Number of kth-nearest records to consider.
#' @param dist Distance function to use (default: euc_dist).
#' @return A list containing two sub-lists:
#'   - original_list: Contains the measures for the original_data without a anon_data.
#'   - anon_list: Contains the measures for the original_data with the provided anon_data.
#'
#' @examples
#' \dontrun{
#' original_data <- as.data.frame(matrix(1:6, ncol = 2))
#' anon_data <- as.data.frame(matrix(7:12, ncol = 2))
#' prediction_all(original_data, k = 2, anon_data)
#' }
#' @export
prediction_all_list = function(datalist, k, dist = euc_dist){
  # Calculate all the measures and store them in a list

  # calculate prediction_all for all data pairs (original and anonymous) in the list
  pred_all_list = lapply(seq_along(datalist), function(i){
    prediction_all(datalist[[i]][[1]], k, datalist[[i]][[2]], dist)
  })

  return(pred_all_list)
}

#predplot-----------------------------------------------------------------

#' Prediction Plot
#'
#' Plot the measures of predictive disclosure risk in an nonoverlapping sample against the measures of the same type in the anonymous data.
#'
#'
#' @param original_data The original data
#' @param k The amount of neighbouring points to consider (recommended range: from 5 to 10)
#' @param anon_data The anonymized data
#' @param n The number of points to plot (default: 1000)
#' @param dist The distance measure to use. (Defaults to eucledian distance)
#'
#' @importFrom graphics plot legend lines par title
#' @importFrom stats ecdf
#' @examples
#' \dontrun{
#' original_data <- as.data.frame(matrix(1:6, ncol = 2))
#' anon_data <- as.data.frame(matrix(7:12, ncol = 2))
#' prediction_plot(original_data, k = 2, anon_data)
#' }
#' @export
prediction_plot = function(original_data, k, anon_data, n = 1000, dist = euc_dist){
  # plot the measures in original against measures of the same type in reference make par(mfrow = c(3,1)) before calling this function and revert it after calling this function
  par(mfrow = c(3,1))

  prediction_all_output = prediction_all(original_data, k, anon_data, dist)

  # Helper function to make vertical lines if needed
  vert_maker = function(y){
    # If only one unique value is present, return a sequence from 0 to 1 instead
    table = table(y)
    if(length(table) == 1){
      vert = seq(0,1,length.out = length(y))
      return(vert)
    }
    # Else return the original data unchanged
    return(y)
  }

  # estimate cdf from data and calculate n equidistant points from it
  ecdf_points <- function(data) {
    # Create an ECDF function
    ecdf_func <- ecdf(data)

    # Generate a sequence of values for x-axis
    x <- seq(min(data), max(data), length.out = n)

    # Calculate the estimated CDF values for the x-axis values
    y <- ecdf_func(x)

    # return x and y
    return(list(x,y))

  }

  quantile_distance_0_1_calc = function(og, anon, n = 1000){
    # calculate the proportion of points in anon quantile function that have no smaller value thank in og quantile function
    amount = 0

    # Estimate quantile functions
    og_q = quantile(og, probs = seq(0, 1, 1/n))
    anon_q = quantile(anon, probs = seq(0, 1, 1/n))

    # Calculate the proportion
    for(i in 1:n){
      if(og_q[i] <= anon_q[i]){
        amount = amount + 1
      }
    }
    return(amount/n)
  }

  og = ecdf_points(prediction_all_output$original$prediction_distance)
  ref = ecdf_points(prediction_all_output$reference$prediction_distance)
  # prediction_distance

  erotus = quantile_distance_0_1_calc(prediction_all_output$original$prediction_distance, prediction_all_output$reference$prediction_distance)
  format = formatC(signif(erotus, digits=3), digits=3, format="fg", flag="#")

  plot(vert_maker(og[[2]]), og[[1]], type = "l",  xlab = "Kvantiilifunktio", cex.lab = 1.5, ylab = "Ennuste-etäisyys",   ylim = c(0, max(prediction_all_output$original$prediction_distance, prediction_all_output$reference$prediction_distance)))
  lines(vert_maker(ref[[2]]), ref[[1]], type = "l", col = "red")
  legend("topleft", legend = c("Alkuperäinen aineisto", "Anonyymi aineisto"), col = c("black", "red"), lty = 1, cex = 1.3, bg = "transparent")
  legend("bottomright", legend = paste("Riittävä suoja: ", format),  bty = "n", cex = 1.3, bg = "transparent")
  title("Ennuste-etäisyys", cex.main = 1.4)

  og = ecdf_points(prediction_all_output$original$prediction_ambiguity)
  ref = ecdf_points(prediction_all_output$reference$prediction_ambiguity)
  # prediction_ambiguity

  erotus = quantile_distance_0_1_calc(prediction_all_output$original$prediction_ambiguity, prediction_all_output$reference$prediction_ambiguity)
  format = formatC(signif(erotus, digits=3), digits=3, format="fg", flag="#")

  plot(vert_maker(og[[2]]), og[[1]], type = "l",  xlab = "Kvantiilifunktio", cex.lab = 1.5, ylab = "Ennuste-epäselvyys",   ylim = c(0, max(prediction_all_output$original$prediction_ambiguity, prediction_all_output$reference$prediction_ambiguity)))
  lines(vert_maker(ref[[2]]), ref[[1]], type = "l", col = "red")
  legend("topleft", legend = c("Alkuperäinen aineisto", "Anonyymi aineisto"), col = c("black", "red"), lty = 1, cex = 1.3, bg = "transparent")
  legend("bottomright", legend = paste("Riittävä suoja: ", format),  bty = "n", cex = 1.3, bg = "transparent")
  title("Ennuste-epäselvyys", cex.main = 1.4)

  og = ecdf_points(prediction_all_output$original$prediction_uncertainty)
  ref = ecdf_points(prediction_all_output$reference$prediction_uncertainty)
  # prediction_uncertainty

  erotus = quantile_distance_0_1_calc(prediction_all_output$original$prediction_uncertainty, prediction_all_output$reference$prediction_uncertainty)
  format = formatC(signif(erotus, digits=3), digits=3, format="fg", flag="#")

  plot(vert_maker(og[[2]]), og[[1]], type = "l",  xlab = "Kvantiilifunktio", cex.lab = 1.5, ylab = "Ennuste-epävarmuus",   ylim = c(0, max(prediction_all_output$original$prediction_uncertainty, prediction_all_output$reference$prediction_uncertainty)))
  lines(vert_maker(ref[[2]]), ref[[1]], type = "l", col = "red")
  legend("topleft", legend = c("Alkuperäinen aineisto", "Anonyymi aineisto"), col = c("black", "red"), lty = 1, cex = 1.3, bg = "transparent")
  legend("bottomright", legend = paste("Riittävä suoja: ", format),  bty = "n", cex = 1.3, bg = "transparent")
  title("Ennuste-epävarmuus", cex.main = 1.4)

  par(mfrow = c(1,1))
}


#' Prediction Plot list version
#'
#' Plot the measures of predictive disclosure risk in an nonoverlapping sample against the measures of the same type in the anonymous data.
#'
#'
#' @param prediction_all_output List of the prediction_all outputs for a single model with several repetitions
#' @param k The amount of neighbouring points to consider (recommended range: from 5 to 10)
#' @param n The number of points to plot (default: 1000)
#' @param dist The distance measure to use. (Defaults to eucledian distance)
#'
#' @importFrom graphics plot legend lines par title
#' @importFrom stats ecdf
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @examples
#' \dontrun{
#' original_data <- as.data.frame(matrix(1:6, ncol = 2))
#' anon_data <- as.data.frame(matrix(7:12, ncol = 2))
#' prediction_plot(original_data, k = 2, anon_data)
#' }
#' @export
prediction_plot_list = function(prediction_all_output, k, n = 10000, dist = euc_dist){

  # Helper function to make vertical lines if needed
  vert_maker = function(y){
    # If only one unique value is present, return a sequence from 0 to 1 instead
    table = table(y)
    if(length(table) == 1){
      vert = seq(0,1,length.out = length(y))
      return(vert)
    }
    # Else return the original data unchanged
    return(y)
  }

  # estimate cdf from data and calculate n equidistant points from it
  ecdf_points <- function(data) {
    # Create an ECDF function
    ecdf_func <- ecdf(data)

    # Generate a sequence of values for x-axis
    x <- seq(min(data), max(data), length.out = n)

    # Calculate the estimated CDF values for the x-axis values
    y <- ecdf_func(x)

    # return x and y
    return(list(x,y))

  }

  quantile_distance_0_1_calc = function(og, anon, n = 1000){
    # calculate the proportion of points in anon quantile function that have no smaller value thank in og quantile function
    amount = 0

    # Estimate quantile functions
    og_q = quantile(og, probs = seq(0, 1, 1/n))
    anon_q = quantile(anon, probs = seq(0, 1, 1/n))

    # Calculate the proportion
    for(i in 1:n){
      if(og_q[i] <= anon_q[i]){
        amount = amount + 1
      }
    }
    return(amount/n)
  }

  # prediction_distance
  og_list = lapply(seq_along(prediction_all_output), function(x) ecdf_points(prediction_all_output[[x]]$original$prediction_distance))
  ref_list = lapply(seq_along(prediction_all_output), function(x) ecdf_points(prediction_all_output[[x]]$reference$prediction_distance))

  erotus_list = lapply(seq_along(prediction_all_output), function(x) quantile_distance_0_1_calc(prediction_all_output[[x]]$original$prediction_distance, prediction_all_output[[x]]$reference$prediction_distance))
  mean_erotus = mean(unlist(erotus_list))
  format = formatC(signif(mean_erotus, digits=3), digits=3, format="fg", flag="#")

  # plot with ggplot2
  p_distance = ggplot(data = NULL) +
    lapply(seq_along(prediction_all_output), function(x) geom_line(aes(x = vert_maker(og_list[[x]][[2]]), y = og_list[[x]][[1]], colour = "Alkuperäinen aineisto"), size = 1, alpha = 0.1)) +
    lapply(seq_along(prediction_all_output), function(x) geom_line(aes(x = vert_maker(ref_list[[x]][[2]]), y = ref_list[[x]][[1]], colour = "Anonyymi aineisto"), size = 1, alpha = 0.5)) +
    scale_colour_manual(name = "Aineisto", values = c("Alkuperäinen aineisto" = "black", "Anonyymi aineisto" = "red")) +
    labs(x = "Kvantiilifunktio", y = "Ennuste-etäisyys", title = "Ennuste-etäisyys", subtitle = paste0("Riittävä suoja: ", format)) +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 17), legend.key.size = unit(1.5, "cm"), legend.key = element_rect(fill = "transparent", colour = "transparent"), plot.title = element_text(size = 20, face = "bold"), axis.title = element_text(size = 17), axis.text = element_text(size = 17), plot.subtitle = element_text(size = 18))

  #print(p_distance)

  # prediction_ambiguity
  og_listt2 = lapply(seq_along(prediction_all_output), function(x) ecdf_points(prediction_all_output[[x]]$original$prediction_ambiguity))
  ref_list2 = lapply(seq_along(prediction_all_output), function(x) ecdf_points(prediction_all_output[[x]]$reference$prediction_ambiguity))

  erotus_list = lapply(seq_along(prediction_all_output), function(x) quantile_distance_0_1_calc(prediction_all_output[[x]]$original$prediction_ambiguity, prediction_all_output[[x]]$reference$prediction_ambiguity))
  mean_erotus = mean(unlist(erotus_list))
  format = formatC(signif(mean_erotus, digits=3), digits=3, format="fg", flag="#")

  # plot with ggplot2
  p_ambiguity = ggplot(data = NULL) +
    lapply(seq_along(prediction_all_output), function(x) geom_line(aes(x = vert_maker(og_listt2[[x]][[2]]), y = og_listt2[[x]][[1]], colour = "Alkuperäinen aineisto"), size = 1, alpha = 0.5)) +
    lapply(seq_along(prediction_all_output), function(x) geom_line(aes(x = vert_maker(ref_list2[[x]][[2]]), y = ref_list2[[x]][[1]], colour = "Anonyymi aineisto"), size = 1, alpha = 0.5)) +
    scale_colour_manual(name = "Aineisto", values = c("Alkuperäinen aineisto" = "black", "Anonyymi aineisto" = "red")) +
    labs(x = "Kvantiilifunktio", y = "Ennuste-epäselvyys", title = "Ennuste-epäselvyys", subtitle = paste0("Riittävä suoja: ", format)) +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 17), legend.key.size = unit(1.5, "cm"), legend.key = element_rect(fill = "transparent", colour = "transparent"), plot.title = element_text(size = 20, face = "bold"), axis.title = element_text(size = 17), axis.text = element_text(size = 17), plot.subtitle = element_text(size = 18))

  #print(p_ambiguity)


  # prediction_uncertainty
  og_listt3 = lapply(seq_along(prediction_all_output), function(x) ecdf_points(prediction_all_output[[x]]$original$prediction_uncertainty))
  ref_list23 = lapply(seq_along(prediction_all_output), function(x) ecdf_points(prediction_all_output[[x]]$reference$prediction_uncertainty))

  erotus_list = lapply(seq_along(prediction_all_output), function(x) quantile_distance_0_1_calc(prediction_all_output[[x]]$original$prediction_uncertainty, prediction_all_output[[x]]$reference$prediction_uncertainty))
  mean_erotus = mean(unlist(erotus_list))
  format = formatC(signif(mean_erotus, digits=3), digits=3, format="fg", flag="#")

  # plot with ggplot2
  p_uncertainty = ggplot(data = NULL) +
    lapply(seq_along(prediction_all_output), function(x) geom_line(aes(x = vert_maker(og_listt3[[x]][[2]]), y = og_listt3[[x]][[1]], colour = "Alkuperäinen aineisto"), size = 1, alpha = 0.5)) +
    lapply(seq_along(prediction_all_output), function(x) geom_line(aes(x = vert_maker(ref_list23[[x]][[2]]), y = ref_list23[[x]][[1]], colour = "Anonyymi aineisto"), size = 1, alpha = 0.5)) +
    scale_colour_manual(name = "Aineisto", values = c("Alkuperäinen aineisto" = "black", "Anonyymi aineisto" = "red")) +
    labs(x = "Kvantiilifunktio", y = "Ennuste-epävarmuus", title = "Ennuste-epävarmuus", subtitle = paste0("Riittävä suoja: ", format)) +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 17), legend.key.size = unit(1.5, "cm"), legend.key = element_rect(fill = "transparent", colour = "transparent"), plot.title = element_text(size = 20, face = "bold"), axis.title = element_text(size = 17), axis.text = element_text(size = 17), plot.subtitle = element_text(size = 18))



  #print(p_uncertainty)

  grid.arrange(p_distance, p_ambiguity, p_uncertainty, nrow = 3)
}

#ri rates-------------------------------------------------------------

#' Calculate the reidentification rate of a dataset after anonymization
#'
#' This function calculates the reidentification rate of a dataset after it has been anonymized. The function one-hot encodes both the original and reference datasets to enable distance calculations, scales the datasets to have mean 0 and standard deviation 1, and then finds the nearest row in the reference data for each row of the original data. The function returns the proportion of correct guesses.
#'
#' @param data_list A list containing original and anon items
#' @param quasiIdentifiers The quasi-identifiers of the data given as a vector of names.
#' @param n The number of times to calculate the reidentification rate
#' @importFrom onehot onehot
#' @param dist An optional distance function to use for calculating distances between rows of the datasets. Defaults to the Euclidean distance function.
#' @return A numeric value representing the proportion of correct guesses
#' @export
#' @examples
#' \dontrun{
#' reidentification_rate(data_list, quasiIdentifiers, n)
#' }
reidentification_rate_list = function(data_list, quasiIdentifiers, n = 100,  dist = euc_dist){

  # Drop non quasi identifiers from datalist items [[1]] and [[2]]
  og_datalist = lapply(seq_along(data_list), function(x) data_list[[x]][[1]][, quasiIdentifiers])
  anon_datalist = lapply(seq_along(data_list), function(x) data_list[[x]][[2]][, quasiIdentifiers])

  # Calculate distances for every row of the original data against the reference data
  distances = lapply(seq_along(data_list), function(x) distances(og_datalist[[x]], anon_datalist[[x]], dist))

  # Iterate over each model n times. For each loop, take a random permutation of 1:n, reorder the distances objects by column with it and calculate the reidentification rate each time
  all_model_rirates = rep(NA, length(distances))
  for(i in seq_along(distances)){

    # Emtpty vector for reidentification rates
    model_rirates = rep(NA, n)

    # Calculate reidentification rate n times for each distances object, to get a more precise estimate
    for(j in 1:n){
      permutation = sample(1:nrow(distances[[i]]))
      permutated_distances = distances[[i]][, permutation]
      reidentification_rate = sum(apply(permutated_distances, 1, which.min) == order(permutation)) / nrow(distances[[i]])
      model_rirates[j] = reidentification_rate
    }

    # Add ri_rate of the model to the list
    all_model_rirates[i] = mean(model_rirates)
  }

  # return mean of all_model_rirates
  return(mean(all_model_rirates))

}

#' Calculate the reidentification rate of a dataset after anonymization
#'
#' This function calculates the reidentification rate of a dataset after it has been anonymized. The function one-hot encodes both the original and reference datasets to enable distance calculations, scales the datasets to have mean 0 and standard deviation 1, and then finds the nearest row in the reference data for each row of the original data. The function returns the proportion of correct guesses.
#'
#' @param data_list A list containing the original (first item) and anonymous datasets
#' @param quasiIdentifiers The quasi-identifiers of the data given as a vector of names.
#' @param n The number of times to calculate the reidentification rate
#' @importFrom onehot onehot
#' @param dist An optional distance function to use for calculating distances between rows of the datasets. Defaults to the Euclidean distance function.
#' @return A numeric value representing the proportion of correct guesses
#' @export
#' @examples
#' \dontrun{
#' reidentification_rate(data_list, quasiIdentifiers, n)
#' }
reidentification_rate = function(data_list, quasiIdentifiers, n = 1000,  dist = euc_dist){

  # Drop rows not in quasiIdentifier, take original_data here for readability
  original_data = data_list[[1]][, quasiIdentifiers]
  data_list = lapply(data_list, function(x) x[, quasiIdentifiers])

  # Calculate distances for every row of the original data against the reference data
  distances = lapply(data_list, function(x) distances(original_data, x, dist))

  # Iterate over each model n times. For each loop, take a random permutation of 1:n, reorder the distances objects by column with it and calculate the reidentification rate each time
  all_model_rirates = list()
  for(i in seq_along(data_list)){
    # Emtpty vector for reidentification rates
    model_rirates = rep(NA, n)
    for(j in 1:n){
      permutation = sample(1:nrow(original_data))
      permutated_distances = distances[[i]][, permutation]
      reidentification_rate = sum(apply(permutated_distances, 1, which.min) == order(permutation)) / nrow(original_data)
      model_rirates[j] = reidentification_rate
    }
    # Add ri_rate of the model to the list
    all_model_rirates[[names(data_list)[i]]] = model_rirates
  }

  # return all_model_rirates
  return(all_model_rirates)

}
