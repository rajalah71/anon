#' Calculate the reidentification rate of a dataset after anonymization
#'
#' This function calculates the reidentification rate of a dataset after it has been anonymized. The function one-hot encodes both the original and reference datasets to enable distance calculations, scales the datasets to have mean 0 and standard deviation 1, and then finds the nearest row in the reference data for each row of the original data. The function returns the proportion of correct guesses.
#'
#' @param original_data A data frame containing the original dataset
#' @param anon_data A data frame containing the reference dataset
#' @param quasiIdentifiers The quasi-identifiers of the data given as a vector of names.
#' @importFrom onehot onehot
#' @param dist An optional distance function to use for calculating distances between rows of the datasets. Defaults to the Euclidean distance function.
#' @return A numeric value representing the proportion of correct guesses
reidentification_rate_old = function(original_data, anon_data, quasiIdentifiers,  dist = euc_dist){

  # Drop rows not in quasiIdentifier
  original_data = original_data[, quasiIdentifiers]
  anon_data = anon_data[, quasiIdentifiers]

  # Calculate distances for every row of the original data against the reference data
  distances = distances(original_data, anon_data, dist)

  # Calculate the reidentification rate, i.e. the proportion of correct guesses, when taking the smallest distance as the best match
  reidentification_rate = sum(apply(distances, 1, which.min) == seq(nrow(original_data)))/nrow(original_data)

  cat("Reidentification rate of anonymous data:",  reidentification_rate, ", versus pure guessing (approximately):", 1/nrow(original_data))

  return(reidentification_rate)


}



#------------------------------------------------------------

#-----------------------------------------------------------------

#' Prediction Plot
#'
#' Plot the measures of predictive disclosure risk in an nonoverlapping sample to the general populace against the measures of the same type in the reference data.
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
#'
prediction_plot_old = function(original_data, k, anon_data, n = 1000, dist = euc_dist){
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

  plot(og[[1]], vert_maker(og[[2]]), type = "l",  xlab = "Ennuste-etäisyys", cex.lab = 1.5, ylab = "Kertymäfunktio",   xlim = c(0, max(prediction_all_output$original$prediction_distance, prediction_all_output$reference$prediction_distance)))
  lines(ref[[1]], vert_maker(ref[[2]]), type = "l", col = "red")
  legend("topleft", legend = c("Alkuperäinen aineisto", "Anonyymi aineisto"), col = c("black", "red"), lty = 1, cex = 1.3, bg = "transparent")
  legend("bottomright", legend = paste("Riittävä suoja: ", format),  bty = "n", cex = 1.3, bg = "transparent")
  title("Ennuste-etäisyys", cex.main = 1.4)

  og = ecdf_points(prediction_all_output$original$prediction_ambiguity)
  ref = ecdf_points(prediction_all_output$reference$prediction_ambiguity)
  # prediction_ambiguity

  erotus = quantile_distance_0_1_calc(prediction_all_output$original$prediction_ambiguity, prediction_all_output$reference$prediction_ambiguity)
  format = formatC(signif(erotus, digits=3), digits=3, format="fg", flag="#")


  plot(og[[1]], vert_maker(og[[2]]), type = "l",  xlab = "Ennuste-epäselvyys", cex.lab = 1.5, ylab = "Kertymäfunktio", xlim = c(0, max(prediction_all_output$original$prediction_ambiguity, prediction_all_output$reference$prediction_ambiguity)))
  lines(ref[[1]], vert_maker(ref[[2]]), type = "l", col = "red")
  legend("topleft", legend = c("Alkuperäinen aineisto", "Anonyymi aineisto"), col = c("black", "red"), lty = 1, cex = 1.3, bg = "transparent")
  legend("bottomright", legend = paste("Riittävä suoja: ", format),  bty = "n", cex = 1.3, bg = "transparent")
  title("Ennuste-epäselvyys", cex.main = 1.4 )

  og = ecdf_points(prediction_all_output$original$prediction_uncertainty)
  ref = ecdf_points(prediction_all_output$reference$prediction_uncertainty)
  # prediction_uncertainty

  erotus = quantile_distance_0_1_calc(prediction_all_output$original$prediction_uncertainty, prediction_all_output$reference$prediction_uncertainty)
  format = formatC(signif(erotus, digits=3), digits=3, format="fg", flag="#")

  plot(og[[1]], vert_maker(og[[2]]), type = "l",  xlab = "Ennuste-epävarmuus", cex.lab = 1.5, ylab = "Kertymäfunktio", xlim = c(0, max(prediction_all_output$original$prediction_uncertainty, prediction_all_output$reference$prediction_uncertainty)))
  lines(ref[[1]], vert_maker(ref[[2]]), type = "l", col = "red")
  legend("topleft", legend = c("Alkuperäinen aineisto", "Anonyymi aineisto"), col = c("black", "red"), lty = 1, cex = 1.3, bg = "transparent")
  legend("bottomright", legend = paste("Riittävä suoja: ", format),  bty = "n", cex = 1.3, bg = "transparent")
  title("Ennuste-epävarmuus", cex.main = 1.4)

  par(mfrow = c(1,1))
}

