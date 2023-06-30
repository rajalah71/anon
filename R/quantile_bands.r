#' Divide a column into quantile bands
#'
#' This function divides a column into quantile bands based on the specified number of bands.
#'
#' @param column A numeric vector representing the column to be divided into bands.
#' @param n_bands An integer specifying the number of bands to create.
#' @param returnmean A logical value indicating whether to return the mean value of each band instead of the band itself.
#' @param inclusive_tails A logical value indicating whether to include the lowest and highest values in the first and last bands respectively.
#'
#' @importFrom stats quantile
#'
#' @return A character vector representing the quantile bands for each value in the column.
#'
#' @examples
#' data <- c(10, 20, 15, 25, 30)
#' bands <- quantile_bands(data, 3, returnmean = FALSE, inclusive_tails = TRUE)
#' @export
quantile_bands = function(column, n_bands, returnmean = FALSE, inclusive_tails = TRUE) {
  n_data_points <- length(column)

  # Divide the column support to n_bands such that each band holds equal amount of probability mass or samples
  quantiles = quantile(column, probs = seq(0, 1, length.out = n_bands + 1))

  # Check if the quantiles are unique. If the quantiles are not unique:
  if (length(unique(quantiles)) < n_bands + 1) {
    # Return the original data points as they are
    return(as.character(column))
  }

  # Move the given column values to quantile ranges, with dig.lab determining how many digits are returned
  # Fiddle around with it if you have problems with scientific number representation.
  bands = cut(column, breaks = quantiles, include.lowest = TRUE, right = FALSE, dig.lab = 6)

  # Format the first and last bands as inequalities if inclusive_tails == FALSE and returnmean == FALSE
  if (!inclusive_tails && !returnmean) {
    levels(bands)[1] = paste0("<", round(quantiles[2], 2))
    levels(bands)[n_bands] = paste0(">=", round(quantiles[n_bands], 2))
  }

  # If returnmean is TRUE, calculate the mean of each band instead of returning the band itself
  if (returnmean) {
    band_means <- tapply(column, bands, mean)
    result <- (band_means[bands])
  } else {
    # Return the band for each value in the column
    result <- as.character(bands)
  }

  return(result)
}
