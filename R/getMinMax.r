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
