#' Combine the lowest classes
#'
#' This function combines the lowest classes in a column up to a specified number (k).
#' The lowest classes are determined based on the frequency of occurrence in the column.
#'
#' @param column The column in which to combine the lowest classes.
#' @param k The maximum number of lowest classes to combine.
#'
#' @return The column with the lowest classes combined.
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
