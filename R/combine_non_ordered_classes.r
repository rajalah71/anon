combine_lowest_classes_legacy <- function(column, k) {

  # Find the unique values in the column and raise error if k is too large
  unique_vals <- unique(column)
  if(k+1 > length(unique_vals)){
    stop("k+1 is larger than the amount of unique classes in the data!")
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

combine_lowest_classes <- function(column, k) {

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
