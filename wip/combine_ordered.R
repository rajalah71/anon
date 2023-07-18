generalize_categories <- function(column) {
  unique_values <- unique(column)
  num_categories <- length(unique_values)

  if (num_categories %% 2 == 0) {
    # Even number of categories
    ranges <- split(unique_values, rep(1:(num_categories/2), each = 2))
  } else {
    # Odd number of categories
    most_populated <- names(which.max(table(column)))
    other_categories <- unique_values[unique_values != most_populated]

    ranges <- split(other_categories, rep(1:(num_categories-1)/2, each = 2))
    ranges <- c(list(most_populated), ranges)
  }

  result <- sapply(ranges, function(x) column %in% x)
  result <- apply(result, 2, function(x) range(column[x]))
  result <- lapply(result, function(x) ifelse(length(x) == 1, x, paste(x[1], x[2], sep = ",")))

  return(ranges)
}

# Here's how you can use this function:
#
# R
#
# # Sample data
# column <- c(1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 7)
#
# # Apply the function
# result <- generalize_categories(column)
#
# # Print the result
# for (i in seq_along(result)) {
#   cat("[", paste(result[[i]], collapse = " "), "] ")
# }
