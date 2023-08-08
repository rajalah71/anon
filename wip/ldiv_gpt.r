#' Check if a dataset is l-diverse for a sensitive attribute
#'
#' This function checks if a dataset is l-diverse for a sensitive attribute
#' based on the quasi-identifiers and the desired level of l-diversity
#'
#' @param data A data frame containing the sensitive data.
#' @param sensitiveAttribute A character vector specifying the column name of the sensitive attribute.
#' @param quasiIdentifiers A character vector specifying the column names of the quasi-identifiers.
#' @param l The desired level of l-diversity.
#'
#' @return A boolean indicating whether the dataset is l-diverse for the sensitive attribute.
#'
#' @importFrom utils flush.console
#'
#' @details The `isLDiverse` function checks if the input dataset `data`
#' is l-diverse for the sensitive attribute `sensitiveAttribute`
#' based on the quasi-identifiers specified in `quasiIdentifiers` and the desired level of l-diversity `l`.
#'
#' @examples
#' \dontrun{
#' data(iris)
#' isLDiverse(iris, "Species", c("Petal.Width", "Sepal.Length"), 2)
#'}
#' @export
isLDiverse <- function(data, sensitiveAttributes, quasiIdentifiers, l) {

  for (sensitiveAttr in sensitiveAttributes) {
    # Treat the remaining sensitive attributes as quasi-identifiers
    quasiIdentifiersForSensitiveAttr <- setdiff(sensitiveAttributes, sensitiveAttr)
    quasiIdentifiersForSensitiveAttr <- c(quasiIdentifiersForSensitiveAttr, quasiIdentifiers)

    # Group the data by the quasi-identifiers
    groups <- split(data, data[, quasiIdentifiersForSensitiveAttr], drop = TRUE)

    # Check if each group has at least l distinct values for the sensitive attribute
    for (group in groups) {
      if (length(unique(group[, sensitiveAttr])) < l) {
        return(FALSE)
      }
    }
  }


  # Return TRUE if all groups for all sensitive attributes are l-diverse
  return(TRUE)
}

#' Make a dataset l-diverse by applying anonymization functions
#'
#' This function takes a dataset and applies a set of diversity functions to the specified
#' quasi-identifier columns in order to achieve l-diversity with respect to the sensitive attributes.
#' It checks if the dataset is already l-diverse and returns the dataset as is in that case.
#' If not, it iteratively combines subsets of the dataset and applies the anonymity functions
#' until the desired level of l-diversity is achieved.
#'
#' @param data The input dataset.
#' @param sensitiveAttributes A character vector specifying the names of the sensitive attributes.
#' @param quasiIdentifiers A character vector specifying the names of the quasi-identifier columns (default: NULL).
#' @param anonymizationFunctions A named list of functions corresponding to the quasi-identifier columns.
#'   Each function should take a vector as input and return a modified vector with the same length.
#' @param l The desired minimum number of distinct values for each sensitive attribute within each group.
#' @param k (Default 5) The minimum number of rows in an l diverse subset. Not stricly an l-diversity
#'          requirement, but here to accomodate for Finnish law / customs on
#'          anonymous data publishing.
#'
#'
#' @return A dataset that is l-diverse with respect to the specified quasi-identifier columns and sensitive attributes,
#'   or an error is thrown if the desired level of l-diversity cannot be achieved.
#'
#' @importFrom utils flush.console
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   age = c(25, 30, 35, 40, 45),
#'   gender = c("M", "M", "F", "F", "M"),
#'   disease = c("A", "A", "B", "C", "C")
#' )
#'
#' # Define functions for generalizing age and gender
#' age_fun <- function(x) floor(x / 10) * 10
#' gender_fun <- function(x) "*"
#'
#' # Apply makeLdiverse function
#' ldiverse_data <- makeLdiverse(data, c("age", "gender"), "disease", list(age = age_fun, gender = gender_fun), 2)
#'}
#' @export
lDiv <- function(data, sensitiveAttributes, l, quasiIdentifiers = NULL, anonymizationFunctions = NULL, k=5) {

  # For runtime
  start_time <- Sys.time()

  # If quasiIdentifiers are not provided, calculate the cardinality of each column
  # and identify numeric and categorical columns for default behavior.
  if (is.null(quasiIdentifiers)) {
    col_cardinality <- sapply(data, function(x) length(unique(x)))
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    categorical_cols <- names(data)[!sapply(data, is.numeric)]
    quasiIdentifiers <- setdiff(names(col_cardinality)[order(-col_cardinality)], sensitiveAttributes)
  } else{
    numeric_cols <- names(data[,quasiIdentifiers, drop = FALSE])[sapply(data[,quasiIdentifiers], is.numeric)]
    categorical_cols <- names(data[,quasiIdentifiers, drop = FALSE])[!sapply(data[,quasiIdentifiers], is.numeric)]
  }

  if (is.null(anonymizationFunctions)) {
    anonymizationFunctions <- list()
    for (col in quasiIdentifiers) {
      if (col %in% numeric_cols) {
        anonymizationFunctions[[col]] <- function(x) mean(x)
      } else {
        anonymizationFunctions[[col]] <- function(x) combine_lowest_classes(x)
      }
    }
  }

  # print(quasiIdentifiers)
  # print(sensitiveAttributes)
  # print(anonymizationFunctions)

  # Check if the column names of quasiIdentifiers match the anonymizationFunctions
  if (!all(names(anonymizationFunctions) %in% quasiIdentifiers)) {
    stop("Column names of the quasi-identifier and anonymization functions do not match.")
  }

  # Check if the dataset is already l-diverse
  if (isLDiverse(data, sensitiveAttributes, quasiIdentifiers, l)) {
    print("The dataset is already l-diverse.")
    return(data)
  }

  # Divide the dataset into a list of subsets based on quasi identifiers
  subsets <- split(data, data[, quasiIdentifiers], drop = TRUE)

  # Iteration counter
  print_counter <- 0

  # For determining maximum iterations
  subset_length <- length(subsets)

  # For indexing.
  i <- 0

  # Iterate over subsets
  # cat("Iterating over all subsets:", subset_length, " iterations at most. \n" )

  while (TRUE) {
    # For printing the progress
    print_counter <- print_counter + 1
    cat("Iteration:", print_counter, "/", subset_length, "\r")
    flush.console()

    # Increase indexing
    i <- i + 1

    # If the length of the subset is smaller than the current index, all blocks have been accounted for
    if (i > length(subsets)){
      break
    }

    # If a given subset is already l-diverse no actions are taken
    if(isLDiverse(subsets[[i]], sensitiveAttributes, quasiIdentifiers, l)){
      #print("Block was already l-diverse")
      next
    }

    # Logical variable that determines how long subsets will be combined to object subsets[[i]]
    combine_more <- TRUE
    while (combine_more) {
      # subsets[i] keeps increasing in size and subsets keep getting popped until subsets[i] can be made l-diverse given the quasi_ids and functions
      nearestSubsetIndex <- findNearestSubset(subsets[i], subsets, quasiIdentifiers)
      if(nearestSubsetIndex == 0){
        message("All subsets combined and no l-diversity obtained.")
        break
      }

      # Add the nearest subset to the current subset
      subsets[[i]] <- rbind(subsets[[nearestSubsetIndex]], subsets[[i]])

      # Remove the added subset from subsets
      subsets <- subsets[-nearestSubsetIndex]

      # If we were to remove a previous subset we need to adjust the indexing
      if(nearestSubsetIndex < i){
        i <- i - 1
      }

      # Check whether the subset has at least k rows. If not, increase size.
      if(nrow(subsets[[i]])<k){
        next
      }

      # The subset may become l-diverse just by increasing its size, so no further actions are taken if so
      if(isLDiverse(subsets[[i]], sensitiveAttributes, quasiIdentifiers, l)){
        break
      }

      # Temporary subset as dataframe to modify
      temp_subset <- subsets[[i]]

      # Apply anonymization functions in the order the columns were given, iteratively until the subset becomes l-diverse
      for (u in seq_along(anonymizationFunctions)) {
        col <- quasiIdentifiers[u]
        fun <- anonymizationFunctions[[col]]
        temp_subset[col] <- lapply(temp_subset[col], fun)
      }

      if(isLDiverse(temp_subset, sensitiveAttributes, quasiIdentifiers, l)){

        # If the subset COULD be made l-diverse, move onto the next block but do NOT commit the changes
        combine_more <- FALSE
      }
    }
  }

  # Finally actually apply the anonymization functions to the subsets and return them as a dataframe
  for(j in seq_along(subsets)){

    # Working subset
    temp_subset <- subsets[[j]]

    # Iterate over anonymization functions
    for (s in seq_along(anonymizationFunctions)) {
      col <- quasiIdentifiers[s]
      fun <- anonymizationFunctions[[col]]
      temp_subset[col] <- lapply(temp_subset[col], fun)

      # No more actions are taken than necessary to make the dataset l-diverse
      if(isLDiverse(temp_subset, sensitiveAttributes, quasiIdentifiers, l)){
        subsets[[j]] <- temp_subset
        break
      }
    }
  }

  # Combine the l-diverse subsets into a single dataset
  lDiverseData <- do.call(rbind, subsets)

  # Just a final check
  if (isLDiverse(lDiverseData, sensitiveAttributes, quasiIdentifiers, l)) {
    shuffled <- lDiverseData[sample(nrow(lDiverseData)), ]
    rownames(shuffled) <- 1:nrow(shuffled)
    print(Sys.time() - start_time)
    return(shuffled)
  }

  else{
    print(Sys.time() - start_time)
    stop("l-diversity could not be obtained.")
  }
}

#obj = sdcMicro::createSdcObj(keyVars = 5, numVars = seq(4), pramVars = 5, sensibleVar = 5, dat = iris)

