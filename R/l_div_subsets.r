#' Check if a dataset is l-diverse for a sensitive attribute
#'
#' This function checks if a dataset is l-diverse for a sensitive attribute
#' based on the quasi-identifiers and the desired level of l-diversity
#'
#' @param data A data frame containing the sensitive data.
#' @param sensitiveAttributes A character vector specifying the column name of the sensitive attribute.
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

#' Make a dataset l-diverse by applying (user provided or default) anonymization functions to a set of quasi-identifiers.
#'
#'
#' @param data The input dataset.
#' @param sensitiveAttributes A character vector specifying the names of the sensitive attributes.
#' @param quasiIdentifiers A character vector specifying the column names of the quasi-identifiers (default: NULL). The first column named will be modified first and the last last:
#' name the columns in ascending order of importance. If quasi-identifiers are nor provided, all columns will be assumed to be quasi-identifiers and modified in descending order of cardinality.
#' @param anonymizationFunctions A named list of anonymization functions for each quasi-identifier column (default: NULL). If not provided, mean and mode will be used for numericals and non-numericals respectively.
#' @param l The desired minimum number of distinct values for each sensitive attribute within each group.
#' @param k (Default 1) The minimum number of rows in an l diverse subset. Not stricly an l-diversity
#'          requirement, but here to be used if needed. Can be increased to improve privacy.
#' @param shuffle Whether to shuffle the dataset before returning. Warning if FALSE, used to calculate empirical reidentification rate.
#'
#' @details
#' This function takes a dataset and applies a set of anonymity functions to the specified
#' quasi-identifier columns in order to achieve l-diversity with respect to the sensitive attributes.
#' It iteratively combines subsets of the dataset and applies the anonymity functions
#' until the desired level of l-diversity is achieved. Quasi-identifiers are
#' columns which combined can identify a person in the data. Quasi-identifiers
#' include but are not limited to: age, gender, zip-code, profession...
#' Sensitive attributes are attributes which values could cause harm to a person
#' if revealed. Sensitive attributes may include: health information and beliefs etc.
#'
#' @return An l-diverse data frame.
#'
#' @importFrom utils flush.console
#'
#' @examples
#' \dontrun{
#' data("iris")
#' lDiversity(iris, "Species", l = 2)
#'}
#' @export
lDiversity <- function(data, sensitiveAttributes, l, quasiIdentifiers = NULL, anonymizationFunctions = NULL, k=1, shuffle = FALSE) {

  # For runtime
  start_time <- Sys.time()

  # if shuffle FALSE, warning
  if(!shuffle) warning("Shuffle is FALSE. Use 'anon::shuffle()' before publishing data.\n")

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
        anonymizationFunctions[[col]] <- function(x) most_common(x)
      }
    }
  }

  # Check if the column names of quasiIdentifiers match the anonymizationFunctions
  if (!all(names(anonymizationFunctions) %in% quasiIdentifiers)) {
    stop("Column names of the quasi-identifier and anonymization functions do not match.")
  }

  # Check if the dataset is already l-diverse
  if (isLDiverse(data, sensitiveAttributes, quasiIdentifiers, l)) {
    print("The dataset is already l-diverse.")
    return(data)
  }

  # Check whether the sensitive columns have at least l unique values
  for (sensitiveAttr in sensitiveAttributes) {
    if (length(unique(data[, sensitiveAttr])) < l) {
      stop("The sensitive attribute '", sensitiveAttr, "' has less than ", l, " unique values.")
    }
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
  cat("Iterating over all subsets:", subset_length, " iterations at most. \n" )

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
        # message("All subsets combined and no l-diversity obtained.")
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
    print(Sys.time() - start_time)
    if(shuffle) return(shuffle(lDiverseData))
    else return(reorder_rownames(lDiverseData))
  } else{
    print(Sys.time() - start_time)
    stop("l-diversity could not be obtained, some sensitive attribute has too many unique values. Try to use 'sensitive_generalizer' on them to modify them beforehand.")
  }
}



#---------------------

#' Find the index of the nearest subset
#'
#' This function finds the index of the nearest subset to a given subset among a list of subsets, based on the specified quasi-identifier columns.
#'
#' @param subset The subset for which to find the nearest subset.
#' @param subsets A list of subsets to compare with the given subset.
#' @param quasiIdentifiers A character vector specifying the quasi-identifier columns.
#'
#' @return The index of the nearest subset in the list.
#'
findNearestSubset <- function(subset, subsets, quasiIdentifiers) {
  nearestSubsetIndex <- 0  # Initialize the index of the nearest subset
  minDistance <- Inf  # Initialize the minimum distance to infinity

  for (i in seq_along(subsets)) {
    otherSubset <- subsets[i]  # Get the current other subset to compare
    if (!identical(subset, otherSubset)) {  # Check if the subsets are not identical
      distance <- matrix_distance(subset, otherSubset, quasiIdentifiers)  # Calculate the distance between the subsets
      if (distance < minDistance) {  # Check if the calculated distance is smaller than the current minimum distance
        minDistance <- distance  # Update the minimum distance
        nearestSubsetIndex <- i  # Update the index of the nearest subset
      }
    }
  }

  return(nearestSubsetIndex)  # Return the index of the nearest subset
}


#----------------

# Function to calculate the distance between mean vectors of two subsets (must be imputed as lists)
#' Calculate the diversity distance between two subsets based on the given quasi-identifiers
#'
#' This function calculates the diversity distance between two subsets based on the given quasi-identifiers.
#'
#' @param subset A data frame representing one subset of the data.
#' @param otherSubset A data frame representing another subset of the data.
#' @param quasiIdentifiers A character vector containing the names of the columns that serve as quasi-identifiers.
#'
#' @importFrom data.table rbindlist as.data.table
#' @importFrom stats dist predict
#' @import onehot
#'
#' @return A numeric value representing the diversity distance between the two subsets.
#'
#'
matrix_distance <- function(subset, otherSubset, quasiIdentifiers) {

  # Convert subset and otherSubset to data frames if they are lists
  subset <- rbindlist(subset)
  otherSubset <- rbindlist(otherSubset)

  # Drop non-quasi-identifiers from the subset
  subset <- subset[, ..quasiIdentifiers]
  n_sub <- nrow(subset)

  # Drop non-quasi-identifiers from the other subset
  otherSubset <- otherSubset[, ..quasiIdentifiers]
  n_other <- nrow(otherSubset)

  # Combine the partial data frames into one
  both_sets <- rbind(subset, otherSubset)

  # Check the levels of variables
  levels_count <- lapply(both_sets, function(x) length(unique(x)))

  # Remove variables with only one level
  col_select <- both_sets[, levels_count > 1, drop=FALSE]
  both_sets <- both_sets[, ..col_select]

  # If no columns are present anymore, the dataframes are indentical
  if(ncol(both_sets) == 0){
    return(0)
  }

  as_numerical_both = predict(onehot::onehot(both_sets, stringsAsFactors = TRUE, max_levels = Inf), both_sets)

  # Normalize
  normalized_all = as.data.table(scale(as_numerical_both))

  # Divide the normalized data frame into the original parts
  subset_normalized <- normalized_all[1:n_sub, ,drop = FALSE]
  otherSubset_normalized <- normalized_all[-(1:n_sub), ,drop = FALSE]

  # Take the colMeans
  mean_subset <- colMeans(subset_normalized)
  mean_otherSubset <- colMeans(otherSubset_normalized)

  # Calculate vector distance using Euclidean distance
  return(dist(rbind(mean_subset, mean_otherSubset)))

}

