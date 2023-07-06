# Define the modified l_diverse function
isLdiverse_temp = function(df, quasi_id_cols, sensitive_cols, l) {
  # Load the required libraries

  #Silence the summary function message
  #options(dplyr.summarise.inform = FALSE)

  # Group the data by the quasi-identifier and sensitive columns
  grouped_df = df[, c(quasi_id_cols, sensitive_cols)] %>% group_by(across(all_of(quasi_id_cols)))
  print(grouped_df)

  # Count the number of distinct rows in each group
  count_df = grouped_df %>% summarise(across(all_of(sensitive_cols), n_distinct))
  print(count_df)

  # Check if all groups have at least l rows
  all(count_df[, sensitive_cols] >= l)
}

#------------------

#' Check if a DataFrame is l-diverse
#'
#' This function checks if a DataFrame is l-diverse based on the specified quasi-identifier columns, sensitive columns, and l value.
#'
#' @param df The DataFrame to be checked.
#' @param quasi_id_cols A character vector specifying the quasi-identifier columns.
#' @param sensitive_cols A character vector specifying the sensitive columns.
#' @param l The minimum number of rows required in each group.
#'
#' @return A logical value indicating whether the DataFrame is l-diverse.
#'
#' @importFrom dplyr group_by summarise n_distinct across all_of
#'
#' @examples
#' \donttest{
#' df <- data.frame(
#'   Q1 = c("A", "A", "B", "B"),
#'   Q2 = c("X", "Y", "X", "Y"),
#'   S = c(1, 2, 3, 4)
#' )
#' is_ldiverse(df, c("Q1", "Q2"), c("S"), 2)
#' }
#' @export
isLdiverse = function(df, quasi_id_cols, sensitive_cols, l) {

  # Iterate over sensitive columns, considering one of them as a sensitive attribute and the rest as quasi identifiers
  for (i in seq_along(sensitive_cols)){
    #print(sensitive_cols[i])

    # Combine the quasi ids and the rest of the sensitive attributes to be the new quasi ids
    quasi_id_cols = c(quasi_id_cols, sensitive_cols[-i])

    # Consider one of the sensitive attributes at time
    sensitive = sensitive_cols[i]

    # Group the data by the quasi-identifier and sensitive columns
    grouped_df = df[, c(quasi_id_cols, sensitive)] %>% group_by(across(all_of(quasi_id_cols)))
    #print(grouped_df)

    # Count the number of distinct rows in each group
    count_df = grouped_df %>% summarise(across(all_of(sensitive), n_distinct))
    #print(count_df)

    # Check if all groups have at least l rows
    if(!all(count_df[, sensitive] >= l)){
      return(FALSE)
    }
  }

  return(TRUE)

}

#--------------



#' Make a dataset l-diverse by applying diversity functions
#'
#' This function takes a dataset and applies a set of diversity functions to the specified
#' quasi-identifier columns in order to achieve l-diversity with respect to the sensitive attributes.
#' It checks if the dataset is already l-diverse and returns the dataset as is in that case.
#' If not, it iteratively combines subsets of the dataset and applies the diversity functions
#' until the desired level of l-diversity is achieved or it is not possible with the given functions.
#'
#' @param data The input dataset.
#' @param quasiIdentifiers A character vector specifying the names of the quasi-identifier columns.
#' @param sensitiveAttributes A character vector specifying the names of the sensitive attributes.
#' @param diversityFunctions A named list of functions corresponding to the quasi-identifier columns.
#'   Each function should take a vector as input and return a modified vector with the same length.
#' @param l The desired minimum number of distinct values for each sensitive attribute within each group.
#'
#'
#' @return A dataset that is l-diverse with respect to the specified quasi-identifier columns and sensitive attributes,
#'   or an error is thrown if the desired level of l-diversity cannot be achieved.
#'
#' @examples
#' \donttest{
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
makeLdiverse <- function(data, quasiIdentifiers, sensitiveAttributes, diversityFunctions, l) {

  # For runtime
  start_time <- Sys.time()

  # Check if the column names of quasiIdentifiers match the diversityFunctions
  if (!all(names(diversityFunctions) %in% quasiIdentifiers)) {
    stop("Column names of the quasi-identifier and diversity functions do not match.")
  }

  # Check if the dataset is already l-diverse
  if (isLdiverse(data, quasiIdentifiers, sensitiveAttributes, l)) {
    print("The dataset is already l-diverse.")
    return(data)
  }

  # Divide the dataset into a list of subsets based on quasi identifiers
  subsets <- split(data, data[, quasiIdentifiers], drop = TRUE)

  # Iteration counter
  print_counter = 0

  # For determining maximun iterations
  subset_length = length(subsets)

  # For indexing.
  i = 0

  # Iterate over subsets
  cat("Iterating over all subsets:", subset_length, " iterations at most. \n" )

  while (TRUE) {
    # For printing the progress
    print_counter = print_counter + 1
    cat("Iteration:", print_counter, "\n")

    # Increase indexing
    i = i + 1

    # If the length of the subset is smaller than the current index, all blocks have been accounter for
    if (i > length(subsets)){
      break
    }

    # If a given subset is already l-diverse no actions are taken
    if(isLdiverse(subsets[[i]], quasiIdentifiers, sensitiveAttributes, l)){
      #print("BLock was alread l-diverse")
      next
    }

    # Logical variable that determines how long subsets will be combined to object subsets[[i]]
    combine_more = TRUE
    while (combine_more) {
      # subsets[i] keeps increasing in size and subsets keep getting popped until subsets[i] can be mande l-diverse given the quasi_ids and functions
      nearestSubsetIndex <- findNearestSubset(subsets[i], subsets, quasiIdentifiers)
      if(nearestSubsetIndex == 0){
        stop("All subsets combined and no l-diversity obtained")
      }

      # Add the nearest subset to the current subset
      subsets[[i]] <- rbind(subsets[[nearestSubsetIndex]], subsets[[i]])

      # Remove the added subset from subsets
      subsets = subsets[-nearestSubsetIndex]

      # If we were to remove a previous subset we need to adjust the indexing
      if(nearestSubsetIndex < i){
        i = i - 1
      }

      # The subset may become l-diverse just by increasing its size, so no further actions are taken if so
      if(isLdiverse(subsets[[i]], quasiIdentifiers, sensitiveAttributes, l)){
        break
      }

      # Temporary subset as dataframe to modify
      temp_subset = subsets[[i]]

      # Apply diversity functions in the order the columns were given, iteratively until the subset becomes l-diverse
      for (u in seq_along(diversityFunctions)) {
        col = quasiIdentifiers[u]
        fun = diversityFunctions[[col]]
        temp_subset[col] <- lapply(temp_subset[col], fun)
      }

      if(isLdiverse(temp_subset, quasiIdentifiers, sensitiveAttributes, l)){

        # If the subset COULD be made l-diverse, move onto next block but do NOT commit the changes
        combine_more = FALSE
      }
    }
  }

  # Finally actually apply the diversity functions to the subsets and return them as a dataframe
  for(j in seq_along(subsets)){

    # Working subset
    temp_subset = subsets[[j]]

    # Iterate over diversity functions
    for (s in seq_along(diversityFunctions)) {
      col = quasiIdentifiers[s]
      fun = diversityFunctions[[col]]
      temp_subset[col] <- lapply(temp_subset[col], fun)

      # No more actions are taken than necessary to make the dataset l-diverse
      if(isLdiverse(temp_subset, quasiIdentifiers, sensitiveAttributes, l)){
        subsets[[j]] = temp_subset
        break
      }
    }
  }


  # Combine the l-diverse subsets into a single dataset
  lDiverseData <- do.call(rbind, subsets)

  # Just a final check
  if (isLdiverse(lDiverseData, quasiIdentifiers, sensitiveAttributes, l)) {
    shuffled = lDiverseData[sample(nrow(lDiverseData)), ]
    rownames(shuffled) = 1:nrow(shuffled)
    print(Sys.time() - start_time)
    return(shuffled)
  }

  else{
    print(Sys.time() - start_time)
    stop("Something went wrong.")
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

#' importFrom caret dummyVars contr.ltfr
#' import caret
#' import MASS
#' importFrom scorecard one_hot
#' importFrom caret dummyVars contr.ltfr
#' import caret

#----------------

# Function to calculate the distance between mean vectors of two subsets (must be inputed as lits)
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
#' @examples
#' \donttest{
#' data1 <- list(data.frame(ID = 1:5, Age = c(25, 35, 40, 28, 32), Gender = c("M", "F", "F", "M", "F")))
#' data2 <- list(data.frame(ID = 6:10, Age = c(28, 38, 42, 31, 29), Gender = c("M", "F", "F", "F", "M")))
#' diversity_distance <- matrix_distance(data1, data2, c("Age", "Gender"))
#' }
#'
matrix_distance <- function(subset, otherSubset, quasiIdentifiers) {

  # Convert subset and otherSubset to data frames if they are lists
  subset <- rbindlist(subset)
  otherSubset <- rbindlist(otherSubset)

  # print("subset")
  # print(subset)
  # print("otherSubset")
  # print(otherSubset)

  # Drop non-quasi-identifiers from the subset
  subset <- subset[, ..quasiIdentifiers]
  n_sub <- nrow(subset)

  # Drop non-quasi-identifiers from the other subset
  otherSubset <- otherSubset[, ..quasiIdentifiers]
  n_other <- nrow(otherSubset)

  # print("subset after drop")
  # print(subset)
  # print("otherSubset after drop")
  # print(otherSubset)

  # Combine the partial data frames into one
  both_sets <- rbind(subset, otherSubset)
  # print("both sets")
  # print(both_sets)


  # Check the levels of variables
  levels_count <- lapply(both_sets, function(x) length(unique(x)))


  # Remove variables with only one level
  col_select <- both_sets[, levels_count > 1, drop=FALSE]
  # print("colselect")
  # print(col_select)
  both_sets <- both_sets[, ..col_select]

   # print("both sets after drop")
   # print(both_sets)

  # If no columns are present anymore, the dataframes are indentical
  if(ncol(both_sets) == 0){
    return(0)
  }

  # One-hot encode the combined data frame
  # dummies_both <- dummyVars(" ~ .", data = both_sets)
  # #print("between onehot")
  # as_numerical_both <- stats::predict(dummies_both, newdata = both_sets)

  #as_numerical_both = one_hot(both_sets)

  as_numerical_both = predict(onehot::onehot(both_sets, stringsAsFactors = TRUE, max_levels = 20), both_sets)

  # print("as numerical both")
  # print(as_numerical_both)


  # Normalize
  normalized_all = as.data.table(scale(as_numerical_both))
  # print("normalized all")
  # print(normalized_all)

  # Divide the normalized data frame into the original parts
  subset_normalized <- normalized_all[1:n_sub, ,drop = FALSE]
  otherSubset_normalized <- normalized_all[-(1:n_sub), ,drop = FALSE]

  # Take the colMeans
  mean_subset <- colMeans(subset_normalized)
  mean_otherSubset <- colMeans(otherSubset_normalized)
  # print("meansubs")
  # print(mean_subset)
  # print("meanotherS")
  # print(mean_otherSubset)

  # Calculate vector distance using Euclidean distance
  return(dist(rbind(mean_subset, mean_otherSubset)))

}



