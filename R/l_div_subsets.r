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

isLdiverse = function(df, quasi_id_cols, sensitive_cols, l) {
  # Load the required libraries

  #Silence the summary function message
  options(dplyr.summarise.inform = FALSE)

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



# This function tries to make a given dataset (dataframe) into an l-diverse version of it given a set of quasi-identifiers (a vector of corresponding column names)
# and a sensitive attribute (column name as string) and a
# list of diversity functions (list with function names exactly the same as the column they are supposed to operate on) and an
# integer l which determines how l-diverse the dataset will be. The greater the l, the greater the anononymity of the dataset and lower the utility of it.

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
  subsets <- base::split(data, data[, quasiIdentifiers], drop = TRUE)

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

# Takes two lists and the quasi ids as parameters
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

#-------------------

# Function to calculate the distance between mean vectors of two subsets (must be inputed as lits)
matrix_distance <- function(subset, otherSubset, quasiIdentifiers) {

  # Package needed for onehot encoding
  #require(caret)

  # Convert subset and otherSubset to data frames if they are lists

  subset <- data.table::rbindlist(subset)

  otherSubset <- data.table::rbindlist(otherSubset)

  # Drop non-quasi-identifiers from the subset
  subset <- subset[, quasiIdentifiers, drop=FALSE]
  n_sub <- nrow(subset)

  # Drop non-quasi-identifiers from the other subset
  otherSubset <- otherSubset[, quasiIdentifiers, drop=FALSE]
  n_other <- nrow(otherSubset)

  # Combine the partial data frames into one
  both_sets <- rbind(subset, otherSubset)

  # Check the levels of variables
  levels_count <- lapply(both_sets, function(x) length(unique(x)))

  # Remove variables with only one level
  both_sets <- both_sets[, levels_count > 1, drop=FALSE]

  # One-hot encode the combined data frame
  dummies_both <- caret::dummyVars(" ~ .", data = both_sets)
  as_numerical_both <- stats::predict(dummies_both, newdata = both_sets)

  #print("checkpoint")

  # Normalize the data frame
  #normalized_all <- apply(as_numerical_both, 2, function(x) {
   # if (sd(x) != 0) {
    #  (x - mean(x)) / sd(x)
    #} else {
    #  x
    #}
  #})

  ### NEW ###
  normalized_all = data.table::as.data.table(scale(as_numerical_both))
  #print(normalized_all)
  #normalized_all <- normalized_all[,which(unlist(lapply(normalized_all, function(x)!all(is.na(x))))),with=F]
  #print(normalized_all)
  #print("checkpoint1.5")
  ### NEW ###

  # Divide the normalized data frame into the original parts
  subset_normalized <- normalized_all[1:n_sub, ,drop = FALSE]
  otherSubset_normalized <- normalized_all[-(1:n_sub), ,drop = FALSE]
  #print("checkpoint2")

  mean_subset <- colMeans(subset_normalized)

  mean_otherSubset <- colMeans(otherSubset_normalized)


  #print(str(mean_subset))

  #print(str(mean_otherSubset))

  # Calculate vector distance using Euclidean distance
  return(stats::dist(rbind(mean_subset, mean_otherSubset)))
}
