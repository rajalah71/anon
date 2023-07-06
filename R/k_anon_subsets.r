#' Perform k-Anonymization on a dataset
#'
#' This function applies k-anonymization to a dataset by dividing it into subsets
#' based on quasi-identifiers and modifying the data to achieve k-anonymity
#'
#' @param data A data frame containing the sensitive data.
#' @param quasiIdentifiers A character vector specifying the column names of the quasi-identifiers.
#' @param diversityFunctions A named list of diversity functions for each quasi-identifier column.
#' @param k The desired level of k-anonymity.
#'
#' @return A k-anonymous data frame.
#'
#' @details The `kAnon` function applies k-anonymization on the input dataset `data`
#' by dividing it into subsets based on quasi-identifiers specified in `quasiIdentifiers`.
#' The function modifies the data in each subset to achieve k-anonymity
#' based on the diversity functions provided in `diversityFunctions`.
#'
#' @examples
#' \dontrun{
#' data(iris)
#' diversityFunctions <- list(Species = function(x) "*", Petal.Width = function(x) "*")
#' kAnonData <- kAnon(iris, quasiIdentifiers = c("Species", "Petal.Width"), diversityFunctions, k = 3)
#'}
#' @export
kAnon <- function(data, quasiIdentifiers, diversityFunctions, k) {

  # For runtime
  start_time <- Sys.time()

  # Check if the column names of quasiIdentifiers match the diversityFunctions
  if (!all(names(diversityFunctions) %in% quasiIdentifiers)) {
    stop("Column names of the quasi-identifier and diversity functions do not match.")
  }

  # Check if the dataset is already k-anonymouys
  if (is_k_anonymous(data, quasiIdentifiers, k)) {
    print("The dataset is already k-anonymous.")
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

    # If a given subset is already k-anonymous no actions are taken
    if(is_k_anonymous(subsets[[i]], quasiIdentifiers, k)){
      #print("BLock was alread k-anonymous")
      next
    }

    # Logical variable that determines how long subsets will be combined to object subsets[[i]]
    combine_more = TRUE
    while (combine_more) {
      # subsets[i] keeps increasing in size and subsets keep getting popped until subsets[i] can be mande k-anonymous given the quasi_ids and functions
      nearestSubsetIndex <- findNearestSubset(subsets[i], subsets, quasiIdentifiers)
      if(nearestSubsetIndex == 0){
        stop("All subsets combined and no k-anonymity obtained")
      }

      # Add the nearest subset to the current subset
      subsets[[i]] <- rbind(subsets[[nearestSubsetIndex]], subsets[[i]])

      # Remove the added subset from subsets
      subsets = subsets[-nearestSubsetIndex]

      # If we were to remove a previous subset we need to adjust the indexing
      if(nearestSubsetIndex < i){
        i = i - 1
      }

      # The subset may become k-anonymous just by increasing its size, so no further actions are taken if so
      if(is_k_anonymous(subsets[[i]], quasiIdentifiers, k)){
        break
      }

      # Temporary subset as dataframe to modify
      temp_subset = subsets[[i]]

      # Apply diversity functions in the order the columns were given, iteratively until the subset becomes k-anonymous
      for (u in seq_along(diversityFunctions)) {
        col = quasiIdentifiers[u]
        fun = diversityFunctions[[col]]
        temp_subset[col] <- lapply(temp_subset[col], fun)
      }

      if(is_k_anonymous(temp_subset, quasiIdentifiers, k)){

        # If the subset COULD be made k-anonymous, move onto next block but do NOT commit the changes
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

      # No more actions are taken than necessary to make the dataset k-anonymous
      if(is_k_anonymous(temp_subset, quasiIdentifiers, k)){
        subsets[[j]] = temp_subset
        break
      }
    }
  }


  # Combine the k-anonymous subsets into a single dataset
  kAnonData <- do.call(rbind, subsets)

  # Just a final check
  if (is_k_anonymous(kAnonData, quasiIdentifiers, k)) {
    shuffled = kAnonData[sample(nrow(kAnonData)), ]
    rownames(shuffled) = 1:nrow(shuffled)
    print(Sys.time() - start_time)
    return(shuffled)
  }

  else{
    print(Sys.time() - start_time)
    stop("Something went wrong.")
  }
}
