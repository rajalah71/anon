#' Check if a data frame is k-anonymous with respect to specified quasi-identifier columns
#'
#' This function takes a data frame and checks if it satisfies k-anonymity with respect
#' to the specified quasi-identifier columns. It groups the data by the quasi-identifiers,
#' counts the number of rows in each group, and checks if all groups have at least k rows.
#'
#' @param data The input data frame.
#' @param quasi_id_cols A character vector specifying the names of the quasi-identifier columns.
#' @param k The desired level of k-anonymity.
#'
#' @return TRUE if the data frame is k-anonymous, FALSE otherwise.
#'
#' @examples
#' df <- data.frame(
#'   age = c(25, 30, 35, 40, 45),
#'   gender = c("M", "M", "F", "F", "M"),
#'   income = c(50000, 60000, 70000, 80000, 90000)
#' )
#'
#' is_k_anonymous(df, c("age", "gender"), 2)
#'
#' @export
is_k_anonymous = function(data, quasi_id_cols, k) {

  # Group the data by the quasi-identifiers
  groups <- split(data, data[, quasi_id_cols], drop = TRUE)

  # Check if each group has at least k values
  for (group in groups) {
    if (nrow(group) < k) {
      return(FALSE)
    }
  }

  return(TRUE)

}


#' Perform k-Anonymization on a dataset
#'
#' This function applies k-anonymization to a dataset by dividing it into subsets
#' based on quasi-identifiers and modifying the data to achieve k-anonymity
#'
#' @param data A data frame containing the sensitive data.
#' @param k The desired level of k-anonymity.
#' @param quasiIdentifiers A character vector specifying the column names of the quasi-identifiers (default: NULL).
#' @param anonymizationFunctions A named list of anonymization functions for each quasi-identifier column (default: NULL).
#' @param shuffle (Default TRUE) A parameter which determines whether the data
#'                  will be shuffled or not before returning. Will give warning
#'                  uf FALSE, as no data should be released without shuffling.
#'                  For diagnostic use, or for calculating reidentification rate.
#'
#'
#' @return A k-anonymous data frame.
#'
#' @importFrom utils flush.console
#'
#' @details The `kAnon` function applies k-anonymization on the input dataset `data`
#' by dividing it into subsets based on quasi-identifiers specified in `quasiIdentifiers`.
#' The function modifies the data in each subset to achieve k-anonymity
#' based on the anonymization functions provided in `anonymizationFunctions`.
#'
#' @examples
#' \dontrun{
#' data(iris)
#' anonymizationFunctions <- list(Species = function(x) "*", Petal.Width = function(x) "*")
#' kAnonData <- kAnon(iris, quasiIdentifiers = c("Species", "Petal.Width"), anonymizationFunctions, k = 3)
#'}
#' @export
kAnon <- function(data, k, quasiIdentifiers = NULL, anonymizationFunctions = NULL, shuffle = TRUE) {

  if(shuffle == FALSE){
    warning("Shuffle is FALSE, do not release data.")
  }

  # For runtime
  start_time <- Sys.time()

  # If quasiIdentifiers and anonymizationFunctions are not provided, calculate the cardinality of each column
  # and identify numeric and categorical columns for default behavior.
  if (is.null(quasiIdentifiers) || is.null(anonymizationFunctions)) {
    col_cardinality <- sapply(data, function(x) length(unique(x)))
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    categorical_cols <- names(data)[!sapply(data, is.numeric)]
  }

  # Use default behavior if quasiIdentifiers and anonymizationFunctions are not provided
  if (is.null(quasiIdentifiers)) {
    quasiIdentifiers <- names(col_cardinality)[order(-col_cardinality)]
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
    cat("Iteration:", print_counter, "/", subset_length, "\r")
    flush.console()

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
      for (u in seq_along(anonymizationFunctions)) {
        col = quasiIdentifiers[u]
        fun = anonymizationFunctions[[col]]
        temp_subset[col] <- lapply(temp_subset[col], fun)
      }

      if(is_k_anonymous(temp_subset, quasiIdentifiers, k)){

        # If the subset COULD be made k-anonymous, move onto next block but do NOT commit the changes
        combine_more = FALSE
      }
    }
  }

  # Finally actually apply the anonymization functions to the subsets and return them as a dataframe
  for (j in seq_along(subsets)) {
    # Working subset
    temp_subset <- subsets[[j]]

    # Iterate over anonymization functions
    for (s in seq_along(anonymizationFunctions)) {
      col <- names(anonymizationFunctions)[s]
      fun <- anonymizationFunctions[[col]]
      temp_subset[col] <- lapply(temp_subset[col], fun)

      # No more actions are taken than necessary to make the dataset k-anonymous
      if (is_k_anonymous(temp_subset, quasiIdentifiers, k)) {
        subsets[[j]] <- temp_subset
        break
      }
    }
  }

  # Combine the k-anonymous subsets into a single dataset
  kAnonData <- do.call(rbind, subsets)

  # Just a final check
  if (is_k_anonymous(kAnonData, quasiIdentifiers, k)) {
    # shuffled <- kAnonData[sample(nrow(kAnonData)), ]
    # rownames(shuffled) <- 1:nrow(shuffled)
    # print(Sys.time() - start_time)
    # return(shuffled)

    if(shuffle == TRUE){
      return(shuffle(kAnonData))
    } else{
      return(kAnonData)
    }

  } else {
    print(Sys.time() - start_time)
    stop("Something went wrong.")
  }
}

#---------------------------------------------------------------------------

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
#' @importFrom utils flush.console
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
kAnon_legacy <- function(data, quasiIdentifiers, diversityFunctions, k) {

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
  # cat("Iterating over all subsets:", subset_length, " iterations at most. \n" )

  while (TRUE) {
    # For printing the progress
    print_counter = print_counter + 1
    cat("Iteration:", print_counter, "/", subset_length, "\r")
    flush.console()

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
    # shuffled = kAnonData[sample(nrow(kAnonData)), ]
    # rownames(shuffled) = 1:nrow(shuffled)
    # print(Sys.time() - start_time)
    # return(shuffled)

    if(shuffle == TRUE){
      return(shuffle(kAnonData))
    } else{
      return(kAnonData)
    }
  }

  else{
    print(Sys.time() - start_time)
    stop("Something went wrong.")
  }
}

