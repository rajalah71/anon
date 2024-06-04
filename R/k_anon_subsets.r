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
#' This function applies (user provided or default) generalization functions to a dataset to achieve k-anonymity.
#'
#' @param data The input dataset.
#' @param k The desired level of k-anonymity.
#' @param quasiIdentifiers A character vector specifying the column names of the quasi-identifiers (default: NULL). The first column named will be modified first and the last last:
#' name the columns in ascending order of importance. If quasi-identifiers are nor provided, all columns will be assumed to be quasi-identifiers and modified in descending order of cardinality.
#' @param anonymizationFunctions A named list of anonymization functions for each quasi-identifier column (default: NULL). If not provided, mean and mode will be used for numericals and non-numericals respectively.
#' @param shuffle Whether to shuffle the dataset before returning. Warning if FALSE, used to calculate empirical reidentification rate.
#'
#'
#'
#' @return A k-anonymous data frame.
#'
#' @importFrom utils flush.console
#'
#' @details The `kAnon` function applies k-anonymization on the input dataset `data`
#' by dividing it into subsets based on quasi-identifiers specified in `quasiIdentifiers`.
#' The function modifies the data in each subset to achieve k-anonymity
#' based on the anonymization functions provided in `anonymizationFunctions`. Quasi-identifiers are
#' columns which combined can identify a person in the data. Quasi-identifiers
#' include but are not limited to: age, gender, zip-code, profession...
#'
#' @examples
#' \dontrun{
#' data(iris)
#' anonymizationFunctions <- list(Species = function(x) "*", Petal.Width = function(x) mean(x))
#' kAnonData <- kAnon(iris, quasiIdentifiers = c("Species", "Petal.Width"), anonymizationFunctions, k = 3)
#'}
#' @export
kAnon <- function(data, k, quasiIdentifiers = NULL, anonymizationFunctions = NULL, shuffle = FALSE) {

  # For runtime
  start_time <- Sys.time()

  # if shuffle FALSE, warning
  if(!shuffle) warning("Shuffle is FALSE. Use 'anon::shuffle()' before publishing data.\n")

  # If quasiIdentifiers or anonymizationFunctions are not provided, calculate the cardinality of each column
  # and identify numeric and categorical columns
  if (is.null(quasiIdentifiers) || is.null(anonymizationFunctions)) {
    col_cardinality <- sapply(data, function(x) length(unique(x)))
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    categorical_cols <- names(data)[!sapply(data, is.numeric)]
  }

  # Use all columns as quasi-identifiers and order them based on their cardinality if quasi-identifiers are not provided.
  if (is.null(quasiIdentifiers)) {
    quasiIdentifiers <- names(col_cardinality)[order(-col_cardinality)]
  }

  # If anonymization functions are not provided, use mean for numerical columns and mode for un-ordered categoricals.
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
        break
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
    print(Sys.time() - start_time)
    if(shuffle) return(shuffle(kAnonData))
    else return(reorder_rownames(kAnonData))
  } else {
    print(Sys.time() - start_time)
    stop("k-anonymity could not be obtained.")
  }
}

