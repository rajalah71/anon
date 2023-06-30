#' Check if a data frame is k-anonymous with respect to specified quasi-identifier columns
#'
#' This function takes a data frame and checks if it satisfies k-anonymity with respect
#' to the specified quasi-identifier columns. It groups the data by the quasi-identifiers,
#' counts the number of rows in each group, and checks if all groups have at least k rows.
#'
#' @param df The input data frame.
#' @param quasi_id_cols A character vector specifying the names of the quasi-identifier columns.
#' @param k The desired level of k-anonymity.
#'
#' @return TRUE if the data frame is k-anonymous, FALSE otherwise.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr n
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
is_k_anonymous = function(df, quasi_id_cols, k) {

  # Group the data by the quasi-identifiers
  grouped_df = df %>% group_by(across(all_of(quasi_id_cols)))
  #print(grouped_df)

  # Count the number of rows in each group
  count_df = grouped_df %>% summarise(count = n())
  #print(count_df)

  # Check if all groups have at least k rows
  all(count_df$count >= k)
}

#-----------------

#' Make a data frame k-anonymous by applying functions to quasi-identifier columns
#'
#' This function takes a data frame and applies a set of functions to the specified
#' quasi-identifier columns in order to achieve k-anonymity. It checks if the data frame
#' is already k-anonymous and returns a random permutation of the data in that case. If
#' not, it applies the functions iteratively to the columns until the desired level of
#' k-anonymity is achieved or it is not possible with the given functions.
#'
#' @param df The input data frame.
#' @param quasi_id_cols A character vector specifying the names of the quasi-identifier columns.
#' @param fun_list A named list of functions corresponding to the quasi-identifier columns.
#'   Each function should take a vector as input and return a modified vector with the same length.
#' @param k The desired level of k-anonymity.
#'
#' @return A data frame that is k-anonymous with respect to the specified quasi-identifier columns,
#'   or an error is thrown if the desired level of k-anonymity cannot be achieved.
#'
#' @examples
#' df <- data.frame(
#'   age = c(25, 30, 35, 40, 45),
#'   gender = c("M", "M", "F", "F", "M"),
#'   income = c(50000, 60000, 70000, 80000, 90000)
#' )
#'
#' # Define functions to generalize age and income
#' age_fun <- function(x) floor(x / 10) * 10
#' income_fun <- function(x) round(x, -3)
#'
#' # Apply make_k_anonymous function
#' k_anon_df <- make_k_anonymous(df, c("age", "income"), list(age = age_fun, income = income_fun), 2)
#'
#' @export
make_k_anonymous <- function(df, quasi_id_cols, fun_list, k) {

  # Check if the data is already l-diverse
  if (is_k_anonymous(df, quasi_id_cols, k)) {
    cat("Data was already", k,"-anonymous!\n")
    # Return a random permutation of the data
    shuffled = df[sample(nrow(df)), ]
    rownames(shuffled) = 1:nrow(shuffled)
    return(shuffled)
  }

  # Apply the functions to the corresponding columns and check if the resulting data frame is k-anonymous after each function
  for (i in seq_along(quasi_id_cols)) {
    col <- quasi_id_cols[i]
    fun <- fun_list[[col]]
    df[[col]] <- fun(df[[col]])

    if (is_k_anonymous(df, quasi_id_cols, k)) {
      cat("Data was made", k,"-anonyous with respect to (", quasi_id_cols, ")\n")
      # Return a random permutation of the data
      shuffled = df[sample(nrow(df)), ]
      rownames(shuffled) = 1:nrow(shuffled)
      return(shuffled)
    }
  }

  stop("Data frame could not be k-anonymous with given functions!\n  Try to use coarser functions!")
}




