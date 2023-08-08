#' Check if a data frame is l-diverse with respect to specified quasi-identifier and sensitive columns
#'
#' This function takes a data frame and checks if it satisfies l-diversity with respect to the
#' specified quasi-identifier and sensitive columns. It groups the data by the quasi-identifier
#' columns, counts the number of distinct values in each group for the sensitive columns, and
#' checks if all groups have at least l distinct values for each sensitive column.
#'
#' @param df The input data frame.
#' @param quasi_id_cols A character vector specifying the names of the quasi-identifier columns.
#' @param sensitive_cols A character vector specifying the names of the sensitive columns.
#' @param l The desired minimum number of distinct values for each sensitive column within each group.
#'
#' @return TRUE if the data frame is l-diverse, FALSE otherwise.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr across
#' @importFrom dplyr n_distinct
#'
#' @examples
#' df <- data.frame(
#'   age = c(25, 30, 35, 40, 45),
#'   gender = c("M", "M", "F", "F", "M"),
#'   disease = c("A", "A", "B", "C", "C")
#' )
#'
#' is_l_diverse(df, c("age", "gender"), "disease", 2)
#'
#'
is_l_diverse = function(df, quasi_id_cols, sensitive_cols, l) {
  # Load the required libraries
  #suppressMessages(library(dplyr))

  #Silence the summary function message
  #options(dplyr.summarise.inform = FALSE)

  # Group the data by the quasi-identifier and sensitive columns
  grouped_df = df[, c(quasi_id_cols, sensitive_cols)] %>% group_by(across(all_of(quasi_id_cols)))
  #print(grouped_df)

  # Count the number of rows in each group
  count_df = grouped_df %>% summarise(across(all_of(sensitive_cols), n_distinct))
  #print(count_df)

  # Check if all groups have at least l rows
  all(count_df[, sensitive_cols] >= l)
}

#' Make a data frame l-diverse by applying functions to quasi-identifier and sensitive columns
#'
#' This function takes a data frame and applies a set of functions to the specified
#' quasi-identifier columns in order to achieve l-diversity with respect to the sensitive columns.
#' It checks if the data frame is already l-diverse and returns a random permutation of the data in that case.
#' If not, it applies the functions iteratively to the columns until the desired level of l-diversity is achieved
#' or it is not possible with the given functions.
#'
#' @param df The input data frame.
#' @param quasi_id_cols A character vector specifying the names of the quasi-identifier columns.
#' @param sensitive_cols A character vector specifying the names of the sensitive columns.
#' @param fun_list A named list of functions corresponding to the quasi-identifier columns.
#'   Each function should take a vector as input and return a modified vector with the same length.
#' @param l The desired minimum number of distinct values for each sensitive column within each group.
#'
#' @return A data frame that is l-diverse with respect to the specified quasi-identifier and sensitive columns,
#'   or an error is thrown if the desired level of l-diversity cannot be achieved.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr all_of
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   age = c(25, 30, 35, 40, 45),
#'   gender = c("M", "M", "F", "F", "M"),
#'   disease = c("A", "A", "B", "C", "C")
#' )
#'
#' # Define functions to generalize age and disease
#' age_fun <- mean
#' gender_fun <- function(x) "*"
#'
#' # Apply make_l_diverse function
#' l_diverse_df <- make_l_diverse_columnwise(df, c("age", "gender"), "disease", list(age = age_fun, gender = gender_fun), 2)
#'}
#'
make_l_diverse_columnwise <- function(df, quasi_id_cols, sensitive_cols, fun_list, l) {

  # Check if the data is already l-diverse
  if (is_l_diverse(df, quasi_id_cols, sensitive_cols, l)) {
    cat("Data was already", l,"-diverse!\n")
    # Return a random permutation of the data
    shuffled = df[sample(nrow(df)), ]
    rownames(shuffled) = 1:nrow(shuffled)
    return(shuffled)
  }

  # Apply the functions to the corresponding columns and check if the resulting data frame is l-diverse after each function
  for (i in seq_along(quasi_id_cols)) {
    col <- quasi_id_cols[i]
    fun <- fun_list[[col]]
    df[col] <- lapply(df[col], fun)

    if (is_l_diverse(df, quasi_id_cols, sensitive_cols, l)) {
      cat("Data was made", l,"-diverse with respect to (", quasi_id_cols, ") and (", sensitive_cols, ")\n")
      # Return a random permutation of the data
      shuffled = df[sample(nrow(df)), ]
      rownames(shuffled) = 1:nrow(shuffled)
      return(shuffled)
    }
  }

  stop("Data frame could not be l-diverse with given functions!\n  Try to use coarser functions!")
}
