# Define the modified l_diverse function
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

# Define the make_l_diverse function
make_l_diverse <- function(df, quasi_id_cols, sensitive_cols, fun_list, l) {

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
