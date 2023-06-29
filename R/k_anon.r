# Define the modified l_diverse function
is_k_anonymous = function(df, quasi_id_cols, k) {
  # Load the required libraries
  #suppressMessages(library(dplyr))

  #Silence the summary function message
  #options(dplyr.summarise.inform = FALSE)

  # Group the data by the quasi-identifiers
  grouped_df = df %>% group_by(across(all_of(quasi_id_cols)))
  #print(grouped_df)

  # Count the number of rows in each group
  count_df = grouped_df %>% summarise(count = n())
  #print(count_df)

  # Check if all groups have at least k rows
  all(count_df$count >= k)
}

# Define the make_k_anonymous function
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


