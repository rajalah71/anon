# one-hot encoding function that takes a dataframe as a parameter and automatically detects the categorical variable columns

one_hot <- function(df) {
  # get the column names of the categorical variables
  cat_cols <- names(df)[sapply(df, is.factor)]
  # one-hot encode the categorical variables
  df <- df %>% 
    mutate_at(vars(cat_cols), funs(as.integer(factor(.))))
  return(df)
}
