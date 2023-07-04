onehot_encode_my <- function(data) {
  # Identify columns to one-hot encode
  columns_to_encode <- data %>%
    select_if(is.character) %>%
    colnames

  columns_to_encode <- which(colnames(data) %in% columns_to_encode)

  print(columns_to_encode)

  if (length(columns_to_encode) == 0) {
    # No columns to encode
    return(data)
  }

  # One-hot encode each column
  for (col_index in columns_to_encode) {
    col_name <- names(data)[col_index]
    encoded_cols <- model.matrix(~ . - 1, data = data[, col_index, drop = FALSE])
    col_names <- colnames(encoded_cols)

    # Remove original column
    data <- data[, -col_index, drop = FALSE,]

    # Append encoded columns to dataframe
    data <- cbind(data, encoded_cols)

    # Rename encoded columns
    colnames(data)[(ncol(data) - length(col_names) + 1):ncol(data)] <- paste(col_name, col_names, sep = ".")
  }

  return(data)
}
