#' Min-Max scaling of a numeric vector.
#'
#' This function performs Min-Max scaling on a numeric vector (column), scaling
#' its values to a range between 0 and 1. Min-Max scaling is a common technique
#' used to transform data to a specific range, preserving the relative differences
#' between values.
#'
#' @param column A numeric vector to be scaled.
#'
#' @return A scaled numeric vector with values in the range [0, 1].
#'
#' @examples
#' column <- c(10, 20, 30, 40)
#' scaled_column <- minmax_scaler(column)
#' scaled_column
#' @export
minmax_scaler = function(column){
  # get the minimum and maximum values of the column
  min = min(column)
  max = max(column)
  # scale the column
  column = (column - min)/(max - min)
  return(column)
}

#--------------------------------------

#' Create an empty data frame with the same dimensions and column names as the input data.
#'
#' This function takes a data frame as input and returns a new empty data frame
#' with the same dimensions and column names as the input data. The returned data frame
#' will have no data but can be populated later with relevant information.
#'
#' @param data A data frame to replicate the structure from.
#'
#' @return An empty data frame with the same dimensions and column names as \code{data}.
#'
emptydf = function(data){
  new = as.data.frame(matrix(nrow = nrow(data), ncol = ncol(data)))
  colnames(new) = colnames(data)
  return(new)
}

#----------------------------------------

#' Shuffle the rows of a data frame.
#'
#' This function takes a data frame as input and returns a new data frame with
#' the same dimensions as the original data. The rows of the
#' returned data frame are shuffled randomly using the \code{sample} function,
#' effectively re-ordering the dataframe, as in shuffling a deck of cards.
#'
#' @param data A data frame to be shuffled.
#'
#' @return A new data frame with the same dimensions as data, but with the
#'         rows shuffled.
#'
#' @examples
#' data <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' shuffled_data <- shuffle(data)
#' shuffled_data
#' @export
shuffle = function(data){

  data = data[sample(nrow(data)),]
  rownames(data) = seq(nrow(data))

  return(data)
}

#---------------------------

#' Generate random numbers from a Laplace distribution.
#'
#' This function generates random numbers from a Laplace distribution
#' with given location and scale parameters.
#'
#' @param n The number of random samples to generate.
#' @param location The location parameter (mean) of the Laplace distribution.
#' @param scale The scale parameter (spread) of the Laplace distribution.
#'
#' @return A numeric vector containing \code{n} random numbers following
#' the Laplace distribution.
#'
#' @importFrom stats runif
#'
#' @examples
#' rlaplace(10, 0, 1)
#'
#' @export
rlaplace <- function(n, location, scale) {
  u <- runif(n, min = 0, max = 1) # Generate n uniform random numbers
  laplace_numbers <- location - scale * sign(u - 0.5) * log(1 - 2 * abs(u - 0.5))
  return(laplace_numbers)
}

#-------------------------------------------------------

#' Inverse One-Hot Encoding
#'
#' Perform the inverse operation to one-hot encoding, i.e.,
#'  from one-hot encoded data to the original data.
#'
#' @param data The one-hot encoded dataset.
#' @param names The names of the categorical variables in the original data.
#'                Names in the original are assumed to be mapped to "name=level"
#'                in the one hot encoded data given. Works well with
#'                "onehot" package.
#' @param sample Logical: Whether to sample from the numerical columns or take the max
#' @return The original data with categorical variables restored from one-hot encoding.
#'
#' @export
inverse_onehot = function(data, names, sample=FALSE){
  # the inverse operation to one hot encoding, i.e. from one hot encoded data to the original data
  # names: the names of the categorical variables in the original data
  # sample: if TRUE, the value will be sampled from the one_hot_columns instead of choosing the maximum value

  # iterate over the names of the categorical variables and combine the one hot encoded columns into one column
  for(name in names){
    # get the columns that are one hot encoded
    one_hot_columns = grep(name, names(data))

    # get the names of the grepped columns
    one_hot_columns_names = names(data)[one_hot_columns]

    # remove the first part of the column names, as in the one hot encoding the column names are of the form "name=level". We only need the level part
    one_hot_columns_names = gsub(paste0(name, "="), "", one_hot_columns_names)

    # combine the columns such that the value of the combined column is either the column with the maximum value or sampled using softmax probabilities
    if (sample) {
      combined_column = apply(data[, one_hot_columns], 1, function(x) {
        sample(one_hot_columns_names, 1, prob = x)
      })
    } else {
      combined_column = apply(data[, one_hot_columns], 1, function(x) {
        one_hot_columns_names[which(x == max(x))]
      })
    }

    # remove the one hot encoded columns
    data = data[, -one_hot_columns]

    # replace the numerical column with the combined categorical column
    data[, name] = combined_column
  }

  # order the columns in the original order using colmn reordered
  return(data)
}


#-------------------------------------------------------

#' Inverse Logit Function
#'
#' Calculate the inverse logit function.
#'
#' @param x The input value.
#' @return The result of the inverse logit function.
#'
#' @examples
#' inverse_logit(0)
#' inverse_logit(-2)
#' inverse_logit(2)
#' @export
inverse_logit = function(x){
  # inverse logit function
  return(exp(x) / (1 + exp(x)))
}

#-------------------------------------------------------

#' Get the minimum and maximum values of a numeric vector
#'
#' This function takes a numeric vector as input and returns the minimum and maximum values.
#'
#' @param numbers A numeric vector.
#'
#' @return A character string representing the minimum and maximum values in the format [min, max].
#'
#' @examples
#' numbers <- c(1, 2, 3)
#' getMinMax(numbers)
#'
#' @export
getMinMax <- function(numbers) {
  result <- range(numbers)
  output <- paste0("[", result[1], ",", result[2], "]")
  return(output)
}

#-------------------------------------------------------


#' Combine the lowest classes
#'
#' This function iteratively combines the fewest occuring categorical values in a column (k) times.
#'
#' @param column The column in which to combine the lowest classes.
#' @param k The maximum number of fewest occurances to combine.
#'
#' @return The column with the categorical values combined.
#'
#' @examples
#' column <- c("A", "B", "A", "C", "B", "D", "A", "C", "B")
#' combine_lowest_classes(column, 2)
#' @export
combine_lowest_classes <- function(column, k=1) {

  # Find the unique values in the column and raise error if k is too large
  unique_vals <- unique(column)
  if(k+1 > length(unique_vals)){
    return(column)
  }

  for (i in seq_len(k)) {

    # Count the number of occurrences of each unique value
    counts <- table(column)

    # Sort the counts in ascending order
    sorted_counts <- sort(counts)

    # Find the two lowest classes
    lowest_class1 <- names(sorted_counts)[1]
    lowest_class2 <- names(sorted_counts)[2]

    # Combine the two lowest classes
    new_class <- paste(lowest_class1, "or", lowest_class2)

    # Update the column with the new class
    column[column == lowest_class1 | column == lowest_class2] <- new_class

    # Update the counts with the new class
    counts[new_class] <- counts[lowest_class1] + counts[lowest_class2]

  }

  return(column)
}

#-------------------------------------------------------

#' Column Inserter
#'
#' Insert a column into a data frame at a specified index.
#'
#' @param data The data frame to insert the column into.
#' @param column The column to insert.
#' @param colname The name of the column to insert.
#' @param index The index at which to insert the column.
#' @return The data frame with the new column inserted at the specified index.
#'
column_inserter = function(data, column, colname, index){
  # insert a column into a data frame at a specified index
  # data: the data frame to insert the column into
  # column: the column to insert
  # colname: the name of the column to insert
  # index: the index at which to insert the column

  # get the column names of the data frame
  colnames = names(data)

  # insert the column name at the specified index
  colnames = c(colnames[1:index-1], colname, colnames[(index):length(colnames)])

  # insert the column at the specified index
  data = cbind(data[, 1:(index - 1)], column, data[, index:length(colnames(data))])

  # set the column names
  names(data) = colnames

  return(data)
}

#-------------------------------------------------------

#' Column Reordered
#'
#' Reorder the data frame columns according to the specified column names and indices.
#'
#' @param data The data frame to reorder.
#' @param names The column names in the desired order.
#' @param indicies The target indices for the corresponding column names.
#' @return The data frame with columns reordered as specified.
#'
column_reordered = function(data, names, indicies){
  # reorder the data frame so that names_location[i] column will be at index indicies[i] for all i
  for(i in seq_along(names)){
    # get the location of the column
    location = match(names[i], names(data))

    # if the location is not the same as the index, reorder the data frame
    if(location != indicies[i]){
      tobereplaced = data[, indicies[i]]
      data[, indicies[i]] = data[, location]
      data[, location] = tobereplaced
    }
  }

 return(data)

}

#------------------------------------------------------

#' Row Checker
#'
#' Check whether any row in the reference data is in the original data. If so, stop and raise an error.
#' Could be used to detect failure to anonymize data in some methods.
#'
#' @param original_data The original dataset.
#' @param reference_data The reference dataset.
#'
row_checker = function(original_data, reference_data){
  # check whether any row in the reference data are in the original data, is so stop
  for(i in seq(nrow(reference_data))){
    # checke whether the row is in the original data
    if(any(apply(original_data, 1, function(x) all(x == reference_data[i,])))){
      stop("An exact match was produced with the method. Try again (if random) or try another method alltogether.")
    }

  }
}

#------------------------------------------------------

#' Most Common Value
#'
#' Find the most common value in a column.
#'
#' @param column The input column.
#' @return A vector with the most common value repeated for the length of the input column.
#'
#' @examples
#' data <- c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4)
#' most_common(data)
#' @export
most_common = function(column){

  common = table(column)
  common = names(common)[which.max(common)]
  repeated = rep_len(common, length(column))
  return(repeated)

}

# --------------------------------------------------

#' Reorder Rows Based on Row Names
#'
#' @param df The input dataframe.
#' @return A dataframe with rows reordered based on the extracted row numbers.
#'
#'
reorder_rownames <- function(df) {
  # Extract the numbers from rownames and convert them to integers
  row_numbers <- as.integer(sub(".*\\.(\\d+)$", "\\1", rownames(df)))

  # Order the dataframe based on the extracted numbers
  df <- df[order(row_numbers), ]

  # Remove the rownames
  rownames(df) <- NULL

  return(df)
}

#-----------------------------------------------------

#' Get Mode
#'
#' Find the mode (most frequent value) in a vector.
#'
#' @param x The input vector.
#' @return The mode of the input vector.
#'
#' @examples
#' data <- c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4)
#' get_mode(data)
#'
#' @export
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

#-----------------------------------------------------

#' Sensitive Column Generalizer
#'
#' Divide a sensitive column into subsets of a specified size or into a number of subsets and generalize each subset.
#'
#' @param sensitiveColumn The input sensitive column.
#' @param subsetSize The size of subsets to create.
#' @param subsets The number of subsets to create.
#' @return A generalized version of the sensitive column with subsets replaced by their means (for numeric columns) or modes (for non-numeric columns).
#'
#' @examples
#' numeric_data <- c(1, 2, 3, 4, 5, 6)
#' sensitive_generalizer(numeric_data, subsetSize = 2)
#'
#' categorical_data <- c("A", "B", "B", "C", "C", "D")
#' sensitive_generalizer(categorical_data, subsetSize = 2)
#'
#'
#' @export
sensitive_generalizer = function(sensitiveColumn, subsetSize = NULL, subsets = NULL){

  # only one parameter is allowed as non null
  if(!is.null(subsetSize) && !is.null(subsets)) stop("Choose either individual subsetSize or the number of subsets.")

  # Divide the column into subsets to sizes of "subsetSize"

  if(!is.null(subsetSize)){
      # If numerical, store the original order, sort, divide into subsets of size "subsetSize" and then replace every value in every subset with its mean
    if(is.numeric(sensitiveColumn)){
      originalOrder = order(sensitiveColumn)
      sortedColumn = sort(sensitiveColumn)
      subsets = split(sortedColumn, ceiling(seq_along(sortedColumn)/subsetSize))
      for(i in 1:length(subsets)){
        subset_mean = mean(subsets[[i]])
        subsets[[i]] = rep(subset_mean, length(subsets[[i]]))
      }
      sortedColumn = unlist(subsets)
      sortedColumn = sortedColumn[order(originalOrder)]
      return(sortedColumn)
    }

    # If not numerical, store the original order, sort, divide into subsets of size "subsetSize" and then replace every value in every subset with its mode (function called "get_mode")
    else{
      originalOrder = order(sensitiveColumn)
      sortedColumn = sort(sensitiveColumn)
      subsets = split(sortedColumn, ceiling(seq_along(sortedColumn)/subsetSize))
      for(i in 1:length(subsets)){
        subset_mode = get_mode(subsets[[i]])
        subsets[[i]] = rep(subset_mode, length(subsets[[i]]))
      }
      sortedColumn = unlist(subsets)
      sortedColumn = sortedColumn[order(originalOrder)]
      return(sortedColumn)
    }
  }

  if(!is.null(subsets)){

    # Calculate subsetsize
    subsetSize = ceiling(length(sensitiveColumn) / subsets)

    # If numerical, store the original order, sort, divide into subsets of size "subsetSize" and then replace every value in every subset with its mean
    if(is.numeric(sensitiveColumn)){
      originalOrder = order(sensitiveColumn)
      sortedColumn = sort(sensitiveColumn)
      subsets = split(sortedColumn, ceiling(seq_along(sortedColumn)/subsetSize))
      for(i in 1:length(subsets)){
        subset_mean = mean(subsets[[i]])
        subsets[[i]] = rep(subset_mean, length(subsets[[i]]))
      }
      sortedColumn = unlist(subsets)
      sortedColumn = sortedColumn[order(originalOrder)]
      return(sortedColumn)
    }

    # If not numerical, store the original order, sort, divide into subsets of size "subsetSize" and then replace every value in every subset with its mode (function called "get_mode")
    else{
      originalOrder = order(sensitiveColumn)
      sortedColumn = sort(sensitiveColumn)
      subsets = split(sortedColumn, ceiling(seq_along(sortedColumn)/subsetSize))
      for(i in 1:length(subsets)){
        subset_mode = get_mode(subsets[[i]])
        subsets[[i]] = rep(subset_mode, length(subsets[[i]]))
      }
      sortedColumn = unlist(subsets)
      sortedColumn = sortedColumn[order(originalOrder)]
      return(sortedColumn)
    }

  }

}

#-----------------------------------------------------

#' ROC Plot
#'
#' Generate an ROC plot comparing the performance of a model on original and anonymized data.
#'
#' @param model The model trained on original data.
#' @param model_anon The model trained on anonymized data.
#' @param test The test dataset where the response variable is on the first column.
#' @param test_anon The test dataset for anon data (if different from original data, mainly here for RSA)
#'
#' @examples
#'\dontrun{
#' data("iris")
#' iris[,"Sepal.Length"] = ifelse(iris[,"Sepal.Length"] > 6, "1", "0")
#' rows = sample(c(TRUE, FALSE), replace = TRUE, nrow(iris), prob = c(0.7, 0.3))
#' train = iris[rows, ]
#' test = iris[!rows, ]
#' model = glm(as.factor(Sepal.Length) ~., data = train, family = binomial())
#' anon = spectral(train, cell_swap, shuffle = TRUE)
#' model_anon = glm(as.factor(Sepal.Length) ~., data = anon, family = binomial())
#' roc_plot(model, model_anon, test)
#' }
#'
#' @importFrom pROC roc coords
#' @importFrom ggplot2 ggplot geom_line geom_abline scale_x_continuous scale_y_continuous coord_equal theme_classic theme ggtitle scale_color_manual labs aes margin element_text element_rect element_blank
#'
#' @export
roc_plot = function(model, model_anon, test, test_anon = NULL){

  # If not provided, use the same test for both
  if(is.null(test_anon)) test_anon = test

  model_pred = predict(model, (test[,-1]))
  anon_pred = predict(model_anon, (test_anon[,-1]))

  roc1 <- roc(as.factor(test[,1]), model_pred, auc = TRUE)
  roc2 <- roc(as.factor(test_anon[,1]), anon_pred, auc = TRUE)

  df <- rbind(cbind(model = "Alkuperäinen aineisto", coords(roc1)),
              cbind(model = "Anonyymi aineisto", coords(roc2)))

  ggplot(df, aes(1 - specificity, sensitivity, color = model)) +
    geom_line(aes(), size = 1, linewidth = 0.1) +
    geom_abline() +
    scale_x_continuous("1 - tarkkuus") +
    scale_y_continuous("Herkkyys") +
    coord_equal(expand = FALSE) +
    theme_classic(base_size = 15) +
    theme(plot.margin = margin(10, 30, 10, 10)) +
    ggtitle("ROC-käyrät", ) +
    theme(plot.title = element_text(size = 25)) +
    scale_color_manual(values = c("Alkuperäinen aineisto" = "black", "Anonyymi aineisto" = "red")) +
    theme(legend.position = c(0.9, 0.1), legend.justification = c(1, 0))    +
    labs(color = "Mallit", subtitle = paste0(paste0("Alkuperäinen aineisto, AUC = ", round(roc1$auc, 3)), paste0(". Anonyymi aineisto, AUC = ", round(roc2$auc, 3), ".")), size = 0.1) +
    theme(plot.subtitle = element_text(size = 13))+
    theme( panel.border = element_rect(colour = "black", fill=NA)) +
    theme(legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"), legend.text = element_text(size=15))


}

#-----------------------------------------------------

#' Test R-squared for Multiple Models
#'
#' Calculate R-squared values for multiple models on test data.
#'
#' @param model_list A list of models to evaluate.
#' @param testData The test dataset.
#' @param responseVar The name of the response variable.
#'
#' @return A named list of R-squared values for each model.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' model_list <- list(model1, model2, model3)
#' testRsquared(model_list, test_data, "response_variable")
#'}
#' @export
testRsquared = function(model_list, testData, responseVar){

  # Get the model names and initialize a vector for results
  names = names(model_list)
  results = rep(NA, length(model_list))

  # Calculate R-squared for each model on the test data
  test_mean = mean(testData[, responseVar])
  for(i in seq_along(model_list)){
    model_preds = predict(model_list[[i]], testData[,colnames(testData) != responseVar])
    ss_res = sum((testData[, responseVar] - model_preds)^2)
    ss_tot = sum((testData[, responseVar] - test_mean)^2)
    results[i] = 1 - ss_res/ss_tot
  }

  # Return the results
  return(cbind(names, results))
}

# MEDIANS -----------------------------------------------------



#' Median Difference in Column Means
#'
#' Calculate the median difference in column means between data frames in a list, scaled by a reference data frame.
#'
#' @param dataframelist A list of data frames to compare.
#'
#' @return A list containing two elements: "Medians" (median differences in column means) and "MADs" (median absolute deviations of column means).
#' @importFrom stats median mad sd
medianDiffsinMean = function(dataframelist){
  # Drop non-numeric columns
  dataframelist = lapply(dataframelist, function(df) df[, sapply(df, is.numeric)])

  # Get names of the dataframes
  names = names(dataframelist)

  # The first item in the list is the reference dataframe. Calculate colmeans and vars for each column.
  ref_df = dataframelist[[1]]
  ref_means = colMeans(ref_df)
  ref_sds = sapply(ref_df, sd)

  # Scale the other dataframes with the reference means and sds
  scaled_dataframelist = lapply(dataframelist[-1], function(df) scale(df, ref_means, ref_sds))

  # calculate column means for each other dataframe of the list.
  other_means = lapply(scaled_dataframelist, colMeans)

  # Calculate absolute distances from the reference means for each dataframe to colmeans of 0
  abs_diffs = lapply(other_means, function(om) abs(om))

  # Calculate the median difference in colmeans for each dataframe in the list
  med_diffs = lapply(abs_diffs, function(ad) median(ad)) # returned

  # subtract the the med_diffs item i from the abs_diffs item i
  abs_diffs = lapply(seq_along(med_diffs), function(i) median(abs(abs_diffs[[i]] - med_diffs[[i]]))) # returned

  # Name the list items
  names(abs_diffs) = names[-1]

  # return
  return(list("Medians" = unlist(med_diffs), "MADs" = unlist(abs_diffs)))

  #print(med_diffs)

  # Calculate the absolute distance to their corresponding median in each dataset and take a median from that
  # abs_distances = list()
  # for(i in seq_along(med_diffs)){
  #   abs_distances[[i]] = median(abs(abs_diffs[[i]] - med_diffs[[i]]))
  # }
  #
  # print(abs_distances)

  # Calculate median of abs_diffs
  #med_abs_diffs = sapply(abs_diffs, function(ad) median(ad)) # returned

  #return
  #return(list("Medians" = med_diffs, "MADs" = med_abs_diffs))

  # # Calculate the Median Absolute Deviation for colmeans for each dataset
  # df_mads = lapply(other_means, function(df) mad(df, constant = 1, center = rep(0, length(ref_means))))
  #
  # # Calculate the median difference in colmeans for each dataframe in the list
  # med_diffs = sapply(other_means, function(om) median(abs(om)))
  #
  # # Return a named vector of median differences
  # names(med_diffs) = names[-1]
  # return(list("Medians" = med_diffs, "MADs" = unlist(df_mads)))
}


#' Median Difference in Column Variances
#'
#' Calculate the median difference in column variances between data frames in a list, scaled by a reference data frame.
#'
#' @param dataframelist A list of data frames to compare.
#'
#' @return A list containing two elements: "Medians" (median differences in column variances) and "MADs" (median absolute deviations of column variances).
#'
#' @importFrom stats median mad sd
medianDiffsinVar = function(dataframelist){

  # Drop non-numeric columns
  dataframelist = lapply(dataframelist, function(df) df[, sapply(df, is.numeric)])

  # Get names of the dataframes
  names = names(dataframelist)

  # The first item in the list is the reference dataframe. Calculate colmeans and vars for each column.
  ref_df = dataframelist[[1]]
  ref_means = colMeans(ref_df)
  ref_sds = sapply(ref_df, sd)

  # Scale the other dataframes with the reference means and sds
  scaled_dataframelist = lapply(dataframelist[-1], function(df) scale(df, ref_means, ref_sds))

  # calculate column variances for each other dataframe of the list.
  other_vars = lapply(scaled_dataframelist, colVars)

  # Calculate absolute distances from the reference vars for each dataframe to colvars of 1
  abs_diffs = lapply(other_vars, function(ov) abs(ov - rep(1, length(ref_means))))

  # Calculate the median difference in colvars for each dataframe in the list
  med_diffs = lapply(abs_diffs, function(ad) median(ad)) # returned

  # subtract the the med_diffs item i from the abs_diffs item i
  abs_diffs = lapply(seq_along(med_diffs), function(i) median(abs(abs_diffs[[i]] - med_diffs[[i]]))) # returned

  # Name the list items
  names(abs_diffs) = names[-1]

  # return
  return(list("Medians" = unlist(med_diffs), "MADs" = unlist(abs_diffs)))

  # # Drop non-numeric columns
  # dataframelist = lapply(dataframelist, function(df) df[, sapply(df, is.numeric)])
  #
  # # Get names of the dataframes
  # names = names(dataframelist)
  #
  # # The first item in the list is the reference dataframe. Calculate colmeans and vars for each column.
  # ref_df = dataframelist[[1]]
  # ref_means = colMeans(ref_df)
  # ref_sds = sapply(ref_df, sd)
  #
  # # Scale the other dataframes with the reference means and sds
  # scaled_dataframelist = lapply(dataframelist[-1], function(df) scale(df, ref_means, ref_sds))
  #
  # # calculate column vars for each other dataframe of the list.
  # other_vars = lapply(scaled_dataframelist, colVars)
  #
  # # Calculate the Median Absolute Deviation for colvars for each dataset
  # df_mads = lapply(other_vars, function(df) mad(df, constant = 1, center = rep(1, length(ref_means))))
  #
  # # Calculate the median difference in colvars for each dataframe in the list
  # med_diffs = sapply(other_vars, function(ov) median(abs(ov-1)))
  #
  # # Return a named vector of median differences
  # names(med_diffs) = names[-1]
  # return(list("Medians" = med_diffs, "MADs" = unlist(df_mads)))
}

#' Median Difference in Correlation Matrices
#'
#' Calculate the median difference in correlation matrices between data frames in a list, scaled by a reference data frame.
#'
#' @param dataframelist A list of data frames to compare.
#'
#' @return A list containing two elements: "Medians" (median differences in correlation matrices) and "MADs" (median absolute deviations of correlation matrices).
#'
#' @importFrom stats median mad cor sd
medianDiffsinCor = function(dataframelist){
  # Drop non-numeric columns
  dataframelist = lapply(dataframelist, function(df) df[, sapply(df, is.numeric)])

  # Get names of the dataframes
  names = names(dataframelist)

  # The first item in the list is the reference dataframe. Calculate colmeans and vars for each column. Only keep the upper triangle.
  ref_df = dataframelist[[1]]
  ref_means = colMeans(ref_df)
  ref_sds = sapply(ref_df, sd)
  ref_cor = cor(scale(ref_df))
  ref_cor[lower.tri(ref_cor)] <- NA
  diag(ref_cor) <- NA

  # Scale the other dataframes with the reference means and sds
  scaled_dataframelist = lapply(dataframelist[-1], function(df) scale(df, ref_means, ref_sds))

  # calculate correlation matrix for each other dataframe of the list. Only keep the upper triangle.
  other_cor = lapply(scaled_dataframelist, cor)
  other_cor = lapply(other_cor, function(x) {x[lower.tri(x)] <- NA; diag(x) <- NA; x})

  # Calculate the Median Absolute Deviation for colmeans for each dataset
  df_mads = lapply(other_cor, function(df) mad(df-ref_cor, constant = 1, na.rm = TRUE))

  # Calculate the median difference in correlation matrix for each dataframe in the list
  med_diffs = sapply(other_cor, function(oc) median(abs(oc - ref_cor), na.rm = TRUE))

  # Return a named vector of median differences
  names(med_diffs) = names[-1]
  return(list("Medians" = med_diffs, "MADs" = unlist(df_mads)))
}

#' Calculate Median Differences in Means, Variances, and Correlation Matrices
#'
#' Calculate median differences in means, variances, and correlation matrices between data frames in a list.
#'
#' @param dataframelist A list of data frames to compare.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' data_list <- list(reference_data, data_frame1, data_frame2)
#' mediansAll(data_list)
#'}
#' @export
mediansAll = function(dataframelist){
  print("Means:")
  print(medianDiffsinMean(dataframelist))
  print("Vars:")
  print(medianDiffsinVar(dataframelist))
  print("Cors:")
  print(medianDiffsinCor(dataframelist))
}

#colVars-------------------------------------------------

#' Calculate Column Variances
#'
#' Calculate the variances of columns in a data frame.
#'
#' @param df A data frame.
#' @param na.rm Logical. Should missing values (NA) be removed?
#'
#' @return A numeric vector of variances for each column.
#'
#' @examples
#' \dontrun{
#' variances <- colVars(data_frame)
#' }
#' @export
colVars <- function(df, na.rm = FALSE) {
  apply(df, 2, var, na.rm = na.rm)
}

