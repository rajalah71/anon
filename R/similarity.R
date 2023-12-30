# MEANS -----------------------------------------------------



#' Mean Difference in Column Means
#'
#' Calculate the mean difference in column means between data frames in a list, each scaled by a reference data frame.
#'
#' @param dataframelist A list of data frame lists. Each item contains a list of data frames; an original data, anonymized data and test data.
#'
#' @return A list containing two elements: "Mean" (mean difference in column means for all dataframes) and "Sd" (standard deviations of column means).
#' @importFrom stats sd
meanDiffsinMean = function(dataframelist){

  # Drop non-numeric columns from train, anon and test dataframes for each item in the input list
  dataframelist = lapply(dataframelist, function(listitem) lapply(listitem, function(df) df[, sapply(df, is.numeric)]) )

  # The first item in the each item is the reference dataframe. Calculate colmeans and sds for each column.
  ref_means = lapply(seq(length(dataframelist)), function(i) colMeans(dataframelist[[i]][[1]]))
  ref_sds = lapply(seq(length(dataframelist)), function(i) colSds(dataframelist[[i]][[1]]))

  # Scale the 'anon' dataframes with the 'ref' means and sds
  scaled_dataframelist = lapply(seq(length(dataframelist)), function(i)  scale(dataframelist[[i]][[2]], ref_means[[i]], ref_sds[[i]]))

  # Calculate colmeans for each scaled item 'anon' dataframe of the list.
  col_means = lapply(scaled_dataframelist, colMeans)

  # Calculate the mean of each 'colmeans' item
  mean_col_means = lapply(col_means, mean)

  # Calculate the sd of each 'colmeans' item
  sd_col_means = lapply(col_means, sd)

  # Return the mean of 'mean_col_means' and the mean of 'sd_col_means'
  return(list("Mean mean" = mean(unlist(mean_col_means)), "Mean mean sd" = mean(unlist(sd_col_means))))

}


#' Mean Difference in Column Variances
#'
#' Calculate the mean difference in column variances between data frames in a list, scaled by a reference data frame.
#'
#' @param dataframelist A list of data frames to compare.
#'
#' @return A list containing two elements: "Means" (Mean differences in column variances) and "Sds" (mean standard deviations of column variances).
#'
#' @importFrom stats sd
meanDiffsinVar = function(dataframelist){

  # Drop non-numeric columns from train, anon and test dataframes for each item in the input list
  dataframelist = lapply(dataframelist, function(listitem) lapply(listitem, function(df) df[, sapply(df, is.numeric)]) )

  # The first item in the each item is the reference dataframe. Calculate colmeans and sds for each column.
  ref_means = lapply(seq(length(dataframelist)), function(i) colMeans(dataframelist[[i]][[1]]))
  ref_sds = lapply(seq(length(dataframelist)), function(i) colSds(dataframelist[[i]][[1]]))

  # Scale the 'anon' dataframes with the 'ref' means and sds
  scaled_dataframelist = lapply(seq(length(dataframelist)), function(i)  scale(dataframelist[[i]][[2]], ref_means[[i]], ref_sds[[i]]))

  # Calculate colmsds for each scaled item 'anon' dataframe of the list and take the subtraction from colsds of the og data, (1 for all, since they are scaled).
  col_sds = lapply(scaled_dataframelist, function(dataframe) colSds(dataframe)-1)

  # Calculate the mean of each 'colmsds' item
  mean_col_sds = lapply(col_sds, mean)

  # Calculate the sd of each 'colsds' item
  sd_col_sds = lapply(col_sds, sd)

  # Return the mean of 'mean_col_means' and the mean of 'sd_col_means'
  return(list("Mean sd" = mean(unlist(mean_col_sds)), "Mean sd sd" = mean(unlist(sd_col_sds))))


}

#' Mean Difference in Correlation Matrices
#'
#' Calculate the median difference in correlation matrices between data frames in a list, scaled by a reference data frame.
#'
#' @param dataframelist A list of data frames to compare.
#'
#' @return A list containing two elements: "Medians" (median differences in correlation matrices) and "MADs" (median absolute deviations of correlation matrices).
#'
#' @importFrom stats cor sd
meanDiffsinCor = function(dataframelist){

  # Drop non-numeric columns from train, anon and test dataframes for each item in the input list
  dataframelist = lapply(dataframelist, function(listitem) lapply(listitem, function(df) df[, sapply(df, is.numeric)]) )

  # The first item in the each item is the reference dataframe. Calculate colmeans and sds for each column.
  ref_means = lapply(seq(length(dataframelist)), function(i) colMeans(dataframelist[[i]][[1]]))
  ref_sds = lapply(seq(length(dataframelist)), function(i) colSds(dataframelist[[i]][[1]]))

  # Scale the 'anon' dataframes with the 'ref' means and sds
  scaled_dataframelist = lapply(seq(length(dataframelist)), function(i) scale(dataframelist[[i]][[2]], ref_means[[i]], ref_sds[[i]]))

  # Scale the 'train' dataframes with the 'ref' means and sds
  scaled_trainlist = lapply(seq(length(dataframelist)), function(i) scale(dataframelist[[i]][[1]], ref_means[[i]], ref_sds[[i]]))

  # The first item in the list is the reference dataframe. Calculate colmeans and vars for each column. Only keep the upper triangle.
  upper_triangle_cor = function(df){
    corrs = cor(df)
    corrs[lower.tri(corrs)] <- NA
    diag(corrs) <- NA
    return(corrs)
  }

  # Calculate the 'upper_triangle_cor' for each 'train' and 'anon' dataframe in the list
  train_cor = lapply(scaled_trainlist, upper_triangle_cor)
  anon_cor = lapply(scaled_dataframelist, upper_triangle_cor)

  # Subtract the 'anon' correlation matrix from the 'train' correlation matrix for each dataframe in the list
  cor_diffs = lapply(seq(length(dataframelist)), function(i) anon_cor[[i]] - train_cor[[i]])

  # Calculate the mean difference in correlation matrices for each dataframe in the list
  col_means = lapply(cor_diffs, function(corr) mean(corr, na.rm = TRUE))

  # Calculate the standard deviation of the difference in correlation matrices for each dataframe in the list
  col_sds = lapply(cor_diffs, function(corr) sd(corr, na.rm = TRUE))


  # Return the mean of 'mean_col_means' and the mean of 'sd_col_means'
  return(list("Mean cor" = mean(unlist(col_means)), "Mean cor sd" = mean(unlist(col_sds))))
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
#' data_list <- list(data_frame1, data_frame2, data_frame3)
#' meansAll_list(data_list)
#'}
#' @export
meansAll_list = function(dataframelist){

  mean = (meanDiffsinMean(dataframelist))

  sd = (meanDiffsinVar(dataframelist))

  cor = (meanDiffsinCor(dataframelist))

  return(list("Means" = mean, "Sds" = sd, "Cor" = cor))
}

