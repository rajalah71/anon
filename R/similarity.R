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
  return(list("Mean mean" = signum(mean(unlist(mean_col_means)),3), "Mean mean sd" = signum(mean(unlist(sd_col_means)),3)))

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
  col_vars = lapply(scaled_dataframelist, function(dataframe) colVars(dataframe)-1)

  # Calculate the mean of each 'colmsds' item
  mean_col_vars = lapply(col_vars, mean)

  # Calculate the sd of each 'colsds' item
  sd_col_vars = lapply(col_vars, sd)

  # Return the mean of 'mean_col_means' and the mean of 'var_col_means'
  return(list("Mean var" = signum(mean(unlist(mean_col_vars)),3), "Mean var sd" = signum(mean(unlist(sd_col_vars)),3)))


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
  return(list("Mean cor" = signum(mean(unlist(col_means)),3), "Mean cor sd" = signum(mean(unlist(col_sds)),3)))
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

  # The first item in the list is the reference dataframe. Calculate colmeans and sds for each column.
  ref_df = dataframelist[[1]]
  ref_means = colMeans(ref_df)
  ref_sds = sapply(ref_df, sd)

  # Scale the other dataframes with the reference means and sds
  scaled_dataframelist = lapply(dataframelist[-1], function(df) scale(df, ref_means, ref_sds))

  # calculate column means for each other dataframe of the list.
  other_means = lapply(scaled_dataframelist, colMeans)

  # Calculate distances from the reference means for each dataframe to colmeans of 0
  diffs = lapply(other_means, function(om) (om))

  # Calculate the median difference in colmeans for each dataframe in the list
  med_diffs = lapply(diffs, function(diff) median(diff)) # returned

  # subtract the the med_diffs item i from the abs_diffs item i
  mads = lapply(seq_along(med_diffs), function(i) median(abs(diffs[[i]] - med_diffs[[i]]))) # returned

  # Name the list items
  names(mads) = names[-1]

  # return
  return(list("Medians" = unlist(med_diffs), "MADs" = unlist(mads)))

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

  # The first item in the list is the reference dataframe. Calculate colmeans and sds for each column.
  ref_df = dataframelist[[1]]
  ref_means = colMeans(ref_df)
  ref_sds = sapply(ref_df, sd)

  # Scale the other dataframes with the reference means and sds
  scaled_dataframelist = lapply(dataframelist[-1], function(df) scale(df, ref_means, ref_sds))

  # calculate column variances for each other dataframe of the list.
  other_vars = lapply(scaled_dataframelist, colVars)

  # Calculate absolute distances from the reference vars for each dataframe to colvars of 1
  diffs = lapply(other_vars, function(ov) (ov - rep(1, length(ref_means))))

  # Calculate the median difference in colvars for each dataframe in the list
  med_diffs = lapply(diffs, function(ad) median(ad)) # returned

  # subtract the the med_diffs item i from the abs_diffs item i
  mads = lapply(seq_along(med_diffs), function(i) median(abs(diffs[[i]] - med_diffs[[i]]))) # returned

  # Name the list items
  names(mads) = names[-1]

  # return
  return(list("Medians" = unlist(med_diffs), "MADs" = unlist(mads)))

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
  med_diffs = sapply(other_cor, function(oc) median((oc - ref_cor), na.rm = TRUE))

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

