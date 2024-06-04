#R^2-----------------------------------------------------

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
testRsquared_multimodel = function(model_list, testData, responseVar){

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

#' Test R-squared for single model with several repetitions
#'
#' Calculate R-squared values for multiple models on test data.
#'
#' @param model_list A list of models trained on anony
#' @param test_datalist The test dataset list.
#' @param responseVar_index The index of the response variable.
#'
#' @return A named list of R-squared values for each model.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' model_list <- list(model1, model2, model3)
#' testRsquared(model_list, test_data, responseVar_index)
#'}
#' @export
testRsquared_list = function(model_list, test_datalist, responseVar_index){

  # Function to calculate R-squared for a single model on test data
  testRsquared = function(model, testData, responseVar_index){
    test_mean = mean(testData[, responseVar_index])
    model_preds = predict(model, testData[,-responseVar_index])
    ss_res = sum((testData[, responseVar_index] - model_preds)^2)
    ss_tot = sum((testData[, responseVar_index] - test_mean)^2)
    return(1 - ss_res/ss_tot)
  }

  # Calculate R-squared for each model on the test data
  results = lapply(seq_along(model_list), function(i) testRsquared(model_list[[i]], test_datalist[[i]], responseVar_index))

  # Return the mean of the results
  return(signum(mean(unlist(results)),3))
}

#rocplots-----------------------------------------------------

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
    labs(color = "Mallit", subtitle = paste0(paste0("Alkuperäisen aineiston AUC = ", round(roc1$auc, 3)), paste0(".\nAnonyymin aineiston AUC = ", round(roc2$auc, 3), ".")), size = 0.1) +
    theme(plot.subtitle = element_text(size = 15))+
    theme( panel.border = element_rect(colour = "black", fill=NA)) +
    theme(legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"), legend.text = element_text(size=15))+
    theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12))


}

#' ROC Plot list
#'
#' Generate an ROC plot comparing the performance of a model on original and anonymized data.
#'
#' @param targetIndex The index of the target variable in the test data.
#' @param model_list The model trained on original data.
#' @param anon_model_list The model trained on anonymized data.
#' @param test_list The test dataset where the response variable is on the first column.
#' @param test_anon_list The test dataset for anon data (if different from original data, mainly here for RSA)
#'
#' @examples
#'\dontrun{
#' print("This is a test")
#' }
#'
#' @importFrom pROC roc coords
#' @importFrom ggplot2 ggplot geom_line geom_abline scale_x_continuous scale_y_continuous coord_equal theme_classic theme ggtitle scale_color_manual labs aes margin element_text element_rect element_blank
#'
#' @export
# roc_plot but with one list of models, with items [1] being trained on original data and [2] on anon data
roc_plot_list = function(targetIndex = 1, model_list, anon_model_list, test_list, test_anon_list = NULL){

  # If not provided, use the same test for both
  if(is.null(test_anon_list)) test_anon_list = test_list

  # Predict the values for the test data on both original and anonymous models
  model_pred_list = lapply(seq_along(model_list), function(i) predict(model_list[[i]], (test_list[[i]][,-targetIndex])))
  anon_pred_list = lapply(seq_along(anon_model_list), function(i) predict(anon_model_list[[i]], (test_anon_list[[i]][,-targetIndex])))

  # Calculate ROC curves for both data
  roc_list = lapply(seq_along(model_list), function(i) roc(as.factor(test_list[[i]][,targetIndex]), model_pred_list[[i]], auc = TRUE))
  anon_roc_list = lapply(seq_along(anon_model_list), function(i) roc(as.factor(test_anon_list[[i]][,targetIndex]), anon_pred_list[[i]], auc = TRUE))

  # Calculate mean AUC for both data
  auc_og = mean(sapply(roc_list, function(x) x$auc))
  auc_anon = mean(sapply(anon_roc_list, function(x) x$auc))

  # Combine the results into a single dataframe list
  df_list = lapply(seq_along(roc_list), function(i) rbind(cbind(model = "Alkuperäinen aineisto", coords(roc_list[[i]])),
              cbind(model = "Anonyymi aineisto", coords(anon_roc_list[[i]]))))

  # Create a ggplot object
  p <- ggplot(NULL) +
    lapply(df_list, function(df) {
      geom_line(data = df, aes(1 - specificity, sensitivity, color = model), alpha = 0.5)
    }) +
    # geom_line(size = 1, linewidth = 0.1, alpha = 0.1) +
    scale_x_continuous("1 - tarkkuus") +
    scale_y_continuous("Herkkyys") +
    coord_equal(expand = FALSE) +
    theme_classic(base_size = 15) +
    theme(plot.margin = margin(10, 30, 10, 10)) +
    ggtitle("ROC-käyrät") +
    theme(plot.title = element_text(size = 25)) +
    scale_color_manual(values = c("Alkuperäinen aineisto" = "black", "Anonyymi aineisto" = "red")) +
    theme(legend.position = c(0.9, 0.1), legend.justification = c(1, 0)) +
    labs(color = "Mallit", subtitle = paste0(paste0("Alkuperäisen aineiston AUC = ", signum(auc_og, 3)), paste0(".\nAnonyymin aineiston AUC = ", signum(auc_anon, 3), ".")), size = 0.1) +
    theme(plot.subtitle = element_text(size = 15)) +
    theme(panel.border = element_rect(colour = "black", fill=NA)) +
    theme(legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"),
          legend.text = element_text(size=15)) +
    theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12))

  # Return the ggplot object
  return(p)

}

#coefplot-------------

#' Coefficient Picker List
#'
#' A function to calculate the mean of the means and the mean of the confidence intervals for a list of models.
#'
#' @param multimodel_lm_list A list of models where each element is a list containing a reference model (non-anonymous) and an anonymous model.
#' @param interval The confidence interval level.
#' @param intercept Logical indicating whether to include the intercept in the calculation.
#'
#' @return Returns a list containing information about the means and confidence intervals of coefficients for each model.
#'
#' @importFrom stats coef confint quantile
#'
coeff_picker_list = function(multimodel_lm_list, interval, intercept){

  # A helper function to calculate the mean of the means and the mean of the confidence intervals for a single model
  mean_ci_helper = function(lm_list, interval, intercept){

    # Get the coefficients of each model in the list
    coeff_list = lapply(lm_list, coef)

    # Make a matrix of the coefficients
    coeff_mat = do.call(rbind, coeff_list)

    # Calculate means for each variable
    means = apply(coeff_mat, 2, mean)

    # Get the confidence intervals for each coefficient for each model
    ci_list = lapply(lm_list, confint, level = interval)

    # Make a matrix of the confidence intervals lower bounds
    ci_mat_low = do.call(rbind, lapply(ci_list, function(x) x[,1]))
    ci_mat_high = do.call(rbind, lapply(ci_list, function(x) x[,2]))

    # Calculate the mean of the lower and upper bounds
    ci_low_mean = apply(ci_mat_low, 2, mean)
    ci_high_mean = apply(ci_mat_high, 2, mean)

    var_names = names(means)

    # Remove the intercept if it is not wanted
    if(intercept == FALSE){
      var_names = var_names[-1]
      means = means[-1]
      ci_low_mean = ci_low_mean[-1]
      ci_high_mean = ci_high_mean[-1]
    }

    return(list("names" = var_names, "mean" = means, "ci_low" = ci_low_mean, "ci_high" = ci_high_mean))
  }

  # Calculate the means and confidence intervals for each anonymous model (located on index 2; multimodel_lm_list[[i]][[2]])

  # Initialize the list
  model_param_list = list()

  # First item is a reference model, non-anonymous model
  model_param_list[[1]] = mean_ci_helper(multimodel_lm_list[[1]][[1]], interval = interval, intercept = intercept)

  # The rest are anonymous models
  for(i in 1:length(multimodel_lm_list)){
    model_param_list[[i+1]] = mean_ci_helper(multimodel_lm_list[[i]][[2]], interval = interval, intercept = intercept)
  }

  return(model_param_list)

}


#' Coefficient Plot List
#'
#' A function to create a plot of coefficients with point estimates and confidence intervals for a list of models.
#'
#' @param multimodel_lm_list A list of models where each element is a list containing a reference model (non-anonymous) and an anonymous model.
#' @param model_names Optional. Names for the models. If not provided, default names ("Model1", "Model2", etc.) will be used.
#' @param interval The confidence interval level.
#' @param intercept Logical indicating whether to include the intercept in the plot.
#'
#' @importFrom dplyr bind_rows
#' @importFrom ggplot2 ggplot geom_point geom_errorbar coord_flip theme element_text labs theme
#'
#'
coeff_plot_list = function(multimodel_lm_list, model_names = NULL, interval = 0.95, intercept = FALSE){

  # Check if names are provided, if not, make a list of names
  if(is.null(model_names)){
    model_names = paste("Model", seq_along(multimodel_lm_list))
  }

  # Call the coeff_picker_list function to get the means and confidence intervals for each model
  model_param_list = coeff_picker_list(multimodel_lm_list, interval = interval, intercept = intercept)

  # Make a list of dataframes for each model
  model_param_df_list = lapply(model_param_list, function(x) data.frame("Variable" = x$names, "mean" = x$mean, "ci_low" = x$ci_low, "ci_high" = x$ci_high))

  # Combine the list of dataframes into a single dataframe for plotting
  df_all = bind_rows(model_param_df_list, .id = "Mallit")

  # Replace the "Model" column with the names of the models. Repeat the names of the models for each variable
  df_all$Mallit = rep(model_names, each = nrow(model_param_df_list[[1]]))
  df_all$Mallit = factor(df_all$Mallit, levels = model_names)


  # plot with x axis as the variables and y axis as the point estimates and confidence intervals, with the different models seperated by a color and dodge
  plot = ggplot(df_all) +
    geom_point(aes(x = Variable, y = mean, color = Mallit), position = position_dodge(width = -0.5)) +
    geom_errorbar(aes(x = Variable, ymin = ci_low, ymax = ci_high, color = Mallit), position = position_dodge(width = -0.5)) +
    theme_bw() +
    #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Parametrien keskimääräiset 95 %:n luottamusvälit", x = NULL, y = NULL)+
    coord_flip()


  return(plot)
}

# test modify
