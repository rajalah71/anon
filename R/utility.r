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
    labs(color = "Mallit", subtitle = paste0(paste0("Alkuperäisen aineiston AUC = ", round(roc1$auc, 3)), paste0(".\nAnonyymin aineiston AUC = ", round(roc2$auc, 3), ".")), size = 0.1) +
    theme(plot.subtitle = element_text(size = 15))+
    theme( panel.border = element_rect(colour = "black", fill=NA)) +
    theme(legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"), legend.text = element_text(size=15))+
    theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12))


}
