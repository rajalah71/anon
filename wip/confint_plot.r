confintplot = function(glm_list){
  # The function takes a list of general linear models trained on sliglty different variations of the same data and plots the confints for each parameter on top of each other on the same plot
  # the first item in the list is the reference glm and all the others are variations of it
  # for each parameter, add a row to the plot where the different confints are plotted overlayed on top of each other

    for(glm in glm_list){





  }
}

confintplot = function(glm_list) {
  # The function takes a list of general linear models trained on slightly different variations of the same data and plots the confints for each parameter on top of each other on the same plot
  # The first item in the list is the reference glm and all the others are variations of it
  # For each parameter, add a column to the whiskers plot where the different confints are plotted overlayed on top of each other


}

confintplot = function(glm_list) {
  # Extract the reference glm and its parameter names
  ref_glm = glm_list[[1]]
  param_names = names(coef(ref_glm))

  # Create a matrix to store the confints for each parameter and glm
  confints = matrix(NA, nrow = length(param_names), ncol = length(glm_list))

  # Fill the matrix with the confints for each parameter and glm
  for (i in seq_along(glm_list)) {
    glm = glm_list[[i]]
    confints[, i] = confint(glm)[, 1] # Use the lower bound of the confint
  }

  # Check that the dimensions of the confints matrix match the length of the param_names vector
  if (nrow(confints) != length(param_names)) {
    stop("The dimensions of the confints matrix do not match the length of the param_names vector")
  }

  # Plot the confints for each parameter
  par(mar = c(5, 4, 4, 8) + 0.1) # Increase the right margin for the parameter names
  plot(1, type = "n", xlim = c(0.5, length(param_names) + 0.5), ylim = range(confints), xlab = "", ylab = "Confint", xaxt = "n")
  axis(1, at = 1:length(param_names), labels = param_names, las = 2)
  for (i in seq_along(param_names)) {
    for (j in seq_along(glm_list)) {
      if (j == 1) {
        col = "red" # Use red for the reference glm
      } else {
        col = rainbow(length(glm_list) - 1)[j - 1] # Use different colors for the other glm models
      }
      lines(rep(i, 2), confints[i, j], col = col)
      points(i, confints[i, j], col = col)
    }
  }
  legend("topright", legend = paste0("glm", seq_along(glm_list)), col = c("red", rainbow(length(glm_list) - 1)), lty = 1, pch = 1)
}
