data("iris")

lm = lm(Sepal.Length ~ ., data = iris)

iris_mod = iris
iris_mod$Sepal.Length = iris_mod$Sepal.Length + rnorm(nrow(iris_mod))

lm2 = lm(Sepal.Length ~ ., data = iris_mod)

iris_mod = iris
iris_mod$Sepal.Length = iris_mod$Sepal.Length + rnorm(nrow(iris_mod), sd = 1.5)

lm3 = lm(Sepal.Length ~ ., data = iris_mod)

iris_mod = iris
iris_mod$Sepal.Length = iris_mod$Sepal.Length + rnorm(nrow(iris_mod), sd = 2)

lm4 = lm(Sepal.Length ~ ., data = iris_mod)

model_list = list(lm, lm2)
model_list2 = list(lm3, lm4)

model_ref = list(model_list, model_list2)

coeff_plot = function(lm_list, alpha = 0.95, intercept = FALSE){
  confint_list = lapply(seq_along(lm_list), function(i) {
    lapply(lm_list[[i]], function(data) confint(data, level = alpha))
  })
  
  return(confint_list)
  
  # mean_confint_list = lapply()
  #   
  # poist_est_list = lapply(lm_list, coef)
  
}
coeff_plot(model_list)
