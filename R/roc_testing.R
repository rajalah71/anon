data("iris")

iris = iris[1:100,]

samples1 = sample(c(TRUE, FALSE), replace = TRUE, size = nrow(iris), prob = c(0.7, 0.3))

train1 = iris[samples1, ]
test1 = iris[!samples1, ]

samples2 = sample(c(TRUE, FALSE), replace = TRUE, size = nrow(iris), prob = c(0.7, 0.3))

train2 = iris[samples2, ]
test2 = iris[!samples2, ]

devtools::load_all()

anon1 = kAnon(train1, k = 2)
anon2 = kAnon(train2, k = 2)

train_list = list(anon1, anon2)
test_list = list(test1, test2)

linear_model1 = glm(as.factor(Species) ~ ., data = train1, family = binomial)
linear_model2 = glm(as.factor(Species) ~ ., data = train2, family = binomial)
lm_list = list(linear_model1, linear_model2)


anon_linear_model1 = glm(as.factor(Species) ~ ., data = anon1, family = binomial)
anon_linear_model2 = glm(as.factor(Species) ~ ., data = anon2, family = binomial)
anon_lm_list = list(anon_linear_model1, anon_linear_model2)


roc_plot_list(lm_list, anon_lm_list, test_list)
