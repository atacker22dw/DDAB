library(e1071)

set.seed(30)

# Tuning grids
#tune gamma as well for good measure
cost_grid = c(0.01, 0.1, 1, 10, 100)
gamma_grid = c(0.01, 0.1, 1)

baseline_diff = 0.5

count = 0

diffs = numeric(1000)

for (i in 1:1000) {
  
  # training data
  x_train = matrix(rnorm(40), 20, 2)
  y_train = rep(c(-1, 1), each = 10)
  x_train[y_train == 1, ] = x_train[y_train == 1, ] + 1
  train_data = data.frame(X1 = x_train[,1], X2 = x_train[,2], y = as.factor(y_train))
  
  # test
  x_test = matrix(rnorm(20), 10, 2)
  y_test = rep(c(-1, 1), each = 5)
  x_test[y_test == 1, ] = x_test[y_test == 1, ] + 1
  test_data = data.frame(X1 = x_test[,1], X2 = x_test[,2])
  
  # tune linear SVM 
  tune_linear = tune(
    svm,
    y ~ .,
    data = train_data,
    kernel = "linear",
    ranges = list(cost = cost_grid),
    tunecontrol = tune.control(cross = 5)
  )
  
  svm_linear_best = tune_linear$best.model
  
  # tune radial SVM
  tune_radial = tune(
    svm,
    y ~ .,
    data = train_data,
    kernel = "radial",
    ranges = list(cost = cost_grid, gamma = gamma_grid),
    tunecontrol = tune.control(cross = 5)
  )
  
  svm_radial_best = tune_radial$best.model
  
  # evaluate 
  pred_linear = predict(svm_linear_best, test_data)
  error_linear = mean(pred_linear != as.factor(y_test))
  
  pred_radial = predict(svm_radial_best, test_data)
  error_radial = mean(pred_radial != as.factor(y_test))
  
  diff = error_radial - error_linear
  diffs[i] = diff
  
  # compare to baseline 
  if (diff >= baseline_diff) {
    count = count + 1
  }
}

# final result 
proportion = count / 1000

cat("Proportion where (radial - linear) >=", baseline_diff, ":\n")
print(proportion)