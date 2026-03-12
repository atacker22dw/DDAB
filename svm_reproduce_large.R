library(e1071)

# ---- Step 1: Baseline model (unchanged) ----
set.seed(10111)
x = matrix(rnorm(100), 50, 2)
y = rep(c(-1, 1), each = 25)
x[y == 1, ] = x[y == 1, ] + 1
dat = data.frame(x, y = as.factor(y))

# Fit baseline SVMs
svm_linear = svm(y ~ ., data = dat, kernel = "linear", cost = 1, scale = FALSE)
svm_radial = svm(y ~ ., data = dat, kernel = "radial", cost = 100, scale = FALSE)

# Fixed test data for baseline
set.seed(13)
x1 = matrix(rnorm(40), 20, 2)
y1 = rep(c(-1, 1), each = 10)
x1[y1 == 1, ] = x1[y1 == 1, ] + 1
test_data = data.frame(x1)
colnames(test_data) = colnames(dat)[1:2]

# Baseline misclassification error
baseline_pred_linear = predict(svm_linear, test_data)
baseline_error_linear = mean(baseline_pred_linear != as.factor(y1))

baseline_pred_radial = predict(svm_radial, test_data)
baseline_error_radial = mean(baseline_pred_radial != as.factor(y1))

# Set the baseline difference manually (or use the actual baseline difference if desired)
baseline_diff = baseline_error_radial-baseline_error_linear


cat("Baseline error (linear):", baseline_error_linear, "\n")
cat("Baseline error (radial):", baseline_error_radial, "\n")
cat("Baseline difference (radial - linear):", baseline_diff, "\n")

# ---- Step 2: Simulation loop with new test data each time ----
set.seed(13)
count = 0

for (i in 1:1000) {
  # --- New training data ---
  x_train = matrix(rnorm(100), 50, 2)
  y_train = rep(c(-1, 1), each = 25)
  x_train[y_train == 1, ] = x_train[y_train == 1, ] + 1
  train_data = data.frame(x_train, y = as.factor(y_train))
  colnames(train_data) = c("X1", "X2", "y")
  
  # --- New test data ---
  x_test = matrix(rnorm(40), 20, 2)
  y_test = rep(c(-1, 1), each = 5)
  x_test[y_test == 1, ] = x_test[y_test == 1, ] + 1
  test_data_new = data.frame(x_test)
  colnames(test_data_new) = c("X1", "X2")
  
  # Fit SVMs
  svm_linear_new = svm(y ~ ., data = train_data, kernel = "linear", cost = 1, scale = FALSE)
  svm_radial_new = svm(y ~ ., data = train_data, kernel = "radial", cost = 100, scale = FALSE)
  
  # Predict on the new test set
  pred_linear_new = predict(svm_linear_new, test_data_new)
  error_linear_new = mean(pred_linear_new != as.factor(y_test))
  
  pred_radial_new = predict(svm_radial_new, test_data_new)
  error_radial_new = mean(pred_radial_new != as.factor(y_test))
  
  # Check if radial - linear >= baseline_diff
  if ((error_radial_new - error_linear_new) >= baseline_diff) {
    count = count + 1
  }
}

# ---- Step 3: Proportion ----
proportion_exceeds_baseline_diff = count / 1000

cat("Proportion of simulations where (radial - linear) >= baseline difference (", baseline_diff, "): ", 
    proportion_exceeds_baseline_diff, "\n")
    
# Exact test

binom.test(36, 1000, p = 0.01,
           alternative = "greater",
           conf.level = 0.95)
           
binom.test(147, 1000, p = 0.01,
           alternative = "greater",
           conf.level = 0.95)
           
         
