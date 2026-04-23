library(class)  # For knn()

# ---- Step 1: Set baseline ----
set.seed(123)
n <- 200
x1 <- rnorm(n, mean = 0, sd = 1)
x2 <- rnorm(n, mean = 0, sd = 1)
y <- ifelse(x1^2 + x2^2 + rnorm(n, 0, 0.2) > 1.5, "Class A", "Class B")
data <- data.frame(x1, x2, y = as.factor(y))

# Split data into train/test
set.seed(42)
train_indices <- sample(1:n, size = 0.7 * n)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]
train_X <- train_data[, c("x1", "x2")]
test_X <- test_data[, c("x1", "x2")]
train_y <- train_data$y
test_y <- test_data$y

# Baseline: k = 3
pred_3nn <- knn(train = train_X, test = test_X, cl = train_y, k = 3)
error_3nn <- mean(pred_3nn != test_y)

# Baseline: k = 24
pred_24nn <- knn(train = train_X, test = test_X, cl = train_y, k = 24)
error_24nn <- mean(pred_24nn != test_y)

# Difference (24-NN - 3-NN)
baseline_diff <- error_24nn - error_3nn
#more moderate baseline
#baseline_diff = 0.1

cat("Baseline error (k=3):", error_3nn, "\n")
cat("Baseline error (k=24):", error_24nn, "\n")
cat("Baseline difference (24NN - 3NN):", baseline_diff, "\n")

#add in distance weighted scheme.  addresses a point in discussion
library(kknn)    #for kknn()
train_df <- train_data
test_df  <- test_data

# k = 3 (weighted)
fit_3_w <- kknn(y ~ x1 + x2,
                train = train_df,
                test  = test_df,
                k = 3,
                kernel = "inv")  

pred_3_w  <- fitted(fit_3_w)
error_3_w <- mean(pred_3_w != test_y)

# k = 24 (weighted)
fit_24_w <- kknn(y ~ x1 + x2,
                 train = train_df,
                 test  = test_df,
                 k = 24,kernel = "inv")

pred_24_w  <- fitted(fit_24_w)
error_24_w <- mean(pred_24_w != test_y)

weighted_diff <- error_24_w - error_3_w

# ---- Step 2: Simulation loop ----
set.seed(123)
count <- 0  # Count how many times new difference >= baseline
n_sim <- 1000

for (i in 1:n_sim) {
  # Generate new synthetic data
  x1_sim <- rnorm(n, mean = 0, sd = 1)
  x2_sim <- rnorm(n, mean = 0, sd = 1)
  y_sim <- ifelse(x1_sim^2 + x2_sim^2 + rnorm(n, 0, 0.2) > 1.5, "Class A", "Class B")
  data_sim <- data.frame(x1 = x1_sim, x2 = x2_sim, y = as.factor(y_sim))
  
  # Train/test split
  train_idx <- sample(1:n, size = 0.7 * n)
  train_data_sim <- data_sim[train_idx, ]
  test_data_sim <- data_sim[-train_idx, ]
  train_X_sim <- train_data_sim[, c("x1", "x2")]
  test_X_sim <- test_data_sim[, c("x1", "x2")]
  train_y_sim <- train_data_sim$y
  test_y_sim <- test_data_sim$y
  
  # k = 3
  pred_3nn_sim <- knn(train = train_X_sim, test = test_X_sim, cl = train_y_sim, k = 3)
  error_3nn_sim <- mean(pred_3nn_sim != test_y_sim)
  
  # k = 24
  pred_24nn_sim <- knn(train = train_X_sim, test = test_X_sim, cl = train_y_sim, k = 24)
  error_24nn_sim <- mean(pred_24nn_sim != test_y_sim)
  
  # Compare difference
  diff_sim <- error_24nn_sim - error_3nn_sim
  
  if (diff_sim >= baseline_diff) {
    count <- count + 1
  }
}

# ---- Step 3: Report ----
prop_exceeds_baseline <- count / n_sim
cat("Proportion of simulations where (24NN - 3NN) >= baseline difference (", baseline_diff, "): ",
    prop_exceeds_baseline, "\n")
