set.seed(10111)


library(MASS)
library(e1071)

n_train <- 400    # training observations
n_test  <- 100     # test observations
p <- 50           # number of predictors

rho <- 0.3        # correlation among predictors
shift <- 0.4      # class separation shift
noise_sd <- 0.5
# Covariance matrix for correlated predictors
Sigma <- matrix(rho, p, p)
diag(Sigma) <- 1

# Generate TRAINING SET

x_train <- mvrnorm(n = n_train, mu = rep(0, p), Sigma = Sigma)
y_train <- rep(c(-1, 1), each = n_train/2)

x_train[y_train == 1, ] <- x_train[y_train == 1, ] + shift
x_train <- x_train + matrix(rnorm(n_train * p, sd = noise_sd), n_train, p)

colnames(x_train) <- paste0("X", 1:p)
train_dat <- data.frame(x_train, y = as.factor(y_train))

#TEST SET

x_test <- mvrnorm(n = n_test, mu = rep(0, p), Sigma = Sigma)
y_test <- rep(c(-1, 1), each = n_test/2)

x_test[y_test == 1, ] <- x_test[y_test == 1, ] + shift
x_test <- x_test + matrix(rnorm(n_test * p, sd = noise_sd), n_test, p)

colnames(x_test) <- paste0("X", 1:p)
test_dat <- data.frame(x_test, y = as.factor(y_test))


# Fit SVM models on TRAINING data

svm_linear <- svm(y ~ ., data = train_dat, kernel = "linear", cost = 1, scale = FALSE)
svm_radial <- svm(y ~ ., data = train_dat, kernel = "radial", cost = 100, scale = FALSE)

# Confusion Matrix function

confusion2x2 <- function(pred, actual, positive = "1") {
  pred   <- as.character(pred)
  actual <- as.character(actual)
  
  TP <- sum(pred == positive & actual == positive)
  TN <- sum(pred != positive & actual != positive)
  FP <- sum(pred == positive & actual != positive)
  FN <- sum(pred != positive & actual == positive)
  
  matrix(c(TP, FN,
           FP, TN),
         nrow = 2, byrow = TRUE,
         dimnames = list(
           Predicted = c("Positive", "Negative"),
           Actual    = c("Positive", "Negative")
         ))
}


# Confusion Matrices: TRAINING DATA

pred_linear_train <- predict(svm_linear, train_dat)
pred_radial_train <- predict(svm_radial, train_dat)

confusion_linear_train <- confusion2x2(pred_linear_train, train_dat$y)
confusion_radial_train <- confusion2x2(pred_radial_train, train_dat$y)

confusion_linear_train
confusion_radial_train


# Confusion Matrices: TEST DATA

pred_linear_test <- predict(svm_linear, test_dat)
pred_radial_test <- predict(svm_radial, test_dat)

confusion_linear_test <- confusion2x2(pred_linear_test, test_dat$y)
confusion_radial_test <- confusion2x2(pred_radial_test, test_dat$y)

confusion_linear_test
confusion_radial_test

# Function to compute error rate from a 2×2 confusion matrix
error_rate <- function(cm) {
  TP <- cm[1, 1]
  FN <- cm[1, 2]
  FP <- cm[2, 1]
  TN <- cm[2, 2]
  (FP + FN) / (TP + TN + FP + FN)
}

# ---- Misclassification error rates ----

err_linear_train  <- error_rate(confusion_linear_train)
err_radial_train  <- error_rate(confusion_radial_train)
err_linear_test   <- error_rate(confusion_linear_test)
err_radial_test   <- error_rate(confusion_radial_test)

# Print them
err_linear_train
err_radial_train
err_linear_test
err_radial_test


library(ggplot2)
library(gridExtra)
library(e1071)


# PCA on training data

pca_train <- prcomp(train_dat[, -which(names(train_dat)=="y")], 
                    center = TRUE, scale. = TRUE)
pca_train_scores <- data.frame(PC1 = pca_train$x[,1], 
                               PC2 = pca_train$x[,2],
                               Label = train_dat$y)

# training scatterplots
plot_train <- ggplot(pca_train_scores, aes(x = PC1, y = PC2, color = Label)) +
  geom_point(size=3) +
  labs(title = "Training Data PCA Scatterplot") +
  theme_minimal()


# PCA on combined training + testing data

combined_data <- rbind(train_dat[, -which(names(train_dat)=="y")],
                       test_dat[, -which(names(test_dat)=="y")])
combined_labels <- factor(c(train_dat$y, test_dat$y),
                          labels = c("Class -1", "Class 1"))
combined_set <- c(rep("Train", n_train), rep("Test", n_test))

pca_combined <- prcomp(combined_data, center = TRUE, scale. = TRUE)
pca_combined_scores <- data.frame(PC1 = pca_combined$x[,1],
                                  PC2 = pca_combined$x[,2],
                                  Label = combined_labels,
                                  Set = combined_set)


# Decision boundary grid (project back to original space)

xrange <- seq(min(pca_combined_scores$PC1)-1, max(pca_combined_scores$PC1)+1, length=100)
yrange <- seq(min(pca_combined_scores$PC2)-1, max(pca_combined_scores$PC2)+1, length=100)
grid <- expand.grid(PC1 = xrange, PC2 = yrange)

# Back-project to original 50D space
grid_original <- as.matrix(grid) %*% t(pca_combined$rotation[,1:2])
grid_original <- scale(grid_original, center = -pca_combined$center, scale = FALSE)
grid_original <- scale(grid_original, center = FALSE, scale = 1/pca_combined$scale)

# Predictions for the grid
pred_linear_grid <- predict(svm_linear, as.data.frame(grid_original))
pred_radial_grid <- predict(svm_radial, as.data.frame(grid_original))

grid$Linear <- pred_linear_grid
grid$Radial <- pred_radial_grid


# Combined scatterplot + Linear SVM boundary

plot_linear <- ggplot() +
  geom_point(data = pca_combined_scores, aes(x = PC1, y = PC2, color = Label, shape = Set), size=3) +
  geom_contour(data = grid, aes(x = PC1, y = PC2, z = as.numeric(Linear)), breaks = c(1.5), color = "black") +
  labs(title = "Linear SVM Decision Boundary (Training + Test)") +
  theme_minimal()


# Combined scatterplot + Radial SVM boundary

plot_radial <- ggplot() +
  geom_point(data = pca_combined_scores, aes(x = PC1, y = PC2, color = Label, shape = Set), size=3) +
  geom_contour(data = grid, aes(x = PC1, y = PC2, z = as.numeric(Radial)), breaks = c(1.5), color = "black") +
  labs(title = "Radial SVM Decision Boundary (Training + Test)") +
  theme_minimal()


# Show all plots together

grid.arrange(plot_train, plot_linear, plot_radial, nrow=1)


