# Generate data
n <- 250
x1 <- runif(n, -10, 10)
x2 <- runif(n, -10, 10)
y <- sin(x1 + x2) < 0

# Define function to calculate error rate
error_rate <- function(y_true, y_pred) { mean(y_true != y_pred) }

# Initialize vectors to store error rates
train_errors <- rep(NA, 100)
test_errors <- rep(NA, 100)

# Loop over values of k
for (k in 1:100) {
  # Train k-NN model on full data with k neighbors
  knn_model <- knn(train = cbind(x1, x2), test = cbind(x1, x2), cl = y, k = k)
  
  # Calculate training error rate
  train_errors[k] <- error_rate(y, knn_model)
  
  # Repeat subsampling 100 times and calculate test error rate
  test_errors_rep <- rep(NA, 100)
  for (i in 1:100) {
    # Split data into 2/3 training and 1/3 test
    set.seed(i)
    train_idx <- sample(1:n, size = round(2*n/3), replace = FALSE)
    test_idx <- setdiff(1:n, train_idx)
    
    # Train k-NN model on training data with k neighbors
    knn_model <- knn(train = cbind(x1[train_idx], x2[train_idx]), 
                     test = cbind(x1[test_idx], x2[test_idx]), 
                     cl = y[train_idx], k = k)
    
    # Calculate test error rate
    test_errors_rep[i] <- error_rate(y[test_idx], knn_model)
  }
  test_errors[k] <- mean(test_errors_rep)
}

# Plot error rates as a function of k
plot(1:100, train_errors, type = "l", xlab = "k", ylab = "Error rate", main = "k-NN error rates")
lines(1:100, test_errors, col = "red")
legend("topright", legend = c("Training error", "Test error"), lty = 1, col = c("black", "red"))
