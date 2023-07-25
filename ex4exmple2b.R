# Read the data from fitting2.csv
data <- read.csv("C:/Users/Sabrina/Downloads/fitting.csv")
x <- data$x
y <- data$y

# Define the risk functions
l1_loss <- function(theta0, theta1) {
  y_pred <- theta0 + theta1 * x
  return(mean(abs(y - y_pred)))
}

l2_loss <- function(theta0, theta1) {
  y_pred <- theta0 + theta1 * x
  return(mean((y - y_pred)^2))
}

quantile_loss <- function(theta0, theta1, alpha) {
  y_pred <- theta0 + theta1 * x
  residual <- y - y_pred
  return(mean(ifelse(residual >= 0, alpha * residual, (alpha - 1) * residual)))
}

huber_loss <- function(theta0, theta1, delta) {
  y_pred <- theta0 + theta1 * x
  absolute_error <- abs(y - y_pred)
  quadratic_error <- (y - y_pred)^2
  mask <- absolute_error <= delta
  return(mean(ifelse(mask, quadratic_error / 2, delta * (absolute_error - delta / 2))))
}

epsilon_loss <- function(theta0, theta1, epsilon) {
  y_pred <- theta0 + theta1 * x
  absolute_error <- abs(y - y_pred)
  return(mean(ifelse(absolute_error <= epsilon, absolute_error^2 / 2, epsilon * (absolute_error - epsilon / 2))))
}

# Calculate the optimal models with respect to each loss function
calculate_optimal_model <- function(loss_func) {
  search_interval <- c(-10.0, 10.0)  # Define the search interval for theta values
  num_samples <- 1000  # Number of random samples
  
  best_loss <- Inf
  best_theta <- NULL
  
  for (i in 1:num_samples) {
    theta <- runif(2, min = search_interval[1], max = search_interval[2])
    loss <- loss_func(theta[1], theta[2])
    
    if (loss < best_loss) {
      best_loss <- loss
      best_theta <- theta
    }
  }
  
  return(best_theta)
}

# Calculate the optimal models using different loss functions
optimal_theta_l1_loss <- calculate_optimal_model(l1_loss)
optimal_theta_l2_loss <- calculate_optimal_model(l2_loss)
optimal_theta_quantile_05_loss <- calculate_optimal_model(function(theta0, theta1) quantile_loss(theta0, theta1, 0.05))
optimal_theta_quantile_95_loss <- calculate_optimal_model(function(theta0, theta1) quantile_loss(theta0, theta1, 0.95))
optimal_theta_huber_1_loss <- calculate_optimal_model(function(theta0, theta1) huber_loss(theta0, theta1, 1))
optimal_theta_huber_2_loss <- calculate_optimal_model(function(theta0, theta1) huber_loss(theta0, theta1, 2))
optimal_theta_epsilon_05_loss <- calculate_optimal_model(function(theta0, theta1) epsilon_loss(theta0, theta1, 0.5))
optimal_theta_epsilon_3_loss <- calculate_optimal_model(function(theta0, theta1) epsilon_loss(theta0, theta1, 3))


print(optimal_theta_l1_loss)
print(optimal_theta_l2_loss)
print(optimal_theta_quantile_05_loss)
print(optimal_theta_quantile_95_loss)
print(optimal_theta_huber_1_loss)
print(optimal_theta_huber_2_loss)
print(optimal_theta_epsilon_05_loss)
print(optimal_theta_epsilon_3_loss)
