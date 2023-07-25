# mydt <- read.csv("C:/Users/Sabrina/Downloads/fitting.csv")

squared_loss <- function(y_true, y_pred) {
  mean((y_true - y_pred)^2)
}

absolute_loss <- function(y_true, y_pred) {
  mean(abs(y_true - y_pred))
}

huber_loss <- function(y_true, y_pred, delta = 1.0) {
  error <- y_true - y_pred
  absolute_error <- abs(error)
  quadratic_error <- error^2
  mask <- absolute_error <= delta
  mean(ifelse(mask, quadratic_error / 2.0, delta * (absolute_error - delta / 2.0)))
}


calculate_optimal_theta <- function(loss_func) 
  {
  data <- read.csv("C:/Users/Sabrina/Downloads/fitting.csv")
  x <- data$x  
  y <- data$y 
  
  search_interval <- c(-10.0, 10.0) 
  num_samples <- 1000 
  
  best_loss <- Inf
  best_theta <- NULL
  
  for (i in 1:num_samples) {
    theta <- runif(2, min = search_interval[1], max = search_interval[2])
    y_pred <- theta[1] + theta[2] * x
    loss <- loss_func(y, y_pred)
    
    if (loss < best_loss) {
      best_loss <- loss
      best_theta <- theta
    }
  }
  
  return(best_theta)
}


optimal_theta_squared_loss <- calculate_optimal_theta(squared_loss)
print("Optimal Theta (Squared Loss):")
print(optimal_theta_squared_loss)


optimal_theta_absolute_loss <- calculate_optimal_theta(absolute_loss)
print("Optimal Theta (Absolute Loss):")
print(optimal_theta_absolute_loss)


optimal_theta_huber_loss <- calculate_optimal_theta(huber_loss)
print("Optimal Theta (Huber Loss):")
print(optimal_theta_huber_loss)

