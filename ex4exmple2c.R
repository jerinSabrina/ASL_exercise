# Plot the data points
plot(x, y, main = "Optimal Models with Different Loss Functions", xlab = "x", ylab = "y", pch = 16)

# Function to plot the regression line
plot_regression_line <- function(theta0, theta1, color) {
  x_range <- range(x)
  x_vals <- seq(x_range[1], x_range[2], length.out = 100)
  y_vals <- theta0 + theta1 * x_vals
  lines(x_vals, y_vals, col = color)
}

# Plot the regression lines for each loss function
plot_regression_line(optimal_theta_l1_loss[1], optimal_theta_l1_loss[2], "blue")
plot_regression_line(optimal_theta_l2_loss[1], optimal_theta_l2_loss[2], "red")
plot_regression_line(optimal_theta_quantile_05_loss[1], optimal_theta_quantile_05_loss[2], "green")
plot_regression_line(optimal_theta_quantile_95_loss[1], optimal_theta_quantile_95_loss[2], "orange")
plot_regression_line(optimal_theta_huber_1_loss[1], optimal_theta_huber_1_loss[2], "purple")
plot_regression_line(optimal_theta_huber_2_loss[1], optimal_theta_huber_2_loss[2], "pink")
plot_regression_line(optimal_theta_epsilon_05_loss[1], optimal_theta_epsilon_05_loss[2], "brown")
plot_regression_line(optimal_theta_epsilon_3_loss[1], optimal_theta_epsilon_3_loss[2], "gray")

# Add legend
legend("topright", legend = c("L1 Loss", "L2 Loss", "Quantile Loss (α = 0.05)", "Quantile Loss (α = 0.95)", "Huber Loss (δ = 1)", "Huber Loss (δ = 2)", "Epsilon Loss (ε = 0.5)", "Epsilon Loss (ε = 3)"), col = c("blue", "red", "green", "orange", "purple", "pink", "brown", "gray"), lty = 1, cex = 0.8)
