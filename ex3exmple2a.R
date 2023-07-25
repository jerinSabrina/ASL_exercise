n <- 1000
x1 <- runif(n, -10, 10)
x2 <- runif(n, -10, 10)
y <- sin(x1 + x2) < 0

# Create a grid of points to evaluate the decision boundary
x1_grid <- seq(-10, 10, length.out = 100)
x2_grid <- seq(-10, 10, length.out = 100)
grid <- expand.grid(x1_grid, x2_grid)

# Calculate the true decision boundary for each point in the grid
z <- apply(grid, 1, function(p) as.numeric(sin(p[1] + p[2]) < 0))

# Plot the true decision boundary using the contour function
contour(x1_grid, x2_grid, matrix(z, nrow = length(x2_grid), ncol = length(x1_grid)),
        levels = 0.5, labels = "", xlim = c(-10, 10), ylim = c(-10, 10))
points(x1[y], x2[y], col = "blue")
points(x1[!y], x2[!y], col = "red")


