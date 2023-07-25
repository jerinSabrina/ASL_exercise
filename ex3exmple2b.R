n <- 250
x1 <- runif(n, -10, 10)
x2 <- runif(n, -10, 10)
y <- as.numeric(sin(x1 + x2) < 0)
data <- data.frame(x1, x2, y)


x1_range <- seq(min(x1), max(x1), length.out = 100)
x2_range <- seq(min(x2), max(x2), length.out = 100)
grid <- expand.grid(x1 = x1_range, x2 = x2_range)


library(class)
k_values <- c(1, 5, 10, 100)
pred_list <- lapply(k_values, function(k) {
  knn(train = data[, c("x1", "x2")], test = grid, cl = data$y, k = k)
})
names(pred_list) <- paste0("k", k_values)

library(ggplot2)
grid$pred_k1 <- pred_list[["k1"]]
grid$pred_k5 <- pred_list[["k5"]]
grid$pred_k10 <- pred_list[["k10"]]
grid$pred_k100 <- pred_list[["k100"]]
p1 <- ggplot(grid, aes(x = x1, y = x2, color = factor(pred_k1))) +
  geom_point() +
  scale_color_manual(values = c("blue", "red")) +
  theme_bw() +
  ggtitle("k = 1")
p2 <- ggplot(grid, aes(x = x1, y = x2, color = factor(pred_k5))) +
  geom_point() +
  scale_color_manual(values = c("blue", "red")) +
  theme_bw() +
  ggtitle("k = 5")
p3 <- ggplot(grid, aes(x = x1, y = x2, color = factor(pred_k10))) +
  geom_point() +
  scale_color_manual(values = c("blue", "red")) +
  theme_bw() +
  ggtitle("k = 10")
p4 <- ggplot(grid, aes(x = x1, y = x2, color = factor(pred_k100))) +
  geom_point() +
  scale_color_manual(values = c("blue", "red")) +
  theme_bw() +
  ggtitle("k = 100")
library(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol = 2)


