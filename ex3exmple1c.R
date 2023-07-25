mydt <- read.csv("C:/Users/Sabrina/Downloads/test.csv")

train <- mydt[1:80,]
test <- mydt[81:100,]

hyp1 <- lm(y ~ x, data = train)
hyp2 <- lm(y ~ x + I(x^2),data = train)
hyp3 <- lm(y ~ x + I(x^2) + I(x^3), data = train)
hyp5 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5), data = train)
hyp8 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8), data = train)
hyp12 <-lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12), data = train)



fits<- list(hyp1, hyp2, hyp3, hyp5, hyp8, hyp12)
MSE <- lapply(fits, function(fit) sum((predict(fit, newdata = test) - test$y)^2))
MSE <- data.frame(MSE = unlist(MSE), models = c("H1", "H2", "H3", "H5","H8", "H12"))
MSE