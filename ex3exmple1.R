mydt <- read.csv("C:/Users/Sabrina/Downloads/fitting.csv")

hyp1 <- lm(y ~ x, data = mydt)
hyp2 <- lm(y ~ x + I(x^2),data = mydt)
hyp3 <- lm(y ~ x + I(x^2) + I(x^3), data = mydt)
hyp5 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5), data = mydt)
hyp8 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8), data = mydt)
hyp12 <-lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) +  I(x^9) + I(x^10) + I(x^11) + I(x^12), data = mydt)

fn1 <- function(x) coef(hyp1)[1] + coef(hyp1)[2] * x 
fn2 <- function(x) coef(hyp2)[1] + coef(hyp2)[2] * x + coef(hyp2)[3] * x^2
fn3 <- function(x) coef(hyp3)[1] + coef(hyp3)[2] * x + coef(hyp3)[3] * x^2 + coef(hyp3)[4] * x^3
fn5 <- function(x) coef(hyp5)[1] + coef(hyp5)[2] * x + coef(hyp5)[3] * x^2 + coef(hyp5)[4] * x^3 + coef(hyp5)[5] * x^4 + coef(hyp5)[6] * x^5
fn8 <- function(x) coef(hyp8)[1] + coef(hyp8)[2] * x + coef(hyp8)[3] * x^2 + coef(hyp8)[4] * x^3 + coef(hyp8)[5] * x^4 + coef(hyp8)[6] * x^5 + coef(hyp8)[7] * x^6 + coef(hyp8)[8] * x^7 + coef(hyp8)[9] * x^8
fn12 <- function(x) coef(hyp12)[1] + coef(hyp12)[2] * x + coef(hyp12)[3] * x^2 + coef(hyp12)[4] * x^3 + coef(hyp12)[5] * x^4 + coef(hyp12)[6] * x^5 + coef(hyp12)[7] * x^6 + coef(hyp12)[8] * x^7 + coef(hyp12)[9] * x^8 + coef(hyp12)[10]*x^9 + coef(hyp12)[11]*x^10 +  coef(hyp12)[12]* x^11 + coef(hyp12)[13]*x^12

library(ggplot2)

val <- ggplot(aes(y = y, x = x), data = mydt) + geom_point()

h1 <- val + geom_function(fun = fn1, col = "red") + ggtitle("Hypothesis Space H1")
h2 <- val + geom_function(fun = fn2, col = "red") + ggtitle("Hypothesis Space H2")
h3 <- val + geom_function(fun = fn3, col = "red") + ggtitle("Hypothesis Space H3")
h5 <- val + geom_function(fun = fn5, col = "red") + ggtitle("Hypothesis Space H5")
h8 <- val + geom_function(fun = fn8, col = "red") + ggtitle("Hypothesis Space H8") 
h12 <- val + geom_function(fun = fn12, col = "red") + ggtitle("Hypothesis Space H12")

library(gridExtra)

grid.arrange(h1,h2,h3,h5,h8,h12, nrow = 2)