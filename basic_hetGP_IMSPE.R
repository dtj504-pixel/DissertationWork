# Load the library
library(hetGP)

# Set seed for reproducibility
set.seed(1)

# Generate some example data
n <- 50
x <- seq(0, 1, length = n)
y <- sin(2 * pi * x) + rnorm(n, sd = 0.1)

plot(x, y, main = "Data for Homoskedastic GP", pch = 19)

#Fit a homoskedastic Gaussian Process model (one with constant variance)
model_hom <- mleHomGP(X = matrix(x, ncol = 1), Z = y)
summary(model_hom)

# Predict at new locations
xstar <- seq(0, 1, length = 200)
pred_hom <- predict(x = matrix(xstar, ncol = 1), object = model_hom)

# Plot
plot(x, y, pch = 19, cex = 0.6)
lines(xstar, pred_hom$mean, col = "blue", lwd = 2)
lines(xstar, pred_hom$mean + 2 * sqrt(pred_hom$sd2), col = "red", lty = 2)
lines(xstar, pred_hom$mean - 2 * sqrt(pred_hom$sd2), col = "red", lty = 2)