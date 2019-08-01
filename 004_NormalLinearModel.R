# 004_NormalLinearModel
library(MASS)
set.seed(32497)
rm(list = ls())

# This script implements a low-dimensional linear model and generates
# associated diagnostics, including:
# - Hat matrix diagonal values (Leverage values) / describes residual variance
# - Cook's distances / describes a data point's influence on the fit
# - Standardized residuals
# - Studentized residuals
# - Quantile-quantile plots

# Generate synthetic data by sampling from a multivariate normal distribution
N <- 1000 #number of data points
mu_X <- c(0, 0) # Covariate mean vector
d <- 2 # number of features
Sigma_XX <- matrix(c(1, 0, 0, 1), nrow=2) # Independent features
sigma <- 1
mu_Y <- 1
X <- mvrnorm(N, mu_X, Sigma_XX)
beta <- matrix(c(1, 2), nrow=2, ncol=1)
y <- X %*% beta + rnorm(N, mean = 0, sd = sigma)
plot(X[ , 1], y)
plot(X[ , 2], y)
plot(X[ , 1], X[ , 2])

# Compute the analytical variance of the response
Sigma_XY <- solve(Sigma_XX) %*% beta
Sigma_YY <- sigma^2 + t(Sigma_XY) %*% solve(Sigma_XX) %*% Sigma_XY 
sigma^2/Sigma_YY # Fraction of unexplained variance!

# Compute ordinary least squares estimate of parameter
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
H <- X %*% solve(t(X) %*% X) %*% t(X) # Hat matrix
y_hat <- H %*% y # Fitted values
residuals <- y - y_hat 
leverages <- diag(H)
residual_variance <- (1 - H)*sigma^2 # Theoretical variance of residuals
sigma2_hat <- sum(residuals^2) / (N - 2)
standardized_residuals <- residuals/sqrt(sigma2_hat*(1 - leverages))
Cooks_distances <- (standardized_residuals^2)*leverages/(d*(1 - leverages))
plot(Cooks_distances)

hist(Cooks_distances, breaks=50)

inf_dp <- (Cooks_distances > quantile(Cooks_distances, 0.99))
plot(X[!inf_dp, 1], y[!inf_dp], bg='white', pch=21)
points(X[inf_dp, 1], y[inf_dp], bg='red', pch=21)

plot(X[!inf_dp, 2], y[!inf_dp], bg='white', pch=21)
points(X[inf_dp, 2], y[inf_dp], bg='red', pch=21)

# The purpose of leverage is to identify data points that have low residual variance
# and are therefore influential to the fit. Cook's distance is another
# statistic that can be used to evaluate how influential a data point is.
# Leverage and Cook's distances are not necessarily helpful for evaluating whether
# linearity can be assumed.