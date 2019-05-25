# 25/05/2019
# Adaptive Recursive Least Squares example
# Data stream with linear (w.r.t time) drift in first component
library(MASS)
set.seed(086324)
rm(list = ls())

d <- 3 # parameter vector dimension
T_ <- 100 # Time horizon
X <- matrix(rnorm(T_*d), nrow=T_)
beta <- matrix(rep(1, d*T_), nrow=T_) # true parameter value
beta[ , 1] <- (1:T_) # drifting parameter
y <- rep(NA, T_)
for (t in 1:T_) {
  y[t] = (X[t, , drop=F] %*% t(beta[t, , drop=F]))[1] + rnorm(1)
}

plot(y, type='p')
# y <- X %*% beta + rnorm(T_, sd=1)

# Implement RLS algorithm
beta_hat <- matrix(rep(NA, T_*d), nrow=T_) # parameter estimates
N <- d # For example
lambda <- 0.95 # Forgetting factor

# Initialization estimate
lamN <- sapply(1:N, FUN = function(j) lambda^(N-j))
P <- solve(lamN*t(X[1:N, ]) %*% X[1:N, ])
R <- lamN*t(X[1:N, ]) %*% y[1:N]
beta_hat[N, ] <- P %*% R
one_step_prediction <- rep(NA, T_)

# Recursive update
for (t in (N+1):T_) {
  one_step_prediction[t] <- beta_hat[t-1, , drop=F] %*% t(X[t, , drop=F])
  k <- P %*% t(X[t, , drop=F])/(lambda + X[t, , drop=F] %*% P %*% t(X[t, , drop=F]))[1]
  P <- (P - k %*% X[t, , drop=F] %*% P)/lambda
  beta_hat[t, ] <- beta_hat[t-1, ] + k %*% 
    (y[t] - t(beta_hat[t-1, ] %*% t(X[t, , drop=F]))[1])
}

# Plot estimate versus true value
plot(beta_hat[ , 1], type='l')
lines(beta[ , 1], col='red')

plot(beta_hat[ , 2], type='l')
abline(h=beta[2], col='red')

# Plot squared one-step prediction errors
plot((y - one_step_prediction)^2, type='l')

# Plot predictions against observations
plot(y)
points(one_step_prediction, col='red')
