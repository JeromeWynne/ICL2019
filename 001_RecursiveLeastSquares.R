# 25/05/2019
# Recursive Least Squares example
library(MASS)
set.seed(086324)
rm(list = ls())

d <- 100 # parameter vector dimension
T_ <- 3*d # Time horizon
X <- matrix(rnorm(T_*d, sd=20), nrow=T_)
beta <- matrix(1:d, nrow=d) # true parameter value
y <- X %*% beta + rnorm(T_, sd=1)

# Plot observation sequence
plot(1:length(y), y, type='p', xlab='Time index')

# Implement RLS algorithm
beta_hat <- matrix(rep(NA, T_*d), nrow=T_) # parameter estimates
N <- d # For example

# Initialization estimate
P <- solve(t(X[1:N, ]) %*% X[1:N, ])
R <- t(X[1:N, ]) %*% y[1:N]
beta_hat[N, ] <- P %*% R
one_step_prediction <- rep(NA, T_)

# Recursive update
for (t in (N+1):T_) {
  one_step_prediction[t] <- beta_hat[t-1, , drop=F] %*% t(X[t, , drop=F])
  k <- P %*% t(X[t, , drop=F])/(1 + X[t, , drop=F] %*% P %*% t(X[t, , drop=F]))[1]
  P <- P - k %*% X[t, , drop=F] %*% P
  beta_hat[t, ] <- beta_hat[t-1, ] + k %*% 
    (y[t] - t(beta_hat[t-1, ] %*% t(X[t, , drop=F]))[1])
}

# Batch estimate
# beta_hat_batch <- solve(t(X) %*% X) %*% t(X) %*% y

# beta_hat_batch # target
beta_hat[T_, ] # RLS estimate at time horizon

plot(beta_hat[ , 1], type='l')
abline(h=beta[1])

plot(beta_hat[ , 2], type='l')
abline(h=beta[2])

# Plot squared one-step prediction errors
plot(log((y - one_step_prediction)^2), type='l')
abline(v=2*d, col='red')
