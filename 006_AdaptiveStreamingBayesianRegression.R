set.seed(1)
rm(list=ls())
# Adaptive Bayesian linear regression
sigma2 <- 1 # regression noise

# Specify prior
mu_0 <- matrix(c(0, 0), nrow=2) # mu_0 
Sigma_0 <- matrix(c(1, 0, 0, 1), nrow=2) # sigma2 Lambda_0^-1
Lambda_0 <- solve(Sigma_0/sigma2)

# Generate drifting data
T_ <- 1000
beta <- matrix(rep(NA, T_*2), nrow=T_)
beta[1, ] <- matrix(c(2, 10), nrow=2) # initial parameter value
beta_sd = 0.1 # noise in parameter over time
for (t in 2:T_) {
  beta[t, ] <- beta[t-1, ] + rnorm(2, mean=0, sd=beta_sd)
}

X <- matrix(rnorm(T_*2, mean=0, sd=1), nrow=T_)
y <- matrix(rep(NA, T_), nrow=T_)
for (t in 1:T_) {
  y[t] <- X[t, ] %*% beta[t, ] + rnorm(1, mean=0, sd=sigma2)
}

# Compute non-drifting analytical posterior
mu_1a <- solve((t(X) %*% X) + Lambda_0) %*% (Lambda_0 %*% mu_0 + t(X) %*% y)
Lambda_1a <- (t(X) %*% X) + Lambda_0
Sigma_1a <- sigma2*solve(Lambda_1a)

# Specify parameter noise
# V_w = matrix(c(0, 0, 0, 0), nrow = 2) # assume no noise (fixed parameter)
V_w = matrix((beta_sd^2)*c(1, 0, 0, 1), nrow = 2) # modeled noise in regression parameter between time steps

# perform first regression step
Lambda_0 = solve(Sigma_0/sigma2)
X_t <- X[1, , drop=F]
y_t <- y[1, drop=F]
mu_1 <- solve((t(X_t) %*% X_t) + Lambda_0) %*% (Lambda_0 %*% mu_0 + t(X_t) %*% y_t)
Lambda_1 <- (t(X_t) %*% X_t) + Lambda_0
Sigma_1 <- sigma2*solve(Lambda_1)

# update Lambda_1 to account for parameter uncertainty
Lambda_1 <- solve((Sigma_1 + V_w)/sigma2)
betaMu <- matrix(rep(NA, 2*T_), nrow=T_)
betaMu[1, ] <- mu_1 

for (t in 2:T_) {
  X_t <- X[t, , drop=F]
  y_t <- y[t, drop=F]
  mu_1 <- solve((t(X_t) %*% X_t) + Lambda_1) %*% (Lambda_1 %*% mu_1 + t(X_t) %*% y_t)
  Lambda_1 <- (t(X_t) %*% X_t) + Lambda_1
  Sigma_1 <- sigma2*solve(Lambda_1)
  print(mu_1)
  
  # update Lambda_1 to account for parameter uncertainty
  Lambda_1 <- solve((1/sigma2)*(Sigma_1 + V_w))
  
  # record mean
  betaMu[t, ] <- mu_1
}

mu_1 # should equal
mu_1a

plot(beta[ , 1], type='l')
lines(betaMu[ , 1], col='red')

plot(beta[ , 2], type='l')
lines(betaMu[ , 2], col='red')

