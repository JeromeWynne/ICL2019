set.seed(1)
rm(list=ls())
# Bayesian linear regression
sigma2 <- 1 # noise

# Prior
mu_0 <- matrix(c(0, 0), nrow=2) # mu_0 
Sigma_0 <- 10*matrix(c(1, 0, 0, 1), nrow=2) # sigma2 Lambda_0^-1
Lambda_0 <- solve(Sigma_0/sigma2)

# Data
T_ <- 100
beta <- matrix(c(2, 10), nrow=2)
X <- matrix(rnorm(2*T_), nrow=T_)
y <- X %*% beta + rnorm(T_)

# Posterior
mu_1a <- solve((t(X) %*% X) + Lambda_0) %*% (Lambda_0 %*% mu_0 + t(X) %*% y)
Lambda_1a <- (t(X) %*% X) + Lambda_0
Sigma_1a <- sigma2*solve(Lambda_1a)

# Then all we need to do is to update Sigma_1 to Sigma_1 + V_w and recompute Lambda_1
T_ = 100 # stream length
V_w = matrix(c(0, 0, 0, 0), nrow = 2) # modeled noise in regression parameter between time steps

# perform first regression step
Lambda_0 = solve(Sigma_0/sigma2)
X_t <- X[1, , drop=F]
y_t <- y[1, drop=F]
mu_1 <- solve((t(X_t) %*% X_t) + Lambda_0) %*% (Lambda_0 %*% mu_0 + t(X_t) %*% y_t)
Lambda_1 <- (t(X_t) %*% X_t) + Lambda_0
Sigma_1 <- sigma2*solve(Lambda_1)

# update Lambda_1 to account for parameter uncertainty
Lambda_1 <- Lambda_1 + V_w
betaM <- matrix(rep(NA, 2*T_), nrow=T_)
betaM[1, ] <- mu_1 

for (t in 2:T_) {
  X_t <- X[t, , drop=F]
  y_t <- y[t, drop=F]
  mu_1 <- solve((t(X_t) %*% X_t) + Lambda_1) %*% (Lambda_1 %*% mu_1 + t(X_t) %*% y_t)
  Lambda_1 <- (t(X_t) %*% X_t) + Lambda_1
  Sigma_1 <- sigma2*solve(Lambda_1)
  print(mu_1)
  
  # update Lambda_1 to account for parameter uncertainty
  Lambda_1 <- Lambda_1 + V_w
  
  # record mean
  betaM[t, ] <- mu_1
}

mu_1 # should equal
mu_1a
