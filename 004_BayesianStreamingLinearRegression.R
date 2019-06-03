# Streaming Bayesian linear regression
library(MASS)
rm(list=ls())

T_ = 100 # stream length
mu_0 = matrix(c(0, 0), nrow=2)
Sigma_0 = matrix(c(1, 0, 0, 1), nrow=2)
V_w = matrix(c(0, 0, 0, 0), nrow = 2) # modeled noise in regression parameter between time steps
sigma2 = 0.1 # modeled & true linear regression noise

beta_true <- matrix(c(2, 4), nrow=2) # i.e. no noise in parameter
X <- matrix(rnorm(2*T_), nrow=T_)
y <- X %*% beta_true + rnorm(T_, mean=0, sd=sqrt(sigma2))

# perform first regression step
Lambda_0 = solve(Sigma_0/sigma2)
X_t <- X[1, , drop=F]
y_t <- y[1, drop=F]
mu_1 <- solve((t(X_t) %*% X_t) + Lambda_0) %*% (Lambda_0 %*% mu_0 + t(X_t) %*% y_t)
Lambda_1 <- (t(X_t) %*% X_t) + Lambda_0
Sigma_1 <- sigma2*solve(Lambda_1)

# update Lambda_1 to account for parameter uncertainty
Lambda_1 <- Lambda_1 + V_w

for (t in 2:T_) {
  X_t <- X[t, , drop=F]
  y_t <- y[t, drop=F]
  mu_1 <- solve((t(X_t) %*% X_t) + Lambda_1) %*% (Lambda_1 %*% mu_1 + t(X_t) %*% y_t)
  Lambda_1 <- (t(X_t) %*% X_t) + Lambda_1
  Sigma_1 <- sigma2*solve(Lambda_1)
  #print(mu_1)
  
  # update Lambda_1 to account for parameter uncertainty
  Lambda_1 <- Lambda_1 + V_w
}

mu_1 # should equal
solve(t(X) %*% X) %*% t(X) %*% y


