# First crack at the data-generating engine.
rm(list=ls())
set.seed(69)

# !!! DATA GENERATION !!!

# Specify:
# - Time horizon
# - Parameter dimension
# - Jump size
# - Initial parameter value
# - Post-jump parameter value
# - Jump location (index)

T_ <- 501
p  <- 2 # parameter dimension, first dimension is always intercept
jump.mag <- 2 # jump (size and direction)
jump.loc <- 250 # jump index (occurs between 249-250)
beta0 <- -1*rep(1, p)
beta1 <- beta0 + jump.mag*rep(1, p)
beta  <- matrix(rep(NA, T_*p), nrow=T_, ncol=p)
beta[1:(jump.loc-1), ] <- beta0
beta[jump.loc:T_, ] <- beta1

# Design matrix's features are sampled from the multivariate normal distn.
X <- matrix(rnorm(T_*p, mean=0, sd=1), nrow=T_)
X[ , 1] <- 1 # Intercept
sigma <- 0.5 # error standard deviation
eps <- rnorm(T_, mean=0, sd=sigma)
y <- sapply(1:T_, FUN = function(t){
        X[t, , drop=F] %*% t(beta[t, , drop=F]) + eps[t]
      })

# Plot resulting data stream
plot(X[1:(jump.loc-1), p], y[1:(jump.loc-1)], col='red')
points(X[jump.loc:T_, p], y[jump.loc:T_], col='blue')

plot(X[, p], y, type='p')

# FIT A STREAMING MODEL


