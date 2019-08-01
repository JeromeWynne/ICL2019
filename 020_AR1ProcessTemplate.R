# Autoregressive change process
rm(list=ls())
N <- 10000
rho_ar <- 0.9
sigma_ar <- sqrt(1 - rho_ar^2)
X_t <- rep(NA, 2*N)
X_t[1:N] <- rnorm(N)
for (k in seq(N+1, 2*N)){
  X_t[k] <- rnorm(1, mean=rho_ar*X_t[k-1], sd=sigma_ar)
}

plot(X_t, type='p',
     pch=21, bg='black', cex=0.1)

mean(X_t[(N+1):(2*N)])
mean(X_t[1:N])
mean(X_t[1:N]^2)
mean(X_t[(N+1):(2*N)]^2)
