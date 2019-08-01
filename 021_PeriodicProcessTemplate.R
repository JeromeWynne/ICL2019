rm(list=ls())
T_ <- 10
j <- seq(1, T_)
sigma_P <- 0.1
sigma_P <- sqrt(1/(1 - mean(sin(2*pi*j/T_)^2)))

N <- 1000
X_t <- rnorm(N, mean=0, sd=1)
X_t <- c(X_t, rnorm(N, mean=sin(2*pi*seq(1, N)/T_), sd=1/sigma_P))

plot(X_t)
EX <- c(rep(0, N), sin(2*pi*seq(1, N)/T_))
lines(EX)

mean(X_t[(N+1):(2*N)]^2)
mean(X_t[1:N]^2)

