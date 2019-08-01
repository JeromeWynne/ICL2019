# Generate a trajectory of fixed length,
# with N jumps
# and gaussian distribution otherwise (for X)
# with a linear relationship between response and covariate.
# Start with a univariate response (i.e. mean estimation as regression)
library(ggplot2)
library(gridExtra)

rm(list=ls())
set.seed(14042)

time_horizon <- 1000
n_jumps <- 1
jump <- rep(0, time_horizon) # can be replaced with a scalar
jump_locs <- sample.int(time_horizon, size=n_jumps, replace=F)
jump[jump_locs] <- c(4)
mu  <- 0 # initial mu value
sigma2 <- 1 # initial sigma2 value
y   <- rep(NA, time_horizon)

for (t in 1:time_horizon) {
  mu <- mu + jump[t]
  y[t] <- rnorm(1, mean=mu, sd=sqrt(sigma2))
}

# recursively estimate the mean
# optimize residual sum-of-squares error w.r.t. lambda
rss <- function(lambda){
  y_bar <- rep(NA, time_horizon)
  y_bar[1] <- 0 # value at time step is PREDICTION
  for (t in 2:time_horizon) {
    y_bar[t] <- lambda*y_bar[t-1] + (1 - lambda)*y[t-1]
  }
  return(sum((y_bar - y)^2))
}

lambda_grid <- seq(0.1, 0.99, by=0.01)
rss_by_lambda <- sapply(lambda_grid, FUN=rss)
ggplot() + geom_line(aes(x=lambda_grid, y=rss_by_lambda)) + scale_y_log10()
lambda <- lambda_grid[which.min(rss_by_lambda)]

y_bar <- rep(NA, time_horizon)
sigma2_bar <- rep(NA, time_horizon)
y_bar[1] <- 0 # value at time step is PREDICTION
sigma2_bar[1] <- 1
for (t in 2:time_horizon) {
  y_bar[t] <- lambda*y_bar[t-1] + (1 - lambda)*y[t-1]
  sigma2_bar[t] <- lambda*sigma2_bar[t-1] + (1 - lambda)*(y[t-1] - y_bar[t-1])^2 # asymptotically unbiased
}

# compute the bias summary statistic w.r.t. time
s <- rep(NA, time_horizon)
p <- rep(NA, time_horizon)
B <- rep(NA, time_horizon)
W <- 30  # compute over 30 points
a <- 0.4 # P(0.6 >= p >= 0.4|a_{t-W:t}, K)
b <- 0.6

for (t in (W+1):time_horizon){
  s[t] <- sum((y[(t-W):t] - y_bar[(t-W):t]) > 0)
  p[t] <- diff(pbeta(c(a, b), shape1=1+s[t], shape2=1+W-s[t]))
  B[t] <- p[t]/(1 - p[t]) # Posterior odds
}

e_hat <- (y - y_bar)/sigma2_bar

display_df <- data.frame(cbind(y, 1:time_horizon, y_bar, sigma2_bar, e_hat))
colnames(display_df) <- c('y', 't', 'y_bar', 'sigma2_bar', 'e_hat')
p1 <- ggplot() + geom_point(aes(t, y), data=display_df, size=0.5) + 
  geom_vline(xintercept=jump_locs, linetype='dotted') +
  geom_line(aes(t, y_bar), data=display_df, color='red', size=0.5) +
  geom_line(aes(t, y_bar + sqrt(sigma2_bar)*2), data=display_df, color='red', alpha=0.5) +
  geom_line(aes(t, y_bar - sqrt(sigma2_bar)*2), data=display_df, color='red', alpha=0.5) + 
  xlab('Time')
p2 <- ggplot() + geom_point(aes(t, e_hat), data=display_df, size=0.5)
p3 <- ggplot() + geom_line(aes(display_df$t, p), size=0.5) +
      geom_vline(xintercept=jump_locs, linetype='dotted') +
      ylab('Probability of symmetry') + xlab('Time') + geom_hline(yintercept=0.5, colour='blue',
                                                                  linetype='dashed') +
      ylim(0, 1)
p4 <- ggplot() + geom_line(aes(display_df$t, B), size=0.5) +
  geom_vline(xintercept=jump_locs, linetype='dotted') + scale_y_log10() +
  ylab('Odds of symmetry') + xlab('Time') + geom_hline(yintercept=1, colour='blue',
                                                       linetype='dashed')
grid.arrange(p1, p2, p3, p4, ncol=1, nrow=4)
