rm(list=ls())
set.seed(1)

N <- 10000
for (k in c(2,3,5,10)){
  p <- c() # estimated probability that condition is triggered
  sigma <- 1
  neighborhood_radii <- seq(0.01, 4, 0.05)
  for (r in neighborhood_radii){
    eps <- rnorm(N*k, mean=0, sd=sigma) # epsilon_j's
    eps <- matrix(eps, ncol=k)
    p <- c(p, mean(apply(eps, MARGIN=1,
                         FUN=function(tmp) all(abs(diff(tmp)) < r))))
  }
  if (k==2){
    plot(neighborhood_radii, p, label=k, type='l', lty=k-1, log='y', ylim=c(0.0001, 1))
  }
  else {
    lines(neighborhood_radii, p, label=k, type='l', lty=k-1)
  }
}

p_value <- c()
for (r in neighborhood_radii){
  p_value <- c(p_value, 2*(pnorm(r) - pnorm(0)))
}
lines(neighborhood_radii, p_value, lty=3)

legend(x='bottomright', legend=c('k = 2', 'k = 3', 'k = 5', 'k = 10'), lty=c(1, 2, 4, 9))