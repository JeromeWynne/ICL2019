neigborhood_radii <- seq(0.01, 5, 0.005)
for (eps_j in seq(0, 3, 1)){
  p_value <- c()
  for (r in neigborhood_radii){
    p_value <- c(p_value, diff(pnorm(c(eps_j-r, eps_j+r))))
  }
  if (eps_j==0){
    plot(neigborhood_radii, p_value, type='l', lty=eps_j+1, log='xy', ylim=c(0.001, 1))
  }
  else{
    lines(neigborhood_radii, p_value, lty=eps_j+1)
  }
}

legend(x='bottomright',legend=sapply(seq(0,3), function(n) paste('eps_j = ', n)), lty=seq(1,4))
abline(h=0.05)



# so if I simulate a bunch of data and configure the radius as a function of position,
# I can determine a p-value empirically.
