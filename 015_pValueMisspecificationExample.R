library(ggplot2)
require(gridExtra)

mu <- 0.1
N  <- 300
mu_model <- 0
y <- rnorm(n=N, mean=mu, sd=1)
n <- seq(1, N)
y_bar <- cumsum(y)/n


p_values <- pnorm(y_bar, mean=mu_model, sd=sqrt(1/n)) # p-value under true distribution

p1 <- ggplot() + geom_line(aes(x=n, y=p_values)) + coord_cartesian(ylim=c(0, 1)) + theme_bw() +
      ylab('') + xlab('n, sample size')
p2 <- ggplot() + geom_line(aes(x=n, y=y_bar)) + 
      geom_hline(yintercept=mu, alpha=0.2, colour='red') + 
      geom_hline(yintercept=mu_model, alpha=0.2, colour='blue') + coord_cartesian(ylim=c(-1, 1)) + theme_bw() +
      ylab('p-value') + xlab('n, sample size')
grid.arrange(p1, p2, nrow=2)

# Degree of misspecification is not well-described by a p-value. This same argument applies against Bayes
# factors.
