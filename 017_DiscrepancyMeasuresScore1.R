require(ggplot2)

rm(list=ls())
set.seed(10)

BETA <- 1
EPS_VAR <- 1

rmodel <- function(n=10) {
  x    <- rnorm(n=n, mean=0, sd=3)
  y    <- rnorm(n=n, mean=BETA*x, sd=EPS_VAR) # error variance is 1
  return(cbind(x, y))
}

data.xy <- rmodel(n=1090)

score1 <- function(x, y) {
  p <- pnorm(y-BETA*x, mean=0, sd=EPS_VAR)
  score <- min(p, 1-p)
  return(score)
}

data.score1 <- apply(data.xy, MARGIN=1, FUN=function(r) score1(r[1], r[2]))

grid.xlim <- c(-6, 6)
grid.ylim <- c(-6, 6)
grid.yres <- 0.05
grid.xres <- 0.05
grid.nx   <- length(seq(grid.xlim[1], grid.xlim[2], grid.xres))
grid.ny   <- length(seq(grid.ylim[1], grid.ylim[2], grid.yres))
grid.xdata <- rep(seq(grid.xlim[1], grid.xlim[2], grid.xres), each=grid.ny)
grid.ydata <- rep(seq(grid.ylim[1], grid.ylim[2], grid.yres), grid.nx)
grid.score1 <- apply(cbind(grid.xdata, grid.ydata), MARGIN=1, FUN=function(r) score1(r[1], r[2]))

plot.xlim <- c(-5, 5)
plot.ylim <- c(-5, 5)

score1.threshold <- 1/1000
grid.score1.mask <- grid.score1 < score1.threshold

ggplot() + 
  geom_point(aes(x=grid.xdata, y=grid.ydata, fill=grid.score1), size=2, stroke=0, pch=21) +
  geom_point(aes(x=grid.xdata[grid.score1.mask], y=grid.ydata[grid.score1.mask]),
             fill='red', alpha=0.05, size=2, stroke=0, pch=21) +
  coord_cartesian(xlim=plot.xlim, ylim=plot.ylim) +
  geom_point(aes(x=data.xy[,1], y=data.xy[,2], fill=data.score1), colour='white', pch=21) +
  scale_colour_gradient(low = "#000000", high = "#FFFFFF")

# what's the diameter of a circle that contains a given proportion of observations?
# write the multivariate normal density function in cartesian coordinates, integrate
# over theta, then integrate over the radius from 0 to R. Do this first thing tomorrow morning.
# we can actually generalize this idea to an arbitrarily large number of dimensions.

# it's basically a structured way to partition the space the data sits in

# the ultimate single data-point measure of performance is the p value

# what characteristics does a sequence of draws from this distribution have?
# define a function
# run simulations under the model to obtain a distribution for that statistic under the model
# specify a p-value against that statistic
# how can we define statistics that are orthogonal? Maybe seek statistics that are indepedent 
# under the model?
# Given statistics S(X) and T(X), how can construct a statistic that is independent of T(X) but
# is a function of S(X)?