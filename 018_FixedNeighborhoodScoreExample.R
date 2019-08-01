rm(list=ls())

data <- as.numeric(AirPassengers)
x <- seq(1, length(data))
plot(x, data)

model <- lm(data ~ 1 + x)
abline(a=model$coefficients)

# evaluate runs (ignore the fact we're looking at residuals)
sigma <- sqrt(sum(model$residuals^2)/(length(data)-1))
r <- 0.1
diagnosis <- c(FALSE, abs(diff(model$residuals))<(r*sigma))
diagnosis[-length(data)] <- ((diagnosis[-1] + diagnosis[-length(data)]) > 0)

points(x=x[diagnosis], y=data[diagnosis], bg='red', pch=21)

plot(x, model$residuals)
points(x[diagnosis], model$residuals[diagnosis], bg='red', pch=21)

# Next idea: can we rescale the interval based on the residual's position?
# e.g. if epsilon_j is of known value, what is the probability of symmetric interval around it 
# that is of radius r? We now have the p-value as a function of both r and the eps_j.
# plot r versus p-value as a function of eps_j.