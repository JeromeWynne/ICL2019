
# What about just creating an object instead
# design matrix takes the form of a matrix
# univariate response

# We want to:
#   - Generate a data stream.
#   - Fit models to that data stream.
#   - Evaluate the relative performance of those models.
#   - Apply custom diagnostics to the model and the data stream.
# Start by considering a linear Gaussian data-generating process that
# undergoes a single step-change in parameter values.

# Model object should have properties:
# <method> fit(X, y) # fit model to data stream
# <prop>   beta  # sequence of learnt parameter values
# <prop>   fit_parameters # fitting hyperparameters (e.g. forgetting factor)

# Stream object should have properties:
# <prop>   seed # seed for generating random data
# <method> generate() # generate X, y, beta
# <prop>   th # time horizon (number of datapoints)
# <prop>   beta # sequence of true parameter values
# <prop>   X # sequence of covariates
# <prop>   y # sequence of response values
# <prop>   p # coefficient dimension
# <prop>   eps # errors are stored explicitly
# Convention is that the intercept is always included,
# all stream-wise data is stored as a matrix for which the t'th row
# corresponds to the parameter value at time t.
rm(list=ls())

# STREAM INITIALIZATION
JUMP_LOC <- 500
th_tmp <- 1000
p_tmp <- 2
beta_tmp <- matrix(rep(NA, th_tmp*p_tmp), nrow=th_tmp)
beta_tmp[1:JUMP_LOC, ] <- 0 # No relationship initially
beta_tmp[(JUMP_LOC+1):th_tmp, ] <- 1 # Jumps to 1 mid-way

Stream <- list(
            seed=1,
            p=p_tmp,
            th=th_tmp, # time horizon
            beta=beta_tmp
          )

Stream$generate <- function() {
  # X ~ MN(0, I)
  # eps ~iid N(0, 1)
  set.seed(Stream$seed)
  sigma <- 1
  Stream$X <- matrix(rnorm(Stream$th*Stream$p, mean=0, sd=1),
                     nrow=Stream$th) 
  Stream$X[ , 1] <- 1
  Stream$eps <- matrix(rnorm(Stream$th, mean=0, sd=sigma), nrow=Stream$th) #sigma2 is 1!!!!
  Stream$y <- sapply(1:Stream$th, FUN = function(t) {
                Stream$X[t, , drop=F] %*% t(Stream$beta[t, , drop=F]) + Stream$eps[t]
              })
  return(Stream)
}

Stream <- Stream$generate()

# MODEL INITIALIZATION
random_beta <- (Stream$beta)^2 + rnorm(length(Stream$beta))*0.1

Model <- list(
          p=Stream$p,
          th=Stream$th,
          beta=random_beta,
          mse=NA,
          predictions=NA,
          sigma=1
          )

# Get model MSE
predict <- function(stream, model) {
  sapply(1:stream$th,
         function(t) (stream$X[t, , drop=F] 
                      %*% t(model$beta[t, , drop=F])))
}
mse <- function(v1, v2) {
  return(mean((v1 - v2)^2))
}
euclid_distance <- function(mat1, mat2) { # computes distance between each row of mat1 and mat2
                    sapply(1:nrow(mat1), function(j) sqrt(sum((mat1[j, ]-mat2[j, ])^2)))
                    }

Model$predictions <- predict(Stream, Model)
Model$mse <- mse(Stream$y, Model$predictions)
Model$residual <- (Stream$y - Model$predictions)
Model$beta_distance <- euclid_distance(Model$beta, Stream$beta)

# Model evaluation
# How are the models evaluated?
# > Mean-squared one-step prediction error.
# > Mean-squared parameter error.
# > p-values
plot(Model$residual)
plot(Model$beta_distance)

# Generate a sequence of samples from the model
# assume initially that sigma is known
sample <- function(Stream, Model, seed=1) {
            sapply(1:Model$th, function(t) Model$beta[t, ,drop=F] %*% Stream$X[t, ] + rnorm(1)*Model$sigma)
          }
Model$sample <- sample(Stream, Model)
plot(Model$sample, col='red')
points(Stream$y, col='black')

# Added variable plots might genuinely be helpful

