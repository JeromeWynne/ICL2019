# The Conformal Algorithm Using Old Examples Alone (i.e. without a new object)
# Input: Nonconformity measure A, significance level ε, examples z1,...,zn−1, example z,
# Task: Decide whether to include z in γε(z1,...,zn-1)

# Algorithm:
#  1. Provisionally set zn := z.
#  2. For i = 1,...,n, set αi := A({z1,...,zn}\{zi},zi).
#  3. Set pz :=  (number of i such that 1 ≤ i ≤ n and αi ≥ αn)/n.
#  4. Include z in γε(z1,...,zn−1) if and only if pz > ε.
  
# Sample the zi from a normal distribution.

# Compute non-conformity as absolute distance (defines A) between point and mean.

# Take significance level to be 0.95.

set.seed(2)
rm(list=ls())
A <- function(zbag, zi) return((mean(zbag) - zi)^2) # computed for all zn jointly
zmax <- 30
zmin <- 0
zres <- (zmax - zmin)/100


N <- 100 # number of data points in sequence
zvalues <- rchisq(N, df=10)
epsilon <- 0.1 # significance level, low values indicate strong non-conformity
for (n in 2:N){
  gamma_set <- c() # empty confidence prediction set
  zstar <- zvalues[n]
  for (z in seq(zmin, zmax, zres)){
    alpha <- rep(NA, n)
    zvalues[n] <- z
    for (i in 1:n){
      alpha[i] <- A(zvalues[1:n][-i], zvalues[i])
    }
    pz <- sum(alpha >= alpha[n])/n
    if (pz > epsilon) gamma_set <- c(gamma_set, z)
  }
  zvalues[n] <- zstar
  if (n==2) {
    plot(rep(n, length(gamma_set)), gamma_set, xlim=c(0, N), ylim=c(zmin, zmax), pch=21,
         cex=0.2, bg='black')
  }
  else {
    points(rep(n, length(gamma_set)), gamma_set, pch=21, bg='black', cex=0.2)
  }
}

print(c('Obtained confidence level was ', pchisq(max(gamma_set), df=10) - pchisq(min(gamma_set), df=10)))
print(c('Target confidence level was ', 1-epsilon))
points(1:N, zvalues, pch=21, bg='black', cex=0.5)
