
rm(list = ls())

set.seed(420)
sample_1 <- rnorm(n = 100, mean = 0, sd = 1)
sample_2 <- rnorm(n = 100, mean = 5, sd = 2)


realFunction <- function(x){
  ret <- (dnorm(x, mean = 0, sd = 1) + dnorm(x, mean = 5, sd = 2))/2
  return(ret)
}

sample_full <- c(sample_1, sample_2)


# 2. Fit the Kernel Density Estimation
dens <- density(sample_full)

# 3. Create an approximation function from the KDE
dens_func <- approxfun(dens$x, dens$y)

# 4. Estimate the probability of values falling between 40 and 60
# P(40 <= X <= 60)
prob_interval <- integrate(dens_func, lower = 0, upper = 6)

# View the estimated probability
print(prob_interval$value)


p_realFunction <- function(x){
  ret <- (pnorm(q = x, mean = 0, sd = 1) + pnorm(q = x, mean = 5, sd = 2))/2
  return(ret)
}

print(p_realFunction(6) - p_realFunction(0))
