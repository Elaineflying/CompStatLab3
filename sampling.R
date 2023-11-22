# rejection sampling
rejection_sampling <- function(n) {
  x <- runif(n, min = -1, max = 1)
  y <- runif(n, min = 0, max = 1)
  fx <- ifelse(x <= 0, x + 1, 1 - x)
  accepted <- y <= fx
  return(x[accepted])
}

# composition sampling
composition_sampling <- function(n) {
  Y <- ifelse(runif(n) < 0.5, sqrt(runif(n)) - 1, 1 - sqrt(1 - runif(n)))
  X <- ifelse(runif(n) < 0.5, Y, -Y)
  return(X)
}

# difference of uniforms
difference_of_uniforms <- function(n) {
  U1 <- runif(n)
  U2 <- runif(n)
  return(U1 - U2)
}


# checking the random generators
set.seed(123)
n <- 10000
par(mfrow = c(3, 1))

# Rejection Sampling
x_rejection_samples <- rejection_sampling(n)
hist(x_rejection_samples, main = "Rejection Sampling", xlab = "X", col = "lightblue", freq = FALSE)

# Composition Sampling
x_composition_samples <- composition_sampling(n)
hist(x_composition_samples, main = "Composition Sampling", xlab = "X", col = "lightblue", freq = FALSE)

# Difference of Uniforms
x_difference_uniforms_samples <- difference_of_uniforms(n)
hist(x_difference_uniforms_samples, main = "Difference of Uniforms", xlab = "X", col = "lightblue", freq = FALSE)

# Calculate variance for each method
var_rejection <- var(x_rejection_samples)
var_composition <- var(x_composition_samples)
var_difference <- var(x_difference_uniforms_samples)

cat("Variance of X (Rejection Sampling):", var_rejection, "\n")
cat("Variance of X (Composition Sampling):", var_composition, "\n")
cat("Variance of X (Differences of Uniforms):", var_difference, "\n")






