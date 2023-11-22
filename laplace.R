# inverse CDF
inverse_cdf_laplace <- function(n) {
  U <- runif(n)
  X <- ifelse(U < 0.5, log(2*U), -log(2*(1-U)))
  return(X)
}

# Generate 10000 random numbers and plot histogram
set.seed(123)
x <- inverse_cdf_laplace(10000)
hist(x, breaks=50, prob=TRUE, main = "Histogram of Laplace Distribution by Inverse CDF method", xlab = "X", col = "lightblue", freq = FALSE)

# Add theoretical density
curve(dexp(abs(x), rate=1/2)/2, add=TRUE, col="red", lwd=2)


# rejection sampling for Normal distribution
rejection_sampling_normal <- function(n) {
  a <- sqrt(2 *exp(1)/pi) # Envelop constant for Laplace distribution
  X <- rep(NA, n)
  i <- 1
  count_rejected <- 0

  while(i <= n) {
    Z <- inverse_cdf_laplace(1)
    U <- runif(1)
    f_y <- 2/sqrt(2*pi) * exp(-Z^2/2)
    g_y <- exp(-abs(Z))
    if (U <= (f_y /(a * g_y))) {
      X[i] <- Z
      i <- i + 1
    } else {
      count_rejected <- count_rejected + 1
    }
  }

  rejection_rate <- count_rejected / n
  return(list(samples = X, rejection_rate = rejection_rate))
}

# Generate 2000 random numbers and plot histogram
set.seed(123)
results <- rejection_sampling_normal(2000)
x <- results$samples
hist(x, breaks=100, prob=TRUE, main = "Histogram of Normal Distribution by Rejection Sampling", xlab = "X", col = "lightblue", freq = FALSE)

# Add theoretical density
curve(dnorm(x), add=TRUE, col="red", lwd=2)

# Compute rejection rate
rejection_rate <- results$rejection_rate

# Compare rejection rates
cat("Average rejection rate (R):", rejection_rate, "\n")
cat("Expected rejection rate (ER):", 1 - (1 / sqrt(2 *exp(1)/pi)) , "\n")

# Generate 2000 random numbers using rnorm and plot histogram
y <- rnorm(2000)
hist(y, breaks=100, prob=TRUE, main = "rnorm", xlab = "X", col = "lightblue", freq = FALSE)

# Add theoretical density
curve(dnorm(x), add=TRUE, col="red", lwd=2)








