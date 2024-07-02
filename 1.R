# Function to calculate number of observations
count_obs <- function(data) {
  n_obs <- sum(!is.na(data))
  return(n_obs)
}

# Function to calculate minimum
calc_min <- function(data) {
  min_val <- min(data, na.rm = TRUE)
  return(min_val)
}

# Function to calculate maximum
calc_max <- function(data) {
  max_val <- max(data, na.rm = TRUE)
  return(max_val)
}

# Function to calculate range
calc_range <- function(data) {
  range_val <- calc_max(data) - calc_min(data)
  return(range_val)
}

# Function to calculate sum
calc_sum <- function(data) {
  sum_val <- sum(data, na.rm = TRUE)
  return(sum_val)
}

# Function to calculate mean
calc_mean <- function(data) {
  mean_val <- mean(data, na.rm = TRUE)
  return(mean_val)
}

# Function to calculate median
calc_median <- function(data) {
  median_val <- median(data, na.rm = TRUE)
  return(median_val)
}

# Function to calculate sum of squares
calc_sum_of_squares <- function(data) {
  sum_squares <- sum(data^2, na.rm = TRUE)
  return(sum_squares)
}

# Function to calculate variance
calc_variance <- function(data) {
  variance_val <- var(data, na.rm = TRUE)
  return(variance_val)
}

# Function to calculate standard deviation
calc_std_dev <- function(data) {
  std_dev_val <- sd(data, na.rm = TRUE)
  return(std_dev_val)
}

# Function to calculate cross-products
calc_cross_products <- function(data1, data2) {
  cross_prod <- sum(data1 * data2, na.rm = TRUE)
  return(cross_prod)
}

# Function to calculate covariance
calc_covariance <- function(data1, data2) {
  cov_val <- cov(data1, data2, use = "pairwise.complete.obs")
  return(cov_val)
}

# Function to calculate correlation
calc_correlation <- function(data1, data2) {
  cor_val <- cor(data1, data2, use = "pairwise.complete.obs")
  return(cor_val)
}

# Read the text file
table_data <- read.table("DatasetNA.txt", header = TRUE, sep = " ")

# Continue with the statistical calculations as before...


# Extract the continuous variables
continuous_vars <- dataset[, 4:11]

# Calculate statistics for each continuous variable
results <- lapply(continuous_vars, function(x) {
  stats <- c(
    count_obs(x),
    calc_min(x),
    calc_max(x),
    calc_range(x),
    calc_sum(x),
    calc_mean(x),
    calc_median(x),
    calc_sum_of_squares(x),
    calc_variance(x),
    calc_std_dev(x)
  )
  return(stats)
})

# Print the results
for (i in 1:length(results)) {
  cat("\nVariable:", names(continuous_vars)[i], "\n")
  cat("Number of observations:", results[[i]][1], "\n")
  cat("Minimum:", results[[i]][2], "\n")
  cat("Maximum:", results[[i]][3], "\n")
  cat("Range:", results[[i]][4], "\n")
  cat("Sum:", results[[i]][5], "\n")
  cat("Mean:", results[[i]][6], "\n")
  cat("Median:", results[[i]][7], "\n")
  cat("Sum of squares:", results[[i]][8], "\n")
  cat("Variance:", results[[i]][9], "\n")
  cat("Standard deviation:", results[[i]][10], "\n")
}

# Calculate cross-products, covariance, and correlation between variables
cross_products <- matrix(NA, nrow = ncol(continuous_vars), ncol = ncol(continuous_vars))
covariance <- matrix(NA, nrow = ncol(continuous_vars), ncol = ncol(continuous_vars))
correlation <- matrix(NA, nrow = ncol(continuous_vars), ncol = ncol(continuous_vars))

for (i in 1:(ncol(continuous_vars) - 1)) {
  for (j in (i + 1):ncol(continuous_vars)) {
    cross_products[i, j] <- calc_cross_products(continuous_vars[, i], continuous_vars[, j])
    covariance[i, j] <- calc_covariance(continuous_vars[, i], continuous_vars[, j])
    correlation[i, j] <- calc_correlation(continuous_vars[, i], continuous_vars[, j])
  }
}

# Print cross-products
cat("\nCross-Products:\n")
print(cross_products)

# Print covariance
cat("\nCovariance:\n")
print(covariance)

# Print correlation
cat("\nCorrelation:\n")
print(correlation)

