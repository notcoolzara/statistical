# Custom function to calculate the dot product of two vectors
custom_dot_product <- function(x, y) {
  sum <- 0
  for (i in seq_along(x)) {
    sum <- sum + x[i] * y[i]
  }
  return(sum)
}

# Custom function to calculate the mean of a vector
custom_mean <- function(x) {
  sum <- 0
  for (i in seq_along(x)) {
    sum <- sum + x[i]
  }
  return(sum / length(x))
}

# Custom function to calculate the sum of squared differences
custom_sum_squared_diff <- function(x) {
  mean_val <- custom_mean(x)
  sum <- 0
  for (i in seq_along(x)) {
    sum <- sum + (x[i] - mean_val) ^ 2
  }
  return(sum)
}

# Custom function to perform linear regression and calculate required statistics
custom_linear_regression <- function(Y, X_subset) {
  # Add intercept column to X_subset
  X_subset <- cbind(Intercept = 1, X_subset)
  # Calculate coefficients using normal equation
  beta <- solve(t(X_subset) %*% X_subset) %*% t(X_subset) %*% Y
  # Calculate predictions
  Y_pred <- X_subset %*% beta
  # Calculate residuals
  residuals <- Y - Y_pred
  # Calculate TSS, RSS, RMSS, and R-Squared
  TSS <- custom_sum_squared_diff(Y)
  RSS <- custom_sum_squared_diff(residuals)
  RMSS <- sqrt(RSS / length(Y))
  R_squared <- 1 - (RSS / TSS)
  return(list(TSS = TSS, RSS = RSS, RMSS = RMSS, R_squared = R_squared))
}

# Custom function to generate all possible combinations of predictor variables
generate_combinations <- function(X) {
  n <- ncol(X)
  combinations <- list()
  for (i in 1:n) {
    comb <- combn(1:n, i)
    for (j in 1:ncol(comb)) {
      combinations <- c(combinations, list(comb[, j]))
    }
  }
  return(combinations)
}

# Custom function to perform model selection
custom_model_selection <- function(Y, X) {
  # Convert Y to numeric
  Y <- as.numeric(Y)
  
  # Get column names of X
  colnames_X <- names(X)
  
  # Generate all possible combinations of predictor variables
  combinations <- generate_combinations(X)
  
  # Initialize a list to store results
  results <- list()
  
  # Loop through each combination of predictor variables
  for (i in seq_along(combinations)) {
    combination <- combinations[[i]]
    X_subset <- X[, combination, drop = FALSE]
    regression_result <- custom_linear_regression(Y, X_subset)
    results[[i]] <- c(length(combination), paste(colnames_X[combination], collapse = ", "), unlist(regression_result))
  }
  
  # Convert the results list to a data frame
  results_df <- do.call(rbind, lapply(results, function(x) c(x[1], x[2], unlist(x[3:6]))))
  
  # Create the data frame with correct column names
  results_df <- as.data.frame(results_df, stringsAsFactors = FALSE)
  colnames(results_df) <- c("Number of Variables", "Variable (X) Name", "TSS", "RMSS", "RSS", "R-Square")
  
  # Convert columns to appropriate types
  results_df$`Number of Variables` <- as.integer(as.character(results_df$`Number of Variables`))
  results_df$TSS <- as.numeric(as.character(results_df$TSS))
  results_df$RMSS <- as.numeric(as.character(results_df$RMSS))
  results_df$RSS <- as.numeric(as.character(results_df$RSS))
  results_df$`R-Square` <- as.numeric(as.character(results_df$`R-Square`))
  
  # Sort results by R-Square in descending order
  results_df <- results_df[order(-results_df$`R-Square`), ]
  
  # Reset row indices
  rownames(results_df) <- NULL
  
  return(results_df)
}

# Data file path
file_path <- "MultRegData.txt"

# Read data from file
data <- read.table(file_path, header = TRUE)

# Extract Y and X variables
Y <- as.numeric(data[[2]])
X <- as.matrix(data[, -c(1, 2)])  # Exclude the first two columns

# Perform model selection
result_df <- custom_model_selection(Y, X)
print(result_df)

