data <- read.table("MultRegData.txt", header = TRUE)
Y <- data$Y
X <- data[, -1]  # Exclude the first column (IndNo)

# Function to perform linear regression
linear_regression <- function(Y, X) {
  # Convert Y and X to numeric if they are not already
  Y <- as.numeric(Y)
  X <- as.matrix(as.data.frame(X))  # Convert to matrix
  
  # Add intercept term to X
  X <- cbind(Intercept = 1, X)
  
  # Calculate beta coefficients (b = (X'X)^-1 X'Y)
  b <- solve(t(X) %*% X) %*% t(X) %*% Y
  
  # Calculate residuals (e = Y - Y_hat)
  residuals <- Y - X %*% b
  
  # Calculate Total Sum of Squares (TSS)
  TSS <- sum((Y - mean(Y))^2)
  
  # Calculate Residual Sum of Squares (RSS)
  RSS <- sum(residuals^2)
  
  # Calculate Regression Model Sum of Squares (RMSS)
  RMSS <- TSS - RSS
  
  # Calculate R-squared (R^2)
  R_squared <- RMSS / TSS
  
  # Return results
  return(list(
    intercept = b[1],
    coefficients = b[-1],
    residuals = residuals,
    TSS = TSS,
    RSS = RSS,
    RMSS = RMSS,
    R_squared = R_squared
  ))
}

# Function to perform individual linear regression for each predictor variable
individual_linear_regression <- function(Y, X) {
  results <- list()
  predictors_names <- colnames(X)  # Get the names of predictor variables
  
  for (i in 2:ncol(X)) {  # Start from the second column
    # Extract individual predictor variable
    Xi <- X[, i]
    
    # Perform linear regression
    result <- linear_regression(Y, Xi)
    
    # Store results along with predictor variable name
    results[[predictors_names[i]]] <- result
  }
  
  return(results)
}

# Function to perform multiple linear regression for combinations of predictor variables
multiple_linear_regression <- function(Y, X, combinations) {
  results <- list()
  
  for (i in 1:length(combinations)) {
    # Extract predictor variables
    Xi <- X[, combinations[[i]]]
    
    # Construct a unique name for the combination
    combination_name <- paste("X", paste(combinations[[i]], collapse = "_"))
    
    # Perform linear regression
    result <- linear_regression(Y, Xi)
    
    # Store results with manually constructed name
    results[[combination_name]] <- result
  }
  
  return(results)
}

# Function to perform multiple linear regression for combinations of three predictor variables
multiple_linear_regression_three <- function(Y, X, combinations) {
  results <- list()
  
  for (i in 1:length(combinations)) {
    # Extract predictor variables
    Xi <- X[, combinations[[i]]]
    
    # Perform linear regression
    result <- linear_regression(Y, Xi)
    
    # Store results
    results[[paste("X", paste(combinations[[i]], collapse = "_"), sep = "")]] <- result
  }
  
  return(results)
}

# Function to perform multiple linear regression for combinations of four predictor variables
multiple_linear_regression_four <- function(Y, X, combinations) {
  results <- list()
  
  for (i in 1:length(combinations)) {
    # Extract predictor variables
    Xi <- X[, combinations[[i]]]
    
    # Perform linear regression
    result <- linear_regression(Y, Xi)
    
    # Store results
    results[[paste("X", paste(combinations[[i]], collapse = "_"), sep = "")]] <- result
  }
  
  return(results)
}

# Function to perform multiple linear regression for combinations of five predictor variables
multiple_linear_regression_five <- function(Y, X, combinations) {
  results <- list()
  
  for (i in 1:length(combinations)) {
    # Extract predictor variables
    Xi <- X[, combinations[[i]]]
    
    # Perform linear regression
    result <- linear_regression(Y, Xi)
    
    # Store results
    results[[paste("X", paste(combinations[[i]], collapse = "_"), sep = "")]] <- result
  }
  
  return(results)
}

# Function to perform multiple linear regression for combinations of six predictor variables
multiple_linear_regression_six <- function(Y, X, combinations) {
  results <- list()
  
  for (i in 1:length(combinations)) {
    # Extract predictor variables
    Xi <- X[, combinations[[i]]]
    
    # Perform linear regression
    result <- linear_regression(Y, Xi)
    
    # Store results
    results[[paste("X", paste(combinations[[i]], collapse = "_"), sep = "")]] <- result
  }
  
  return(results)
}

# Function to perform multiple linear regression for combinations of seven predictor variables
multiple_linear_regression_seven <- function(Y, X, combinations) {
  results <- list()
  
  for (i in 1:length(combinations)) {
    # Extract predictor variables
    Xi <- X[, combinations[[i]]]
    
    # Perform linear regression
    result <- linear_regression(Y, Xi)
    
    # Store results
    results[[paste("X", paste(combinations[[i]], collapse = "_"), sep = "")]] <- result
  }
  
  return(results)
}

individual_results <- individual_linear_regression(Y, X)
# Print individual linear regression results
for (variable_name in names(individual_results)) {
  cat("Linear regression for variable:", variable_name, "\n")
  cat("Intercept:", individual_results[[variable_name]]$intercept, "\n")
  cat("Coefficients:", individual_results[[variable_name]]$coefficients, "\n")
  cat("R-squared:", individual_results[[variable_name]]$R_squared, "\n\n")
}

# Define combinations of independent variables
combinations <- list()
for (i in 2:(ncol(X) - 1)) {
  for (j in (i + 1):ncol(X)) {
    combinations <- c(combinations, list(c(i, j)))
  }
}
results <- multiple_linear_regression(Y, X, combinations)
# Print multiple linear regression results
for (i in 1:length(results)) {
  cat("Regression with variables:", paste(names(X)[combinations[[i]]], collapse = ", "), "\n")
  cat("Intercept:", results[[i]]$intercept, "\n")
  cat("Coefficients:", results[[i]]$coefficients, "\n")
  cat("R-squared:", results[[i]]$R_squared, "\n\n")
}

# Define combinations of independent variables with three variables
combinations_three <- list()
for (i in 1:(ncol(X) - 2)) {
  for (j in (i + 1):(ncol(X) - 1)) {
    for (k in (j + 1):ncol(X)) {
      combinations_three <- c(combinations_three, list(c(i, j, k)))
    }
  }
}
results_three <- multiple_linear_regression_three(Y, X, combinations_three)
# Print multiple linear regression results with three variables
for (i in 1:length(results_three)) {
  # Check if Y is included in the combination and skip
  if ("Y" %in% names(X)[combinations_three[[i]]]) {
    next
  }
  cat("Regression with variables:", paste(names(X)[combinations_three[[i]]], collapse = ", "), "\n")
  cat("Intercept:", results_three[[i]]$intercept, "\n")
  cat("Coefficients:", results_three[[i]]$coefficients, "\n")
  cat("R-squared:", results_three[[i]]$R_squared, "\n\n")
}


# Define combinations of independent variables with four variables
combinations_four <- list()
for (i in 1:(ncol(X) - 3)) {
  for (j in (i + 1):(ncol(X) - 2)) {
    for (k in (j + 1):(ncol(X) - 1)) {
      for (l in (k + 1):ncol(X)) {
        combinations_four <- c(combinations_four, list(c(i, j, k, l)))
      }
    }
  }
}
results_four <- multiple_linear_regression_four(Y, X, combinations_four)
# Print multiple linear regression results with four variables
for (i in 1:length(results_four)) {
  # Check if Y is included in the combination and skip
  if ("Y" %in% names(X)[combinations_four[[i]]]) {
    next
  }
  cat("Regression with variables:", paste(names(X)[combinations_four[[i]]], collapse = ", "), "\n")
  cat("Intercept:", results_four[[i]]$intercept, "\n")
  cat("Coefficients:", results_four[[i]]$coefficients, "\n")
  cat("R-squared:", results_four[[i]]$R_squared, "\n\n")
}

# Define combinations of independent variables with five variables
combinations_five <- list()
for (i in 1:(ncol(X) - 4)) {
  for (j in (i + 1):(ncol(X) - 3)) {
    for (k in (j + 1):(ncol(X) - 2)) {
      for (l in (k + 1):(ncol(X) - 1)) {
        for (m in (l + 1):ncol(X)) {
          combinations_five <- c(combinations_five, list(c(i, j, k, l, m)))
        }
      }
    }
  }
}

# Perform multiple linear regression with five variables
results_five <- multiple_linear_regression_five(Y, X, combinations_five)

# Print multiple linear regression results with five variables
for (i in 1:length(results_five)) {
  # Check if Y is included in the combination and skip
  if ("Y" %in% names(X)[combinations_five[[i]]]) {
    next
  }
  cat("Regression with variables:", paste(names(X)[combinations_five[[i]]], collapse = ", "), "\n")
  cat("Intercept:", results_five[[i]]$intercept, "\n")
  cat("Coefficients:", results_five[[i]]$coefficients, "\n")
  cat("R-squared:", results_five[[i]]$R_squared, "\n\n")
}

# Define combinations of independent variables with six variables
combinations_six <- list()
for (i in 1:(ncol(X) - 5)) {
  for (j in (i + 1):(ncol(X) - 4)) {
    for (k in (j + 1):(ncol(X) - 3)) {
      for (l in (k + 1):(ncol(X) - 2)) {
        for (m in (l + 1):(ncol(X) - 1)) {
          for (n in (m + 1):ncol(X)) {
            combinations_six <- c(combinations_six, list(c(i, j, k, l, m, n)))
          }
        }
      }
    }
  }
}

# Perform multiple linear regression with six variables
results_six <- multiple_linear_regression_six(Y, X, combinations_six)

# Print multiple linear regression results with six variables
for (i in 1:length(results_six)) {
  # Check if Y is included in the combination and skip
  if ("Y" %in% names(X)[combinations_six[[i]]]) {
    next
  }
  cat("Regression with variables:", paste(names(X)[combinations_six[[i]]], collapse = ", "), "\n")
  cat("Intercept:", results_six[[i]]$intercept, "\n")
  cat("Coefficients:", results_six[[i]]$coefficients, "\n")
  cat("R-squared:", results_six[[i]]$R_squared, "\n\n")
}

# Define combinations of independent variables with seven variables
combinations_seven <- list()
for (i in 1:(ncol(X) - 6)) {
  for (j in (i + 1):(ncol(X) - 5)) {
    for (k in (j + 1):(ncol(X) - 4)) {
      for (l in (k + 1):(ncol(X) - 3)) {
        for (m in (l + 1):(ncol(X) - 2)) {
          for (n in (m + 1):(ncol(X) - 1)) {
            for (o in (n + 1):ncol(X)) {
              combinations_seven <- c(combinations_seven, list(c(i, j, k, l, m, n, o)))
            }
          }
        }
      }
    }
  }
}

# Perform multiple linear regression with seven variables
results_seven <- multiple_linear_regression_seven(Y, X, combinations_seven)

# Print multiple linear regression results with seven variables
for (i in 1:length(results_seven)) {
  # Check if Y is included in the combination and skip
  if ("Y" %in% names(X)[combinations_seven[[i]]]) {
    next
  }
  cat("Regression with variables:", paste(names(X)[combinations_seven[[i]]], collapse = ", "), "\n")
  cat("Intercept:", results_seven[[i]]$intercept, "\n")
  cat("Coefficients:", results_seven[[i]]$coefficients, "\n")
  cat("R-squared:", results_seven[[i]]$R_squared, "\n\n")
}