# Read the dataset 
table <- read.table("DatasetNA.txt", header = TRUE, dec = ",")
ztable_head <- head(table, 10)

# Minimum Function (no changes)
minimum <- function(column, na.rm) {
  count <- 0
  min_val <- Inf
  for (i in column) {
    if (!is.na(i)) {
      if (count == 0) {
        min_val <- i
      }
      if (i < min_val) {
        min_val <- i
      }
      count <- count + 1
    }
  }
  return(min_val)
}

# Maximum Function (no changes)
maximum <- function(column, na.rm) {
  count <- 0
  max_val <- -Inf
  for (i in column) {
    if (!is.na(i)) {
      if (count == 0) {
        max_val <- i
      }
      if (i > max_val) {
        max_val <- i
      }
      count <- count + 1
    }
  }
  return(max_val)
}

# Function to create histograms
function1_histogram <- function(table, bin_number = 10, ..., main_title = NULL, x_label = NULL, y_label = NULL, x_limit = NULL, y_limit = NULL) {
  
  # Set default y-label 
  if (is.null(y_label)) {
    y_label <- "Frequency"
  }
  
  # Provide a color argument for flexibility
  vars <- as.character(substitute(list(...)))[-1]
  
  for (i in 1:length(vars)) {
    var <- vars[i]
    column <- table[[var]]
    
    # Check for variable existence
    if (is.null(column)) {
      print(paste("There is no variable like this:", var))
      next
    }
    
    # Calculate bins and cut points
    bins <- seq(minimum(column, na.rm = TRUE), maximum(column, na.rm = TRUE), length.out = bin_number + 1)
    column <- as.numeric(column)
    interval_factor <- cut(column, bins, include.lowest = TRUE, dig.lab = 2)
    divided <- table(interval_factor)
    
    
    # Calculate frequencies
    freq <- table(factor(interval_factor))
    freq <- as.numeric(freq)
    
    # Adjust plot limits based on arguments or data
    a <- minimum(bins)
    b <- maximum(bins)
    c <- length(bins) - 1
    if (is.null(x_limit)) {
      x_limit <- b
    }
    if (is.null(y_limit)) {
      y_limit <- max(freq)
    }
    
    # Set axis labels and title based on arguments
    xlabe <- ifelse(!is.null(x_label), x_label, var)
    
    # Create the plot
    plot(c(a, x_limit), c(0, y_limit), type = "n", xaxt = "n", xlab = "", ylab = "", main = paste("Histogram of", var))
    axis(1, at = seq(a, b, length.out = c + 1), labels = seq(a, b, length.out = c + 1), las = 2)
    title(main = paste("Histogram of", var), sub = NULL, xlab = xlabe, ylab = y_label, cex.lab = 1, line = NA, outer = FALSE)
    
    # Generate a color palette for each variable's bars
    colors <- rainbow(n = length(freq))  # Rainbow palette for each variable's bars
    
    # Draw bars with unique colors from the palette
    for (j in 1:c) {
      x <- bins[j]
      y <- freq[j]
      segments(x, 0, x, y, col = colors[j], lwd = 4)  
    }
  }
}


# Call function1_histogram with 8 different variables, displaying two histograms at a time

par(mfrow = c(2,4) )  # Set up two plots side by side

function1_histogram(table, bin_number = 10, Var1, main_title = "Histogram")
function1_histogram(table, bin_number = 10, Var2, main_title = "Histogram")
function1_histogram(table, bin_number = 10, Var3, main_title = "Histogram")
function1_histogram(table, bin_number = 10, Var4, main_title = "Histogram")
function1_histogram(table, bin_number = 10, Var5, main_title = "Histogram")
function1_histogram(table, bin_number = 10, Var6, main_title = "Histogram")
function1_histogram(table, bin_number = 10, Var7, main_title = "Histogram")
function1_histogram(table, bin_number = 10, Var8, main_title = "Histogram")

par(cex.axis = 0.7) # Adjust size of axis labels
