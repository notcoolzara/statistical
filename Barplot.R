# Read the text file
table_data <- read.table("DatasetNA.txt", header = TRUE, sep = " ")

# Minimum Function
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

#Maximum Function
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

#Median
median <- function(column) {
  ordered_vector <- column[!is.na(column)]
  count <- length(ordered_vector)
  unordered <- TRUE
  while (unordered) {
    unordered <- FALSE
    for (i in 1:(count - 1)) {
      if (ordered_vector[i] > ordered_vector[i + 1]) {
        temp <- ordered_vector[i]
        ordered_vector[i] <- ordered_vector[i + 1]
        ordered_vector[i + 1] <- temp
        unordered <- TRUE
      }
    }
  }
  if (count %% 2 == 0) {
    median_val <- (ordered_vector[count / 2] + ordered_vector[(count / 2) + 1]) / 2
  } else {
    median_val <- ordered_vector[(count + 1) / 2]
  }
  
  return(median_val)
}

#Barplot Function
custom_barplot <- function(table, category1, category2, main_title1 = NULL, main_title2 = NULL, x_label = NULL, y_label = NULL, y_limit = NULL, color1 = "lightgreen", color2 = "lightblue") {
  
  par(mfrow=c(1,2))  #Split screen
  
  if (is.null(x_label))
    x_label <- "Category"
  if (is.null(y_label))
    y_label <- "Frequency"
  
  values1 <- table[[category1]]
  values2 <- table[[category2]]
  
  gender_barplot(values1, barcolor = color1, xad = x_label, yad = y_label, main_title = main_title1, ylmi = y_limit)
  group_barplot(values2, barcolor = color2, xad = x_label, yad = y_label, main_title = main_title2, ylmi = y_limit)
}

#Gender barplot function
gender_barplot <- function(x, barcolor = "lightgreen", xad = "Gender", yad = "Frequency", main_title = "GENDER BARPLOT", ylmi = NULL, xlmi = NULL) {
  counts <- table(x)
  bar_width <- 1.7  #Width of bars
  
  if (is.null(ylmi))
    ylmi <- max(counts) * 1.1
  if (is.null(xlmi))
    xlmi <- length(counts) * 2.5
  
  #Empty plot
  plot(1, type = "n", ylim = c(0, ylmi), xlim = c(0, xlmi), xaxt = "n", xlab = "", ylab = yad, main = main_title)
  
  x_positions <- seq(1, length(counts) * 2, by = 2)
  
  #Drawing the bars using lines and polygon
  for (i in seq_along(counts)) {
    lines(x = c(x_positions[i] - bar_width / 2, x_positions[i] - bar_width / 2),
          y = c(0, counts[i]),
          col = "black",
          lwd = bar_width)
    lines(x = c(x_positions[i] - bar_width / 2, x_positions[i] + bar_width / 2),
          y = c(counts[i], counts[i]),
          col = "black",
          lwd = bar_width)
    lines(x = c(x_positions[i] + bar_width / 2, x_positions[i] + bar_width / 2),
          y = c(counts[i], 0),
          col = "black",
          lwd = bar_width)
    polygon(c(x_positions[i] - bar_width / 2, x_positions[i] -     bar_width / 2, x_positions[i] + bar_width / 2, x_positions[i] + bar_width / 2),
            c(0, counts[i], counts[i], 0),
            col = barcolor,
            border = "black")
  }
  #X-axis Labels
  axis(1, at = x_positions, labels = names(counts), las = 2)
  
  #Random Y-axis
  axis(2, at = seq(0, ylmi, by = 10))
}

#Group barplot function
group_barplot <- function(x, barcolor = "lightblue", xad = "Group", yad = "Frequency", main_title = "GROUP BARPLOT", ylmi = NULL, xlmi = NULL) {
  counts <- table(x)
  bar_width <- 1.5  #width of bars
  
  if (is.null(ylmi))
    ylmi <- max(counts) * 1.1 
  if (is.null(xlmi))
    xlmi <- length(counts) * 3.5
  
  #Empty plot
  plot(1, type = "n", ylim = c(0, ylmi), xlim = c(0, xlmi), xaxt = "n", xlab = "", ylab = yad, main = main_title)
  
  x_positions <- seq(1, length(counts) * 2, by = 2)
  
  #Drawing bars using lines and polygon
  for (i in seq_along(counts)) {
    lines(x = c(x_positions[i] - bar_width / 2, x_positions[i] - bar_width / 2),
          y = c(0, counts[i]),
          col = "black",
          lwd = bar_width)
    lines(x = c(x_positions[i] - bar_width / 2, x_positions[i] + bar_width / 2),
          y = c(counts[i], counts[i]),
          col = "black",
          lwd = bar_width)
    lines(x = c(x_positions[i] + bar_width / 2, x_positions[i] + bar_width / 2),
          y = c(counts[i], 0),
          col = "black",
          lwd = bar_width)
    polygon(c(x_positions[i] - bar_width / 2, x_positions[i] - bar_width / 2, x_positions[i] + bar_width / 2, x_positions[i] + bar_width / 2),
            c(0, counts[i], counts[i], 0),
            col = barcolor,
            border = "black")
  }
  
  #X-axic labels
  axis(1, at = x_positions, labels = names(counts), las = 2)
  
  #Random Y-axis labels
  axis(2, at = seq(0, ylmi, by = 10))
}

#Call custom barplot function 
custom_barplot(table_data, "Gender", "Group", main_title1 = "GENDER", main_title2 = "GROUP", color1 = "lightgreen", color2 = "lightblue")
