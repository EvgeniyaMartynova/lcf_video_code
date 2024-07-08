rm(list=ls())

# Load necessary libraries
library(ggplot2)
library(MASS)
library(latex2exp)

source("utils.R")

set.seed(807)

b_cell_color <- "#800066"

# Function to generate correlated data with fixed x values
generate_correlated_data <- function(x, correlation) {
  y <- correlation * x + sqrt(1 - correlation^2) * rnorm(length(x))
  data.frame(x = x, y = y)
}

# Function to interpolate between two sets of data frames with easing
interpolate_frames <- function(start_data, end_data, n_frames) {
  interpolated_data <- vector("list", n_frames)
  for (i in 1:n_frames) {
    t <- (i - 1) / (n_frames - 1)
    interpolated_data[[i]] <- data.frame(
      x = start_data$x,
      y = ease_lerp(start_data$y, end_data$y, t)
    )
  }
  return(interpolated_data)
}

# Function to plot data with correlation coefficient and linear fit line
plot_data <- function(data, correlation, x_limits, y_limits) {
  p <- ggplot(data, aes(x=x, y=y)) +
    geom_point(color = b_cell_color, size=3) +
    geom_smooth(method = "lm", color = "grey", linetype = "dashed", se = FALSE) +
    ggtitle(latex2exp::TeX(paste0("Pearson Correlation: \\textbf{", round(correlation, 2), "}"))) +
    theme_minimal(base_size = 15) +
    theme(panel.background = element_rect(fill = "white", colour = "white"),
          plot.background = element_rect(fill = "white", colour = "white"),
          axis.line = element_line(colour = "black"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(size=28)) +
    xlim(x_limits) +
    ylim(y_limits)

  p
}

# Define parameters
n_points <- 50
n_frames <- 50
start_correlation <- 0.9  # High correlation
mid_correlation <- 0      # Random
end_correlation <- -0.9   # Anti-correlation
output_folder <- "img/correlation_frames"
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

# Generate x values
x_values <- rnorm(n_points)

# Generate data frames for each state
data_correlated <- generate_correlated_data(x_values, start_correlation)
data_random <- generate_correlated_data(x_values, mid_correlation)
data_anticorrelated <- generate_correlated_data(x_values, end_correlation)

# Calculate x and y limits
all_y_values <- c(data_correlated$y, data_random$y, data_anticorrelated$y)
x_limits <- range(x_values)
y_limits <- range(all_y_values)

# Generate interpolated data frames for each transition
data_frames1 <- interpolate_frames(data_correlated, data_random, n_frames)
data_frames2 <- interpolate_frames(data_random, data_anticorrelated, n_frames)
data_frames3 <- interpolate_frames(data_anticorrelated, data_correlated, n_frames)

# Combine all data frames
all_data_frames <- c(data_frames1, data_frames2, data_frames3)

# Generate frames
total_frames <- length(all_data_frames)
for (i in 1:total_frames) {
  data <- all_data_frames[[i]]
  correlation <- cor(data$x, data$y)
  p <- plot_data(data, correlation, x_limits, y_limits)
  frame_path <- file.path(output_folder, sprintf("frame_%03d.png", i))
  ggsave(frame_path, plot = p, width = 5, height = 5, dpi = 100)
}

