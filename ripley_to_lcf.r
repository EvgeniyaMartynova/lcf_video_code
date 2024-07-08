# Load necessary libraries
library(dplyr)
library(readr)

source("settings.R")

b_cell_color <- "#800066"

# Load data
data_folder <- "data"
output_folder <- "img/ripley_to_lcf"
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

lcf_df <- read_csv(file.path(data_folder, "lcf_stat_b_cell.csv")) %>% select(r, iso) %>% rename(lcf = iso)
k_df <- read_csv(file.path(data_folder, "k_tma.csv")) %>% select(r, iso) %>% rename(k = iso)
df <- inner_join(k_df, lcf_df)


# Define parameters
n_frames <- 80

# Generate frames
for (i in 1:n_frames) {
  l <- "Ripley's K"
  if (i <= 20) {
    fade_out <- 1 - (i / 20)
    x_label <- rgb(0, 0, 0,  fade_out)
    y_label <- rgb(0, 0, 0,  fade_out)
  } else if (i <= 60) {
    x_label <- rgb(0, 0, 0, 0)
    y_label <- rgb(0, 0, 0, 0)
  } else {
    l <- "Local Correlation Function"
    fade_in <- (i - 60) / 20
    x_label <- rgb(0, 0, 0, fade_in)
    y_label <- rgb(0, 0, 0, fade_in)
  }

  t <- (i - 1)/(n_frames-1)
  scaling <- 1e-5
  if (i <= 40) {
      scaling <- 10 ^ ((i-1)/39 * -5)
  }

  if (i == 1) {
    y_breaks <- c(0, 5*10^5, 10*10^5, 15*10^5)
    y_labels <- c("0", expression(5~"*"~10^5), expression(10~"*"~10^5), expression(15~"*"~10^5))
  } else {
    y_breaks <- waiver()
    y_labels <- waiver()
  }

  df <- df %>% mutate(val = scaling * k * (1 - t) + lcf * t)
  y_max <- max(max(df$val), 1)

  p <- ggplot(df, aes(x = r, y = val)) +
    geom_hline(yintercept=0, color="grey") +
    geom_line(color = b_cell_color, group = 1, linewidth=1) +
    ylab(l) +
    xlab(expression("r," ~ mu * m)) +
    scale_y_continuous(expand = c(0, 0), limits=c(-1, y_max), breaks = y_breaks, labels = y_labels) +
    scale_x_continuous(expand = c(0, 0), limits=c(0, 500)) +
    theme_bw(base_size = 25, base_family = default_font) +
    theme(plot.margin = margin(10, 20, 10, 2),
          axis.line = element_line(colour = "black"),
          axis.text = element_text(colour = "black", size = 20),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          axis.title.y = element_text(color = y_label))


  # Save frame
  frame_path <- file.path(output_folder, sprintf("frame_%03d.png", i))
  ggsave(frame_path, plot = p, width = 6, height = 5, dpi = 200)
}

