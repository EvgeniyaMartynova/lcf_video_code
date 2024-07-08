library(ggplot2)
library(readr)
library(tools)

source("settings.R")

b_cell_color <- "#800066"

data_folder <- "data"
output_folder <- "img"
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

k_files <- c("k_tma.csv", "k_mc.csv")
line_width <- 1 / ggp2_magic_number
i <- 1

for (k_file in k_files) {
  k_file_path <- file.path(data_folder, k_file)

  k_df <- read_csv(k_file_path)

  k_lims_y <- c(0, max(k_df$iso))
  rmax <- max(k_df$r)

  if (i == 1) {
    y_breaks <- c(0, 5*10^5, 10*10^5, 15*10^5)
    y_labels <- c("0", expression(5~"*"~10^5), expression(10~"*"~10^5), expression(15~"*"~10^5))
  } else {
    y_breaks <- c(0, 20*10^5, 40*10^5, 60*10^5)
    y_labels <- c("0", expression(20~"*"~10^5), expression(40~"*"~10^5), expression(60~"*"~10^5))
  }

  # Plot K
  k_plot <- ggplot(k_df) +
    geom_line(aes(x=r, y=iso, col="obs"), linewidth=line_width) +
    geom_line(aes(x=r, y=theo, col="theo"), linewidth=line_width) +
    scale_y_continuous(expand = c(0.01, 0), limits=k_lims_y, breaks = y_breaks, labels = y_labels) +
    scale_x_continuous(expand = c(0, 0), limits=c(0, rmax)) +
    scale_colour_manual(values=c(b_cell_color, "gray")) +
    ylab("Ripley's K") +
    xlab(expression("r," ~ mu * m)) +
    theme_bw(base_size = default_pointsize, base_family = default_font) +
    theme(plot.margin = margin(5, 7.5, 0, 2),
          strip.background = element_blank(),
          strip.placement = "inside",
          axis.line = element_line(colour = "black", linewidth=line_width),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_text(size = default_pointsize, family = default_font, colour = "black"),
          axis.title = element_text(size = default_pointsize, family = default_font),
          legend.position="none")

  k_pdf_path <- file.path(output_folder, paste0(file_path_sans_ext(k_file), ".pdf"))

  pdf_out(k_pdf_path, width=2.5, height=2)
  print(k_plot)
  dev.off()

  i <- i + 1
}

