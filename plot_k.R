library(tidyverse)

source("../settings.R")

k_df <- read.csv("k_tma.csv")

line_width <- 1 / ggp2_magic_number

k_lims_y <- c(0, max(k_real_df$obs))

rmax <- 500

# Plot K
k_plot <- ggplot(k_df) +
  geom_line(aes(x=r, y=obs, col="obs"), linewidth=line_width) +
  geom_line(aes(x=r, y=theo, col="theo"), linewidth=line_width) +
  scale_y_continuous(expand = c(0, 0), limits=k_lims_y, breaks = c(0, 5*10^5, 10*10^5, 15*10^5), labels = c("0", expression(5~"*"~10^5), expression(10~"*"~10^5), expression(15~"*"~10^5))) +
  scale_x_continuous(expand = c(0, 0), limits=c(0, rmax)) +
  scale_colour_manual(values=c("#800066", "black")) +
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


k_plot

k_pdf_path <- file.path(output_folder, "k_b_cell1.pdf")

pdf_out(k_pdf_path, width=2.5, height=2)
print(k_plot)
dev.off()
