library(gridExtra)
library(ggforce)
library(dplyr)
library(readr)
library(sf)
library(sp)

source("utils.R")
source("settings.R")

set.seed(207)

b_cell_color <- "#800066"

data_folder <- "data"
output_folder <- "img/bcells_k_animation"
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

k_df <- read_csv(file.path(data_folder, "k_tma.csv")) %>% dplyr::select(r, iso) %>% rename(k = iso)

wkt_file <- file.path(data_folder, "tma_bdry.wkt")
wkt_string <- readLines(wkt_file)

# Convert the WKT string to an sf object
boundary <- sf_polygons_from_wkt(wkt_file, rotate=FALSE)[[1]]

bb <- st_bbox(boundary)
wh_ratio <- as.numeric((bb$xmax - bb$xmin) / (bb$ymax - bb$ymin))

width <- 20
height <- width / wh_ratio

# Function to linearly interpolate between two sets of points

real_coords <- read_csv(file.path(data_folder, "b_cell_real.csv")) %>% dplyr::select(X, Y)

circle_coords <- real_coords %>% sample_frac(size=0.015)

k_lims_y <- c(0, max(k_df$k))

n_frames <- 100
max_radius <- 500

i <- 50

# Generate 100 frames
for (i in 1:n_frames) {
    radius <- (i / n_frames) * max_radius
    p1 <- ggplot() +
        geom_sf(data=boundary, fill="white", color="black", lwd=2) +
        geom_circle(data = circle_coords, aes(x0 = X, y0 = Y, r = radius), color = alpha("#808080", 0.9), linetype = "solid", linewidth = 2) +
        geom_point(data=real_coords, aes(x=X, y=Y), color=b_cell_color, size=6) +
        xlim(bb$xmin - max_radius, bb$xmax + max_radius) +
        ylim(bb$ymin - max_radius, bb$ymax + max_radius) +
        theme_void() +
        theme(plot.margin = margin(0, 0, 0, 0),
          panel.background = element_rect(fill = "white", color = "white"))

    p2 <- ggplot(filter(k_df, r <= radius), aes(x = r, y = k)) +
        geom_vline(xintercept = radius, linetype = "dashed", color = "grey", size=4) + # Convert radius to kilometers
        geom_line(color = b_cell_color, group = 1, size=4) +
        ylab("Ripley's K") +
        xlab(expression("r," ~ mu * m)) +
        scale_y_continuous(expand = c(0, 0), limits=k_lims_y, breaks = c(0, 5*10^5, 10*10^5, 15*10^5), labels = c("0", expression(5~"*"~10^5), expression(10~"*"~10^5), expression(15~"*"~10^5))) +
        scale_x_continuous(expand = c(0, 0), limits=c(0, max_radius)) +
        theme_bw(base_size = 70, base_family = default_font) +
        theme(plot.margin = margin(50, 100, 50, 2),
              axis.line = element_line(colour = "black"),
              axis.text = element_text(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              plot.background = element_rect(fill = "white", color = NA))


    combined_plot <- grid.arrange(p1, p2, ncol = 2,  widths = c(1, 1.3))

    frame_path <- file.path(output_folder, sprintf("frame_%03d.png", i))
    ggsave(frame_path, plot=combined_plot, width=width * 2.3, height=height, dpi=60)
}

