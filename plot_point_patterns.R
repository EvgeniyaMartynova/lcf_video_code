rm(list=ls())

library(dplyr)
library(sf)
library(sp)

source("utils.R")
source("settings.R")

data_folder <- "data"
output_folder <- "img"
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

random_color <- "#4EACD7"
clustered_color <- "#DB165C"
dispersed_color <- "#DBCB16"
b_cell_color <- "#800066"

wkt_file <- file.path(data_folder, "tma_bdry.wkt")
wkt_string <- readLines(wkt_file)

# Convert the WKT string to an sf object
boundary <- sf_polygons_from_wkt(wkt_file, rotate=FALSE)[[1]]
bbox <- st_bbox(boundary)

wh_ratio <- as.numeric((bbox$xmax - bbox$xmin) / (bbox$ymax - bbox$ymin))
width <- 20
height <- width / wh_ratio

real_coords <- read_csv(file.path(data_folder, "b_cell_real.csv")) %>% select(X, Y)
random_coords <- read_csv(file.path(data_folder, "b_cell_rand.csv")) %>% select(X, Y)
mc_coords <- read_csv(file.path(data_folder, "b_cell_mc.csv")) %>% select(X, Y)
md_coords <- read_csv(file.path(data_folder, "b_cell_md.csv")) %>% select(X, Y)

# Plot Data
pdf(file.path(output_folder, "Bcell_pp.pdf"), width = width, height = height)

plot(boundary, lwd = 5)
points(Y ~ X, data=real_coords, col=b_cell_color, pch=19, cex=3)
dev.off()

# Plot MD
pdf(file.path(output_folder, "MD_pp.pdf"), width = width, height = height)

plot(boundary, lwd = 5)
points(Y ~ X, data=md_coords, col=dispersed_color, pch=19, cex=3)
dev.off()

# Plot Random
pdf(file.path(output_folder, "Random_pp.pdf"), width = width, height = height)

plot(boundary, lwd = 5)
points(Y ~ X, data=random_coords, col=random_color, pch=19, cex=3)
dev.off()

# Plot MC real
pdf(file.path(output_folder, "MC_pp.pdf"), width = width, height = height)

plot(boundary, lwd = 5)
points(Y ~ X, data=mc_coords, col=clustered_color, pch=19, cex=3)
dev.off()

