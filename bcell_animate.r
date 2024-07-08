rm(list=ls())

library(dplyr)
library(sf)
library(sp)

source("utils.R")
source("settings.R")

# Function to linearly interpolate between two sets of points
lerp <- function(start, end, t) {
  start + t * (end - start)
}

reorder_coords <- function(original_coords, generated_coords) {
  ordered_coords <- matrix(nrow=nrow(generated_coords), ncol=ncol(generated_coords))
  remaining_coords <- as.matrix(generated_coords)

  for (i in 1:nrow(original_coords)) {
    distances <- spDistsN1(remaining_coords, as.numeric(original_coords[i, ]), longlat = FALSE)
    nearest_index <- which.min(distances)
    ordered_coords[i, ] <- remaining_coords[nearest_index, ]
    remaining_coords <- remaining_coords[-nearest_index, , drop = FALSE]
  }

  ordered_coords <- as.data.frame(ordered_coords) %>% setNames(c("X", "Y"))
  return(ordered_coords)
}

# Easing function for ease-in and ease-out
ease_in_out <- function(t) {
  if (t < 0.5) {
    4 * t^3
  } else {
    (t - 1) * (2 * t - 2) * (2 * t - 2) + 1
  }
}

# Function to interpolate between two sets of points with easing
ease_lerp <- function(start, end, t) {
  eased_t <- ease_in_out(t)
  start + eased_t * (end - start)
}

data_folder <- "data"
output_folder <- "img"
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

# load boundary file
# load 3 data files
# make separate plots
# make animation

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
random_coords <- reorder_coords(real_coords, random_coords)

md_coords <- read_csv(file.path(data_folder, "b_cell_md.csv")) %>% select(X, Y)
md_coords <- reorder_coords(random_coords, md_coords)

mc_coords <- read_csv(file.path(data_folder, "b_cell_mc.csv")) %>% select(X, Y)
mc_coords <- reorder_coords(real_coords, mc_coords)

# Animate real to random
anim_folder <- file.path(output_folder, "real_to_random")
dir.create(anim_folder, showWarnings = FALSE, recursive = TRUE)

for (i in 1:50) {
  t <- (i - 1) / 49
  frame_path <- file.path(anim_folder, sprintf("frame_%03d.png", i))
  png(frame_path, width = width, height = height, units = "in", res = 60)

  plot(boundary, lwd = 5)
  coords <- ease_lerp(real_coords, random_coords, t)
  points(Y ~ X, data=coords, col="#800066", pch=19, cex=3)
  dev.off()
}

# Animate random to md
anim_folder <- file.path(output_folder, "random_to_md")
dir.create(anim_folder, showWarnings = FALSE, recursive = TRUE)

for (i in 1:50) {
  t <- (i - 1) / 49
  frame_path <- file.path(anim_folder, sprintf("frame_%03d.png", i))
  png(frame_path, width = width, height = height, units = "in", res = 60)

  plot(boundary, lwd = 5)
  coords <- ease_lerp(random_coords, md_coords, t)
  points(Y ~ X, data=coords, col="#800066", pch=19, cex=3)
  dev.off()
}

# Animate real to mc
anim_folder <- file.path(output_folder, "real_to_mc")
dir.create(anim_folder, showWarnings = FALSE, recursive = TRUE)

for (i in 1:50) {
  t <- (i - 1) / 49
  frame_path <- file.path(anim_folder, sprintf("frame_%03d.png", i))
  png(frame_path, width = width, height = height, units = "in", res = 60)

  plot(boundary, lwd = 5)
  coords <- ease_lerp(real_coords, mc_coords, t)
  points(Y ~ X, data=coords, col="#800066", pch=19, cex=3)
  dev.off()
}
