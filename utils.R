# Loads sf polygons from WKT files
sf_polygons_from_wkt <- function(wkt_path, rotate=TRUE) {
  wkt_file <- file(wkt_path, open="r")
  polygon_strs <-readLines(wkt_file)
  close(wkt_file)

  polygons <- NULL
  for (polygon_str in polygon_strs) {
    sf_pol <- st_as_sfc(polygon_str)
    if (rotate) {
      sf_pol <- sf_pol * rotate(pi)
    }
    polygons <- c(polygons, sf_pol)
  }

  return(polygons)
}

