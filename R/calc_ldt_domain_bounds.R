#' Calculate LDT domain bounds from a shapefile
#'
#' This function reads a shapefile, applies a buffer around its bounding box,
#' snaps the domain to the given dx/dy grid resolution, and returns the
#' lower-left and upper-right cell-center coordinates for LDT domain setup.
#'
#' @param shp_path Path to shapefile.
#' @param dx Domain resolution in x direction.
#' @param dy Domain resolution in y direction. Default is same as dx.
#' @param buffer_coef Buffer coefficient applied to dx and dy. Default is 1.5.
#' @param decimals Number of decimals for rounded domain edges. Default is 4.
#' @param target_crs Target CRS for longitude/latitude calculation. Default is 4326.
#'
#' @return A named list with lower-left lat/lon, upper-right lat/lon, dx, dy, and domain edges.
#' @export
calc_ldt_domain_bounds <- function(
    shp_path,
    dx,
    dy = dx,
    buffer_coef = 1.5,
    decimals = 4,
    target_crs = 4326
) {
  
  shp <- sf::st_read(shp_path, quiet = TRUE)
  
  if (!is.na(sf::st_crs(shp))) {
    shp <- sf::st_transform(shp, target_crs)
  }
  
  bbox <- sf::st_bbox(shp)
  
  buffered_xmin <- bbox["xmin"] - buffer_coef * dx
  buffered_ymin <- bbox["ymin"] - buffer_coef * dy
  buffered_xmax <- bbox["xmax"] + buffer_coef * dx
  buffered_ymax <- bbox["ymax"] + buffer_coef * dy
  
  geo_xmin <- round(floor(buffered_xmin / dx) * dx, decimals)
  geo_ymin <- round(floor(buffered_ymin / dy) * dy, decimals)
  geo_xmax <- round(ceiling(buffered_xmax / dx) * dx, decimals)
  geo_ymax <- round(ceiling(buffered_ymax / dy) * dy, decimals)
  
  left_lon  <- round(geo_xmin + dx / 2, decimals + 1)
  left_lat  <- round(geo_ymin + dy / 2, decimals + 1)
  right_lon <- round(geo_xmax - dx / 2, decimals + 1)
  right_lat <- round(geo_ymax - dy / 2, decimals + 1)
  
  list(
    left_lat = left_lat,
    left_lon = left_lon,
    right_lat = right_lat,
    right_lon = right_lon,
    dx = dx,
    dy = dy,
    geo_xmin = geo_xmin,
    geo_ymin = geo_ymin,
    geo_xmax = geo_xmax,
    geo_ymax = geo_ymax,
    buffer_coef = buffer_coef
  )
}