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
#' @param plot Logical. If TRUE, generates a diagnostic plot of the domain bounds.
#' @param verbose Logical. If TRUE, prints progress messages during execution.
#' 
#' @return A named list with lower-left lat/lon, upper-right lat/lon, dx, dy, and domain edges.
#' @export
calc_ldt_domain_bounds <- function(
    shp_path,
    dx,
    dy = dx,
    buffer_coef = 1.5,
    decimals = 4,
    target_crs = 4326,
    plot = TRUE ,
    verbose = TRUE
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
  
  # ----------------------------------------------------------------------
  # Diagnostic base plot
  # ----------------------------------------------------------------------
  if (isTRUE(plot)) {
    
    # Actual domain rectangle (LDT grid extent)
    domain_x <- c(geo_xmin, geo_xmax, geo_xmax, geo_xmin, geo_xmin)
    domain_y <- c(geo_ymin, geo_ymin, geo_ymax, geo_ymax, geo_ymin)
    
    # Buffered cell-center POINTs (bottom-left and top-right)
    pts_lon <- c(left_lon,  right_lon)
    pts_lat <- c(left_lat,  right_lat)
    
    # Plot limits with a small margin so the red dots/labels aren't clipped
    xr <- range(c(domain_x, pts_lon))
    yr <- range(c(domain_y, pts_lat))
    xpad <- diff(xr) * 0.05
    ypad <- diff(yr) * 0.05
    
    op <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(op), add = TRUE)
    
    # Base canvas
    graphics::plot(
      NA, NA,
      xlim = c(xr[1] - xpad, xr[2] + xpad),
      ylim = c(yr[1] - ypad, yr[2] + ypad),
      xlab = "Longitude", ylab = "Latitude",
      main = "LDT Domain Bounds",
      asp = 1
    )
    
    # Basin boundary
    plot(sf::st_geometry(shp), add = TRUE,
         border = "grey40", col = NA, lwd = 1.5)
    
    # Actual domain rectangle
    graphics::lines(domain_x, domain_y, col = "blue", lwd = 2, lty = 1)
    
    # Buffered POINTs as red dots
    graphics::points(pts_lon, pts_lat, col = "red", pch = 19, cex = 1.4)
    
    # Label the points
    graphics::text(left_lon,  left_lat,
                   labels = sprintf("BL (%.4f, %.4f)", left_lon, left_lat),
                   pos = 4, col = "red", cex = 0.8)
    graphics::text(right_lon, right_lat,
                   labels = sprintf("TR (%.4f, %.4f)", right_lon, right_lat),
                   pos = 2, col = "red", cex = 0.8)
    
    # Legend
    graphics::legend(
      "topleft",
      legend = c("Basin boundary", "Actual domain", "Buffered points (BL/TR)"),
      col    = c("grey40", "blue", "red"),
      lty    = c(1, 1, NA),
      pch    = c(NA, NA, 19),
      lwd    = c(1.5, 2, NA),
      bty    = "n",
      cex    = 0.8
    )
  }
  
  domain <- list(
    left_lat  = left_lat,
    left_lon  = left_lon,
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
  
  if (isTRUE(verbose)) {
    cat(
      sprintf("Run domain lower left lat:                                       %s\n", domain$left_lat),
      sprintf("Run domain lower left lon:                                       %s\n", domain$left_lon),
      sprintf("Run domain upper right lat:                                      %s\n", domain$right_lat),
      sprintf("Run domain upper right lon:                                      %s\n", domain$right_lon),
      sprintf("Run domain resolution (dx):                                      %s\n", domain$dx),
      sprintf("Run domain resolution (dy):                                      %s\n", domain$dy)
    )
    cat(
      sprintf("Run domain xmin:                                       %s\n", domain$geo_xmin),
      sprintf("Run domain ymin:                                       %s\n", domain$geo_ymin),
      sprintf("Run domain xmax:                                      %s\n", domain$geo_xmax),
      sprintf("Run domain ymax:                                      %s\n", domain$geo_ymax)
    )
  }
  
  return(domain)
}