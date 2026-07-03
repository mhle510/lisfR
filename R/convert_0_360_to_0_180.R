#' (Internal) Convert Raster Longitude from 0-360 to -180/180
#'
#' @description 
#' Splits a SpatRaster with longitude range (0 to 360) into two parts, shifts the 
#' (180 to 360) portion to (-180 to 0), and merges them back into a single raster 
#' with standard longitude range (-180 to 180).
#'
#' @param raster A \code{terra::SpatRaster} object with longitude extent within (0, 360).
#'
#' @return A \code{terra::SpatRaster} object with longitude extent within (-180, 180).
#' @keywords internal
.convert_0_360_to_0_180 <- function(raster) {
  
  # Ensure the input is a SpatRaster
  if (!inherits(raster, "SpatRaster")) {
    stop("Input must be a SpatRaster object")
  }
  
  # Retrieve raster extent
  raster_extent <- terra::ext(raster)
  
  # Validate raster extent
  if (terra::xmin(raster_extent) < 0 || terra::xmax(raster_extent) > 360) {
    stop("Raster extent must be within 0 to 360 longitude range")
  }
  
  # Split the raster: keep [0, 180] and [180, 360]
  r1 <- terra::crop(raster, terra::ext(0, 180, terra::ymin(raster_extent), terra::ymax(raster_extent)))
  r2 <- terra::crop(raster, terra::ext(180, 360, terra::ymin(raster_extent), terra::ymax(raster_extent)))
  
  # Shift the [180, 360] part to [-180, 0]
  r2_fixed <- terra::shift(r2, dx = -360)
  
  # Merge the unmodified [0, 180] portion and the shifted [-180, 0] portion
  raster_fixed <- terra::merge(r1, r2_fixed)
  
  return(raster_fixed)
}