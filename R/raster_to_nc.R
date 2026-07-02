#' Write a SpatRaster Stack to a NetCDF File
#'
#' @description 
#' Exports a \code{terra::SpatRaster} stack into a formatted NetCDF file. It automatically 
#' extracts the bounding box, calculates pixel centers, and assigns global attributes.
#'
#' @param output_file Character. Path/filename for the output NetCDF file.
#' @param raster_stack A \code{terra::SpatRaster} object containing the data layers.
#' @param var_names Character vector. Variable names corresponding to raster layers.
#' @param units_netcdf Character vector. Units for each variable.
#' @param title_name Character. Global title attribute for the NetCDF file.
#' @param date_strings Character. Time attribute string.
#' @param missing_value Numeric. Value to assign to NAs (default: \code{NaN}).
#' @param x_res Numeric. Exact X resolution/step size. If NULL, auto-calculates.
#' @param y_res Numeric. Exact Y resolution/step size. If NULL, auto-calculates.
#'
#' @return Character. The path to the created NetCDF file.
#' 
#' @export
raster_to_nc <- function(output_file, raster_stack, var_names, units_netcdf, 
                            title_name, date_strings, missing_value = NaN, 
                            x_res = NULL, y_res = NULL) {
  
  # ========================================================================= #
  # STEP 1: EXTRACT METADATA AND CALCULATE PIXEL CENTERS
  # ========================================================================= #
  ext_orig <- terra::ext(raster_stack[[1]])
  nlat <- nrow(raster_stack[[1]])
  nlon <- ncol(raster_stack[[1]])
  
  # Determine resolution:
  res_x <- if (!is.null(x_res)) x_res else round(terra::res(raster_stack[[1]])[1], 2)
  res_y <- if (!is.null(y_res)) y_res else round(terra::res(raster_stack[[1]])[2], 2)
  
  # Calculate pixel centers (NetCDF standard)
  start_lon_center <- ext_orig[1] + (res_x / 2)  
  start_lat_center <- ext_orig[4] - (res_y / 2)  
  
  lon <- round(seq(from = start_lon_center, by = res_x, length.out = nlon), 4)
  lat <- round(seq(from = start_lat_center, by = -res_y, length.out = nlat), 4)
  
  # ========================================================================= #
  # STEP 2: DEFINE NETCDF DIMENSIONS AND VARIABLES
  # ========================================================================= #
  lon_dim <- ncdf4::ncdim_def("lon", "degrees_east", lon)
  lat_dim <- ncdf4::ncdim_def("lat", "degrees_north", lat)
  
  var_defs <- lapply(seq_along(var_names), function(i) {
    ncdf4::ncvar_def(
      name = var_names[i],
      units = units_netcdf[i],
      dim = list(lon_dim, lat_dim), 
      compression = 5 
    )
  })
  
  # ========================================================================= #
  # STEP 3: CREATE NETCDF FILE AND WRITE DATA
  # ========================================================================= #
  nc_out <- ncdf4::nc_create(output_file, var_defs)
  
  # Ensure file closes safely even if a write error occurs
  on.exit(ncdf4::nc_close(nc_out), add = TRUE)
  
  for (i in seq_along(var_names)) {
    raster_data <- raster_stack[[i]]
    raster_matrix <- as.matrix(raster_data, wide = TRUE)
    raster_matrix[is.na(raster_matrix)] <- missing_value
    ncdf4::ncvar_put(nc_out, var_defs[[i]], t(raster_matrix))
  }
  
  # ========================================================================= #
  # STEP 4: ADD GLOBAL ATTRIBUTES
  # ========================================================================= #
  min_lon <- round(ext_orig[1], 3) 
  max_lon <- round(ext_orig[2], 3) 
  min_lat <- round(ext_orig[3], 3) 
  max_lat <- round(ext_orig[4], 3) 
  
  ncdf4::ncatt_put(nc_out, 0, "title", title_name)
  ncdf4::ncatt_put(nc_out, 0, "time",  as.character(date_strings))
  ncdf4::ncatt_put(nc_out, 0, "MIN_LON", min_lon)
  ncdf4::ncatt_put(nc_out, 0, "MAX_LON", max_lon)
  ncdf4::ncatt_put(nc_out, 0, "MIN_LAT", min_lat)
  ncdf4::ncatt_put(nc_out, 0, "MAX_LAT", max_lat)
  ncdf4::ncatt_put(nc_out, 0, "DX", res_x) 
  ncdf4::ncatt_put(nc_out, 0, "DY", res_y) 
  
  return(output_file)
}