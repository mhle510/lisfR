#' Read a NetCDF File into a Spatially-Referenced Raster Stack
#'
#' @description 
#' Converts NetCDF files of many different formats (LIS, FLDAS, CHIRPS, MODIS, 
#' IMERG, SWOT, and more) into a properly georeferenced \code{terra::SpatRaster} 
#' stack. Handles extent calculation, missing value replacement, transposition, 
#' flipping, and CRS assignment based on \code{nc_type}.
#'
#' @param nc_file Character. Path to the NetCDF file.
#' @param var_names Character vector. Variable name(s) to extract from the NetCDF file.
#' @param nc_type Character. The type of NetCDF file. Supported values:
#' \describe{
#'   \item{"lis"}{LIS RUN model (Land Information System) outputs}
#'   \item{"lis_ensemble"}{LIS RUN with ensemble output}
#'   \item{"nldas"}{NLDAS related products}
#'   \item{"modis"}{MODIS LST}
#'   \item{"own"}{Custom NetCDF export files}
#'   \item{"imerg"}{IMERG global dataset}
#'   \item{"nldas_forcing"}{NLDAS forcing data}
#'   \item{"nldas3_forcing"}{NLDAS3 forcing data}
#'   \item{"EnKF"}{Ensemble Kalman Filter outputs}
#'   \item{"fldas"}{FLDAS land data (Central Asia)}
#'   \item{"fldas_abh", "fldas_abh_leadtimes"}{FLDAS data for sub-Saharan Africa}
#'   \item{"time_array"}{Time series array data}
#'   \item{"merra2"}{MERRA2 SLX Surface Flux Diagnostics}
#'   \item{"jra_3q"}{JRA3Q physical land 2d - fcst_phy2m}
#'   \item{"chirpsv2"}{CHIRPS V2 - global extent 50S-50N}
#'   \item{"chirpsv3"}{CHIRPS V3 - global extent 60S-60N}
#'   \item{"esacci"}{ESACCI}
#'   \item{"swot_2d"}{SWOT water level data (test with version C)}
#' }
#' @param missing_value Numeric. Value to treat as \code{NA} (default: \code{NaN}).
#' @param VALUE_SWLAT Numeric. Manual south-west corner latitude (required for "EnKF").
#' @param VALUE_SWLON Numeric. Manual south-west corner longitude (required for "EnKF").
#' @param VALUE_DX Numeric. Manual X resolution (required for "EnKF").
#' @param VALUE_DY Numeric. Manual Y resolution (required for "EnKF").
#'
#' @return A \code{terra::SpatRaster} stack with all requested variables/layers.
#' 
#' @export
nc_to_raster <- function(nc_file, var_names, nc_type, missing_value = NaN, 
                                   VALUE_SWLAT = NULL, VALUE_SWLON = NULL, 
                                   VALUE_DX = NULL, VALUE_DY = NULL) {
  
  # ========================================================================= #
  # STEP 1: OPEN NETCDF FILE
  # ========================================================================= #
  nc_data <- ncdf4::nc_open(nc_file)
  on.exit(ncdf4::nc_close(nc_data), add = TRUE)
  
  # ========================================================================= #
  # STEP 2: EXTENT CALCULATION BASED ON NC_TYPE
  # Add new extent calculation blocks here if a new dataset requires it
  # ========================================================================= #
  
  swot_2d_utm <- NULL  # placeholder, only populated for nc_type == "swot_2d"
  dates_num <- NULL    # placeholder, only populated for nc_type == "modis"
  
  if (nc_type == "esacci") {
    ext <- terra::ext(-180, 180, -90, 90)
  }  
  
  if (nc_type == "chirpsv2") {
    ext <- terra::ext(-180, 180, -50, 50)
  }  
  
  if (nc_type == "chirpsv3") {
    ext <- terra::ext(-180, 180, -60, 60)
  }  
  
  if (nc_type == "imerg") {
    ext <- terra::ext(-180, 180, -90, 90)
  }  
  
  if (nc_type == "merra2") {
    ext <- terra::ext(-180, 179.375, -90, 90)
  }    
  
  if (nc_type == "jra_3q") {
    ext <- terra::ext(0, 359.625, -89.71324, 89.71324)
  }
  
  ## correct lis and enkf extent (apr 3, 2026)
  if (nc_type %in% c("lis", "lis_ensemble", "nldas", "nldas_forcing", "nldas3_forcing")) {
    east_west_names <- c("east_west", "lon")
    north_south_names <- c("north_south", "lat")
    
    east_west <- get_nc_dim_length(nc_data, east_west_names)
    north_south <- get_nc_dim_length(nc_data, north_south_names)
    
    # These represent the CENTROID (center) of the south-west pixel
    SOUTH_WEST_CORNER_LAT <- ncdf4::ncatt_get(nc_data, 0, "SOUTH_WEST_CORNER_LAT")$value
    SOUTH_WEST_CORNER_LON <- ncdf4::ncatt_get(nc_data, 0, "SOUTH_WEST_CORNER_LON")$value
    DX <- ncdf4::ncatt_get(nc_data, 0, "DX")$value
    DY <- ncdf4::ncatt_get(nc_data, 0, "DY")$value
    
    # Generate sequences for the cell centers
    lat <- seq(SOUTH_WEST_CORNER_LAT, by = DY, length.out = north_south)
    lon <- seq(SOUTH_WEST_CORNER_LON, by = DX, length.out = east_west)
    
    # Calculate true extent (outer edges) by shifting half a pixel resolution
    xmin <- min(lon) - (DX / 2)
    xmax <- max(lon) + (DX / 2)
    ymin <- min(lat) - (DY / 2)
    ymax <- max(lat) + (DY / 2)
    
    ext <- terra::ext(c(xmin, xmax, ymin, ymax))
  }
  
  if (nc_type == "EnKF") {
    east_west_names <- c("east_west", "lon")
    north_south_names <- c("north_south", "lat")
    
    east_west <- get_nc_dim_length(nc_data, east_west_names)
    north_south <- get_nc_dim_length(nc_data, north_south_names)
    
    # These represent the CENTROID (center) of the south-west pixel
    SOUTH_WEST_CORNER_LAT <- VALUE_SWLAT
    SOUTH_WEST_CORNER_LON <- VALUE_SWLON
    DX <- VALUE_DX
    DY <- VALUE_DY
    
    xmin <- SOUTH_WEST_CORNER_LON - (DX / 2)
    ymin <- SOUTH_WEST_CORNER_LAT - (DY / 2)
    
    xmax <- (SOUTH_WEST_CORNER_LON + (east_west - 1) * DX) + (DX / 2)
    ymax <- (SOUTH_WEST_CORNER_LAT + (north_south - 1) * DY) + (DY / 2)
    
    # Define extent using the outer boundaries
    ext <- terra::ext(c(xmin, xmax, ymin, ymax))
  }
  
  if (nc_type %in% c("own", "time_array")) {
    MIN_LAT <- ncdf4::ncatt_get(nc_data, 0, "MIN_LAT")$value
    MIN_LON <- ncdf4::ncatt_get(nc_data, 0, "MIN_LON")$value
    MAX_LAT <- ncdf4::ncatt_get(nc_data, 0, "MAX_LAT")$value
    MAX_LON <- ncdf4::ncatt_get(nc_data, 0, "MAX_LON")$value
    ext <- terra::ext(c(MIN_LON, MAX_LON, MIN_LAT, MAX_LAT))
  }
  
  if (nc_type == "fldas") {
    min_lat <- ncdf4::ncatt_get(nc_data, 0, "SouthernmostLatitude")$value
    min_lon <- ncdf4::ncatt_get(nc_data, 0, "WesternmostLongitude")$value
    max_lat <- ncdf4::ncatt_get(nc_data, 0, "NorthernmostLatitude")$value
    max_lon <- ncdf4::ncatt_get(nc_data, 0, "EasternmostLongitude")$value
    ext_interim <- c(min_lon, max_lon, min_lat, max_lat)
    ext_interim <- as.numeric(gsub("f", "", ext_interim))
    ext <- terra::ext(ext_interim)
  }
  
  if (nc_type == "fldas_abh" || nc_type == "fldas_abh_leadtimes") {
    ext <- terra::ext(-20, 60, -40, 40)
  }
  
  if (nc_type == "modis") {
    lon <- ncdf4::ncvar_get(nc_data, "lon")  
    lat <- ncdf4::ncvar_get(nc_data, "lat")  
    xmin <- min(lon); xmax <- max(lon)
    ymin <- min(lat); ymax <- max(lat)
    ext <- terra::ext(c(xmin, xmax, ymin, ymax))
    
    dates_str <- get_nc_timestamps(nc_file, type = "modis")
    dates_num <- format(dates_str, "%Y%m%d")
  }
  
  if (nc_type == "swot_2d") {
    
    # -- Geographic extent (WGS84, degrees) --
    min_lon <- ncdf4::ncatt_get(nc_data, 0, "geospatial_lon_min")$value
    max_lon <- ncdf4::ncatt_get(nc_data, 0, "geospatial_lon_max")$value
    min_lat <- ncdf4::ncatt_get(nc_data, 0, "geospatial_lat_min")$value
    max_lat <- ncdf4::ncatt_get(nc_data, 0, "geospatial_lat_max")$value
    ext <- terra::ext(as.numeric(c(min_lon, max_lon, min_lat, max_lat)))
    
    # -- UTM extent (metres, native 100m grid) --
    utm_zone_num  <- ncdf4::ncatt_get(nc_data, 0, "utm_zone_num")$value
    mgrs_lat_band <- ncdf4::ncatt_get(nc_data, 0, "mgrs_latitude_band")$value
    x_min         <- ncdf4::ncatt_get(nc_data, 0, "x_min")$value
    x_max         <- ncdf4::ncatt_get(nc_data, 0, "x_max")$value
    y_min         <- ncdf4::ncatt_get(nc_data, 0, "y_min")$value
    y_max         <- ncdf4::ncatt_get(nc_data, 0, "y_max")$value
    ext_utm       <- terra::ext(as.numeric(c(x_min, x_max, y_min, y_max)))
    
    # -- Build UTM CRS string --
    utm_hemisphere <- ifelse(mgrs_lat_band >= "N", "north", "south")
    crs_utm <- paste0("+proj=utm +zone=", utm_zone_num,
                      " +", utm_hemisphere,
                      " +datum=WGS84 +units=m +no_defs")
    
    # -- Store UTM info as a list (used later in Step 3) --
    swot_2d_utm <- list(
      utm_zone_num  = utm_zone_num,
      mgrs_lat_band = mgrs_lat_band,
      hemisphere    = utm_hemisphere,
      crs_utm       = crs_utm,
      ext_utm       = ext_utm,
      x_min = x_min, x_max = x_max,
      y_min = y_min, y_max = y_max
    )
    
    cat("=== SWOT 2D Extent Info ===\n")
    cat("UTM Zone  :", utm_zone_num, mgrs_lat_band, "(", utm_hemisphere, ")\n")
    cat("UTM Extent:", x_min, "->", x_max, "(X) |", y_min, "->", y_max, "(Y) m\n")
    cat("WGS Extent:", min_lon, "->", max_lon, "(lon) |",
        min_lat, "->", max_lat, "(lat)\n")
  }
  
  # ========================================================================= #
  # STEP 3: EXTRACT AND PROCESS VARIABLES
  # Loop through each requested variable and dispatch to .process_nc_matrix()
  # based on nc_type. Add new nc_type handling here AND in .process_nc_matrix().
  # ========================================================================= #
  raster_list <- list()
  
  for (var_name in var_names) {
    var_dims <- nc_data$var[[var_name]]$ndims
    var_data <- ncdf4::ncvar_get(nc_data, var_name)
    
    # -- ESACCI --
    if (nc_type == "esacci") {
      r <- .process_nc_matrix(var_data, var_name, nc_type, ext, missing_value)
      raster_list <- c(raster_list, r)
    }
    
    # -- CHIRPS V2 / V3 --
    if (nc_type %in% c("chirpsv2", "chirpsv3")) {
      num_layers <- dim(var_data)[3]
      for (i in 1:num_layers) {
        r <- .process_nc_matrix(var_data[,,i], var_name, nc_type, ext, missing_value, i = i)
        raster_list <- c(raster_list, r)
      }
      time <- get_nc_timestamps(nc_file, type = "chirps")
      names(raster_list) <- paste0("d", format(time, format = "%Y%m%d"))
    }
    
    # -- LIS, EnKF --
    if (nc_type %in% c("lis", "EnKF")) {
      if (var_dims == 3) {
        num_layers <- dim(var_data)[3]
        for (i in 1:num_layers) {
          r <- .process_nc_matrix(var_data[,,i], var_name, nc_type, ext, missing_value, i = i)
          raster_list <- c(raster_list, r)
        }
      } else {
        r <- .process_nc_matrix(var_data, var_name, nc_type, ext, missing_value)
        raster_list <- c(raster_list, r)  
      }
    } 
    
    # -- LIS ENSEMBLE --
    if (nc_type == "lis_ensemble") {
      if (var_dims == 3) {
        num_ensemble <- dim(var_data)[3]
        for (j in 1:num_ensemble) {
          r <- .process_nc_matrix(var_data[, , j], var_name, nc_type, ext, missing_value, j = j)
          raster_list <- c(raster_list, r)
        }
      }
      if (var_dims == 4) {
        num_ensemble <- dim(var_data)[3]
        num_layers <- dim(var_data)[4]
        for (j in 1:num_ensemble) {
          for (i in 1:num_layers) {
            r <- .process_nc_matrix(var_data[, , j, i], var_name, nc_type, ext, missing_value, i = i, j = j)
            raster_list <- c(raster_list, r)
          }
        }
      }
    }
    
    # -- JRA-3Q --
    if (nc_type == "jra_3q") {
      if (var_dims == 3) {
        num_layers <- dim(var_data)[3]
        for (i in 1:num_layers) {
          r <- .process_nc_matrix(var_data[,,i], var_name, nc_type, ext, missing_value, i = i)
          raster_list <- c(raster_list, r)
        }
      } else {
        r <- .process_nc_matrix(var_data, var_name, nc_type, ext, missing_value)
        raster_list <- c(raster_list, r)  
      }
    } 
    
    # -- NLDAS, OWN --
    if (nc_type %in% c("nldas", "own")) {
      r <- .process_nc_matrix(var_data, var_name, nc_type, ext, missing_value)
      raster_list <- c(raster_list, r)
    }
    
    # -- IMERG --
    if (nc_type == "imerg") {
      r <- .process_nc_matrix(var_data, var_name, nc_type, ext, missing_value)
      raster_list <- c(raster_list, r)
    } 
    
    # -- NLDAS FORCING --
    if (nc_type == "nldas_forcing") {
      r <- .process_nc_matrix(var_data, var_name, nc_type, ext, missing_value)
      raster_list <- c(raster_list, r)
    }
    
    # -- NLDAS3 FORCING --
    if (nc_type == "nldas3_forcing") {
      num_layers <- dim(var_data)[3]
      for (i in 1:num_layers) {
        r <- .process_nc_matrix(var_data[,,i], var_name, nc_type, ext, missing_value, i = i)
        raster_list <- c(raster_list, r)
      }
    }
    
    # -- FLDAS --
    if (nc_type == "fldas") {
      r <- .process_nc_matrix(var_data, var_name, nc_type, ext, missing_value)
      raster_list <- c(raster_list, r)
    }
    
    # -- FLDAS ABH --
    if (nc_type == "fldas_abh") {
      num_layers <- dim(var_data)[3]
      dates <- get_nc_timestamps(nc_file, type = "fldas_abh")
      
      is_matching <- (length(dates) == num_layers)
      message("Timestamps match 4th dimension: ", is_matching)
      
      for (i in 1:num_layers) {
        r <- .process_nc_matrix(var_data[,,i], var_name, nc_type, ext, missing_value, i = dates[i])
        raster_list <- c(raster_list, r)
      }
    }
    
    # -- FLDAS ABH Lead time --
    if (nc_type == "fldas_abh_leadtimes") {
      num_leads <- dim(var_data)[3]      # e.g., 6 lead times
      num_timestamps <- dim(var_data)[4] # e.g., 228 months
      
      dates <- get_nc_timestamps(nc_file, type = "fldas_abh")
      
      is_matching <- (length(dates) == num_timestamps)
      message("Timestamps match 4th dimension: ", is_matching)
      
      # Stop the function if they don't match, to prevent naming errors
      if (!is_matching) {
        stop("Mismatch: Extracted dates length (", length(dates), 
             ") does not equal 4th dimension length (", num_timestamps, ").")
      }
      
      # Loop through lead times, then timestamps
      for (l in 1:num_leads) {
        for (t in 1:num_timestamps) {
          
          # Extract 2D slice for specific lead time and timestamp
          slice <- var_data[, , l, t]
          
          # Create clear name with actual date (e.g., "SubRZSM_lead1_200101")
          layer_name <- paste0(var_name, "_lead", l, "_", dates[t])
          
          # Process the layer using the consolidated dispatcher.
          # Note: layer_name is passed as `i` since fldas_abh_leadtimes
          # uses `i` directly as the full pre-built name (see .process_nc_matrix)
          r <- .process_nc_matrix(slice, var_name, nc_type, ext, missing_value, i = layer_name)
          
          raster_list <- c(raster_list, r)
        }
      } 
    } 
    
    # -- TIME ARRAY / MERRA2 --
    if (nc_type %in% c("time_array", "merra2")) {
      num_layers <- dim(var_data)[3]
      for (i in 1:num_layers) {
        r <- .process_nc_matrix(var_data[,,i], var_name, nc_type, ext, missing_value, i = i)
        raster_list <- c(raster_list, r)
      }
    }
    
    # -- MODIS --
    if (nc_type == "modis") {
      num_layers <- dim(var_data)[3]
      for (i in 1:num_layers) {
        r <- .process_nc_matrix(var_data[,,i], var_name, nc_type, ext, missing_value, 
                                i = i, dates_num = dates_num)
        raster_list <- c(raster_list, r)
      }
    }
    
    # -- SWOT 2D --
    if (nc_type == "swot_2d") {
      cat("Processing SWOT 2D variable:", var_name,
          "| dims:", var_dims, "| size:", paste(dim(var_data), collapse = "x"), "\n")
      
      # -- Single 2D layer [x, y] --
      r <- .process_nc_matrix(
        layer_data    = var_data,
        var_name      = var_name,
        nc_type       = nc_type,
        ext           = ext,
        missing_value = missing_value,
        ext_utm       = swot_2d_utm$ext_utm,
        crs_utm       = swot_2d_utm$crs_utm,
        reproject_wgs = TRUE   # set FALSE to keep native UTM
      )
      raster_list <- c(raster_list, r)
    }
    
  }
  
  # ========================================================================= #
  # STEP 4: COMBINE, ASSIGN CRS AND RETURN
  # ========================================================================= #
  raster_stack <- terra::rast(raster_list)
  terra::crs(raster_stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  return(raster_stack)
}