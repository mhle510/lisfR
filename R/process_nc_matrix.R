#' (Internal) Process a Single NetCDF Layer into a SpatRaster
#'
#' @description 
#' Consolidated internal dispatcher that converts a raw matrix/array slice 
#' extracted from a NetCDF variable into a properly formatted \code{terra::SpatRaster}. 
#' Behavior (transpose, flip, NA replacement, CRS, naming) is branched based on 
#' \code{nc_type}. Each branch is self-contained and independent of the others, 
#' so editing one dataset's logic will never affect another.
#'
#' @param layer_data A matrix (or array slice) extracted via \code{ncdf4::ncvar_get()}.
#' @param var_name Character. Base variable name, used for layer naming.
#' @param nc_type Character. Dataset type. See \code{read_nc_spatial_raster()} for the 
#'   full list of supported types.
#' @param ext A \code{terra::ext()} object defining the raster's spatial extent.
#' @param missing_value Numeric. Value to convert to \code{NA}.
#' @param i Optional integer or character. Layer/time index, used in naming 
#'   (e.g., "_1", "_2", or a date string for FLDAS-ABH).
#' @param j Optional integer. Ensemble index, used only for \code{nc_type == "lis_ensemble"}.
#' @param dates_num Optional character vector. Pre-computed date strings, used only 
#'   for \code{nc_type == "modis"} naming.
#' @param ext_utm Optional \code{terra::ext()} object. Native UTM extent, used only for 
#'   \code{nc_type == "swot_2d"}.
#' @param crs_utm Optional character. UTM CRS proj string, used only for 
#'   \code{nc_type == "swot_2d"}.
#' @param reproject_wgs Logical. If \code{TRUE} (default), SWOT output is reprojected 
#'   to WGS84. Only used for \code{nc_type == "swot_2d"}.
#'
#' @return A \code{terra::SpatRaster} object representing the processed layer.
#' @keywords internal
.process_nc_matrix <- function(layer_data, var_name, nc_type, ext, missing_value,
                               i = NULL, j = NULL, dates_num = NULL,
                               ext_utm = NULL, crs_utm = NULL, reproject_wgs = TRUE) {
  
  # ======================================================================= #
  # BRANCH: ESACCI
  # Transpose, NA replacement, extent assignment. No flip, no naming index.
  # ======================================================================= #
  if (nc_type == "esacci") {
    
    r <- terra::rast(t(layer_data))
    r[r == missing_value] <- NA
    terra::ext(r) <- ext
    names(r) <- var_name
    return(r)
  }
  
  # ======================================================================= #
  # BRANCH: CHIRPS (v2 / v3)
  # Transpose, NA replacement, extent, vertical flip.
  # ======================================================================= #
  if (nc_type %in% c("chirpsv2", "chirpsv3")) {
    
    r <- terra::rast(t(layer_data))
    r[r == missing_value] <- NA
    terra::ext(r) <- ext
    r <- terra::flip(r, direction = "vertical")
    if (!is.null(i)) { names(r) <- paste0(var_name, "_", i) } else { names(r) <- var_name }
    return(r)
  }
  
  # ======================================================================= #
  # BRANCH: NLDAS FORCING
  # Transpose, extent, vertical flip. NO NA replacement (matches original logic).
  # ======================================================================= #
  if (nc_type == "nldas_forcing") {
    
    r <- terra::rast(t(layer_data))
    terra::ext(r) <- ext
    r <- terra::flip(r, direction = "vertical")
    names(r) <- var_name
    return(r)
  }
  
  # ======================================================================= #
  # BRANCH: IMERG
  # NO transpose, extent, vertical flip.
  # ======================================================================= #
  if (nc_type == "imerg") {
    
    r <- terra::rast(layer_data)
    terra::ext(r) <- ext
    r <- terra::flip(r, direction = "vertical")
    names(r) <- var_name
    return(r)
  }
  
  # ======================================================================= #
  # BRANCH: LIS
  # Transpose, NA replacement, extent, vertical flip.
  # ======================================================================= #
  if (nc_type == "lis") {
    
    r <- terra::rast(t(layer_data))
    r[r == missing_value] <- NA
    terra::ext(r) <- ext
    r <- terra::flip(r, direction = "vertical")
    if (!is.null(i)) { names(r) <- paste0(var_name, "_", i) } else { names(r) <- var_name }
    return(r)
  }
  
  # ======================================================================= #
  # BRANCH: LIS ENSEMBLE
  # Same spatial handling as LIS, but naming includes ensemble member (j)
  # and optional layer index (i).
  # ======================================================================= #
  if (nc_type == "lis_ensemble") {
    
    r <- terra::rast(t(layer_data))
    r[r == missing_value] <- NA
    terra::ext(r) <- ext
    r <- terra::flip(r, direction = "vertical")
    
    if (!is.null(i)) {
      names(r) <- paste0(var_name, "_ens_", sprintf("%02d", j), "_layer_", sprintf("%02d", i))
    } else {
      names(r) <- paste0(var_name, "_ens_", sprintf("%02d", j))
    }
    return(r)
  }
  
  # ======================================================================= #
  # BRANCH: EnKF
  # Identical spatial handling to LIS (transpose, NA, extent, flip).
  # Kept as its own branch so EnKF-specific tweaks can be made independently.
  # ======================================================================= #
  if (nc_type == "EnKF") {
    
    r <- terra::rast(t(layer_data))
    r[r == missing_value] <- NA
    terra::ext(r) <- ext
    r <- terra::flip(r, direction = "vertical")
    if (!is.null(i)) { names(r) <- paste0(var_name, "_", i) } else { names(r) <- var_name }
    return(r)
  }
  
  # ======================================================================= #
  # BRANCH: FLDAS (Central Asia)
  # Identical spatial handling to LIS. Kept independent for future tweaks.
  # ======================================================================= #
  if (nc_type == "fldas") {
    
    r <- terra::rast(t(layer_data))
    r[r == missing_value] <- NA
    terra::ext(r) <- ext
    r <- terra::flip(r, direction = "vertical")
    names(r) <- var_name
    return(r)
  }
  
  # ======================================================================= #
  # BRANCH: FLDAS-ABH (sub-Saharan Africa)
  # Identical spatial handling to LIS. Naming uses date string passed as `i`.
  # ======================================================================= #
  if (nc_type == "fldas_abh") {
    
    r <- terra::rast(t(layer_data))
    r[r == missing_value] <- NA
    terra::ext(r) <- ext
    r <- terra::flip(r, direction = "vertical")
    if (!is.null(i)) { names(r) <- paste0(var_name, "_", i) } else { names(r) <- var_name }
    return(r)
  }
  
  # ======================================================================= #
  # BRANCH: FLDAS-ABH with Lead Times
  # Identical spatial handling to LIS. `i` here is a pre-built layer name 
  # (e.g., "SubRZSM_lead1_200101") constructed by the caller.
  # ======================================================================= #
  if (nc_type == "fldas_abh_leadtimes") {
    
    r <- terra::rast(t(layer_data))
    r[r == missing_value] <- NA
    terra::ext(r) <- ext
    r <- terra::flip(r, direction = "vertical")
    names(r) <- i   # `i` is the full pre-built layer name string in this case
    return(r)
  }
  
  # ======================================================================= #
  # BRANCH: NLDAS3 FORCING
  # Identical spatial handling to LIS. Kept independent for future tweaks.
  # ======================================================================= #
  if (nc_type == "nldas3_forcing") {
    
    r <- terra::rast(t(layer_data))
    r[r == missing_value] <- NA
    terra::ext(r) <- ext
    r <- terra::flip(r, direction = "vertical")
    if (!is.null(i)) { names(r) <- paste0(var_name, "_", i) } else { names(r) <- var_name }
    return(r)
  }
  
  # ======================================================================= #
  # BRANCH: JRA-3Q
  # Transpose, NA replacement, longitude conversion (0-360 -> -180/180), CRS.
  # ======================================================================= #
  if (nc_type == "jra_3q") {
    
    r <- terra::rast(t(layer_data))
    r[r == missing_value] <- NA
    terra::ext(r) <- ext
    r <- .convert_0_360_to_0_180(r)
    terra::crs(r) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    if (!is.null(i)) { names(r) <- paste0(var_name, "_", i) } else { names(r) <- var_name }
    return(r)
  }
  
  # ======================================================================= #
  # BRANCH: NLDAS, OWN, TIME_ARRAY, MERRA2, MODIS
  # Base processor: Transpose, NA replacement, extent. No flip.
  # MODIS additionally overrides the layer name with a supplied date string.
  # ======================================================================= #
  if (nc_type %in% c("nldas", "own", "time_array", "merra2", "modis")) {
    
    r <- terra::rast(t(layer_data))
    r[r == missing_value] <- NA
    terra::ext(r) <- ext
    
    if (nc_type == "modis" && !is.null(dates_num) && !is.null(i)) {
      names(r) <- paste0(var_name, "_", dates_num[i])
    } else if (!is.null(i)) {
      names(r) <- paste0(var_name, "_", i)
    } else {
      names(r) <- var_name
    }
    return(r)
  }
  
  # ======================================================================= #
  # BRANCH: SWOT 2D
  # Custom orientation flip, fill-value masking, native UTM raster build, 
  # optional reprojection to WGS84.
  # ======================================================================= #
  if (nc_type == "swot_2d") {
    
    # Step A: Orient array to raster convention
    mat <- t(layer_data)            # [y(S->N), x(W->E)]
    mat <- mat[nrow(mat):1, ]       # [y(N->S), x(W->E)]
    
    # Step B: Mask fill/missing values
    mat[mat == missing_value] <- NA
    mat[abs(mat) > 1e+14] <- NA  # Catch common SWOT fill value if missing_value not set
    
    # Step C: Build raster in native UTM (perfect 100m x 100m)
    r_utm <- terra::rast(mat)
    terra::ext(r_utm) <- ext_utm
    terra::crs(r_utm) <- crs_utm
    
    # Step D: Assign layer name
    if (!is.null(i)) {
      names(r_utm) <- paste0(var_name, "_", i)
    } else {
      names(r_utm) <- var_name
    }
    
    # Step E: Reproject to WGS84 if requested
    if (reproject_wgs) {
      r_out <- terra::project(r_utm, "EPSG:4326", method = "bilinear")
      cat("  [SWOT] Reprojected to WGS84 | res ~",
          round(terra::res(r_out)[1], 6), "x",
          round(terra::res(r_out)[2], 6), "\n")
    } else {
      r_out <- r_utm
      cat("  [SWOT] Kept in native UTM | res =",
          terra::res(r_out)[1], "m x", terra::res(r_out)[2], "m\n")
    }
    
    return(r_out)
  }
  
  # ======================================================================= #
  # FALLBACK: Unrecognized nc_type
  # ======================================================================= #
  stop("`.process_nc_matrix()`: Unsupported nc_type '", nc_type, 
       "'. Please add a new branch for this dataset type.")
}