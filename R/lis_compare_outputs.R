#' Compare Two LIS Output Stacked Rasters Generically
#'
#' @description
#' A LIS-specific utility that reads two directories of `LIS_HIST` NetCDF files, 
#' matches them by date, extracts specified layers, and generates comparative 
#' plots. It plots timeseries for 10 random valid pixels and a domain average, 
#' calculating R, RMSE, and ubRMSE metrics.
#'
#' @param folder_1 Character. Path to the folder containing the first set of netCDF files.
#' @param folder_2 Character. Path to the folder containing the second set of netCDF files.
#' @param start_date Character. String in "YYYYMMDD" format.
#' @param end_date Character. String in "YYYYMMDD" format.
#' @param base_var Character. The base variable name to pass into \code{convert_netcdf_to_stacked_raster}.
#' @param var_1 Character. The specific layer name to extract from output 1 stack.
#' @param var_2 Character. The specific layer name to extract from output 2 stack.
#' @param nc_type_1 Character. \code{nc_type} argument for output 1 (default: "lis").
#' @param nc_type_2 Character. \code{nc_type} argument for output 2 (default: "lis_ensemble").
#' @param name_1 Character. Label for output 1 in plots (default: "Output 1").
#' @param name_2 Character. Label for output 2 in plots (default: "Output 2").
#'
#' @return Invisibly returns a list containing \code{domain_metrics} and \code{dates_processed}.
#' 
#' @export
lis_compare_outputs <- function(folder_1, folder_2, start_date, end_date, 
                                base_var, var_1, var_2, 
                                nc_type_1 = "lis", nc_type_2 = "lis_ensemble",
                                name_1 = "Output 1", name_2 = "Output 2") {
  
  # Safely handle user's plotting parameters so we don't permanently override them
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  
  # 1. List files
  list_files_1 <- list.files(folder_1, pattern = "LIS_HIST_.*\\.nc$", recursive = TRUE, full.names = TRUE)
  list_files_2 <- list.files(folder_2, pattern = "LIS_HIST_.*\\.nc$", recursive = TRUE, full.names = TRUE)
  
  # 2. Extract dates from filenames (assumes format LIS_HIST_YYYYMMDDHHMM.d01.nc)
  dates_1 <- substr(basename(list_files_1), 10, 17)
  dates_2 <- substr(basename(list_files_2), 10, 17)
  
  # 3. Filter files based on start_date and end_date
  idx_1 <- which(dates_1 >= start_date & dates_1 <= end_date)
  idx_2 <- which(dates_2 >= start_date & dates_2 <= end_date)
  
  files_1_sub <- list_files_1[idx_1]
  files_2_sub <- list_files_2[idx_2]
  dates_1_sub <- dates_1[idx_1]
  dates_2_sub <- dates_2[idx_2]
  
  # Find matching dates in both folders to ensure 1:1 comparison
  common_dates <- intersect(dates_1_sub, dates_2_sub)
  common_dates <- sort(common_dates)
  
  if(length(common_dates) == 0) {
    stop("No overlapping dates found between Output 1 and Output 2 in the specified date range.")
  }
  
  # Subset to only matching dates
  files_1_sub <- files_1_sub[match(common_dates, dates_1_sub)]
  files_2_sub <- files_2_sub[match(common_dates, dates_2_sub)]
  
  # 4. Read data and stack
  message(sprintf("Processing %d matching time steps...", length(common_dates)))
  
  ts_1 <- list()
  ts_2 <- list()
  
  for (i in seq_along(common_dates)) {
    # Note: convert_netcdf_to_stacked_raster must be loaded in your package namespace
    r_all_1 <- convert_netcdf_to_stacked_raster(nc_file = files_1_sub[i], var_names = base_var, nc_type = nc_type_1)
    r_all_2 <- convert_netcdf_to_stacked_raster(nc_file = files_2_sub[i], var_names = base_var, nc_type = nc_type_2)
    
    # Extract the specific layers requested
    ts_1[[i]] <- r_all_1[[var_1]]
    ts_2[[i]] <- r_all_2[[var_2]]
  }
  
  # Combine into multi-temporal SpatRasters
  stack_1 <- terra::rast(ts_1) 
  stack_2 <- terra::rast(ts_2)
  
  # 5. Select 10 Random Points
  # Get indices of cells that are not NA in the first layer
  valid_cells <- which(!is.na(terra::values(stack_1[[1]])))
  set.seed(123) # For reproducibility
  random_cells <- sample(valid_cells, size = min(10, length(valid_cells)))
  
  # Extract time series for these 10 points
  pts_ts_1 <- terra::extract(stack_1, random_cells)
  pts_ts_2 <- terra::extract(stack_2, random_cells)
  
  # 6. Helper Function for Metrics
  compute_metrics <- function(obs, sim) {
    valid_idx <- !is.na(obs) & !is.na(sim)
    obs <- obs[valid_idx]
    sim <- sim[valid_idx]
    
    if(length(obs) < 2) return(c(R = NA, RMSE = NA, ubRMSE = NA))
    
    r_val <- cor(obs, sim)
    rmse_val <- sqrt(mean((sim - obs)^2))
    ubrmse_val <- sqrt(mean(((sim - mean(sim)) - (obs - mean(obs)))^2))
    
    return(c(R = r_val, RMSE = rmse_val, ubRMSE = ubrmse_val))
  }
  
  # 7. Plot 10 Random Points
  graphics::par(mfrow = c(5, 2), mar = c(4, 4, 2, 1)) # 5x2 grid
  
  for (i in 1:10) {
    # Extract row i (treat Output 2 as 'obs' and Output 1 as 'sim' conceptually for metrics)
    val_1 <- as.numeric(pts_ts_1[i, ]) 
    val_2 <- as.numeric(pts_ts_2[i, ])
    
    metrics <- compute_metrics(obs = val_2, sim = val_1)
    
    # Plotting
    ylim_range <- range(c(val_1, val_2), na.rm = TRUE)
    plot(1:length(common_dates), val_2, type = "l", col = "blue", lwd = 2,
         ylim = ylim_range, xlab = "Time Step", ylab = base_var,
         main = sprintf("Pt %d | R: %.2f, RMSE: %.3f, ubRMSE: %.3f", 
                        i, metrics["R"], metrics["RMSE"], metrics["ubRMSE"]))
    lines(1:length(common_dates), val_1, col = "red", lwd = 2, lty = 2)
    if(i == 1) legend("topright", legend=c(name_2, name_1), col=c("blue", "red"), lty=c(1, 2), bty="n")
  }
  
  # 8. Compute Domain Average across all grid cells
  domain_avg_1 <- as.numeric(terra::global(stack_1, "mean", na.rm = TRUE)[,1])
  domain_avg_2 <- as.numeric(terra::global(stack_2, "mean", na.rm = TRUE)[,1])
  
  domain_metrics <- compute_metrics(obs = domain_avg_2, sim = domain_avg_1)
  
  # 9. Plot Domain Average
  # Reset par for single plot
  graphics::par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
  
  ylim_range_domain <- range(c(domain_avg_1, domain_avg_2), na.rm = TRUE)
  plot(1:length(common_dates), domain_avg_2, type = "l", col = "blue", lwd = 2,
       ylim = ylim_range_domain, xlab = "Time Step", ylab = paste("Domain Average", base_var),
       main = sprintf("Domain Average Temporal Variation\nR: %.3f | RMSE: %.4f | ubRMSE: %.4f", 
                      domain_metrics["R"], domain_metrics["RMSE"], domain_metrics["ubRMSE"]))
  lines(1:length(common_dates), domain_avg_1, col = "red", lwd = 2, lty = 2)
  legend("topright", legend = c(name_2, name_1), col = c("blue", "red"), lty = c(1, 2), lwd = 2)
  
  # Return metrics silently
  invisible(list(
    domain_metrics = domain_metrics,
    dates_processed = common_dates
  ))
}