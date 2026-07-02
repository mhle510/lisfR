#' Extract Timestamps from a NetCDF File
#'
#' @description 
#' Extracts and formats time variables from a NetCDF file based on the specific 
#' dataset type (e.g., MODIS, CHIRPS, FLDAS). 
#'
#' @param nc_file Character string. The file path to the NetCDF file.
#' @param type Character string. The type of dataset to determine how the time 
#'   variable is parsed. Supported options are `"modis"`, `"chirps"`, `"fldas_abh"`, 
#'   or `"other"`.
#'
#' @return A vector of timestamps. Depending on the `type`, this may be a `Date` 
#'   object, `POSIXct` object, or character string (for `fldas_abh`).
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' dates <- get_nc_timestamps("path/to/modis_data.nc", type = "modis")
#' }
get_nc_timestamps <- function(nc_file, type) {
  
  # Check if the file exists
  if (!file.exists(nc_file)) {
    stop("The file '", nc_file, "' does not exist.")
  }
  
  # Open the NetCDF file
  nc_data <- ncdf4::nc_open(nc_file)
  
  # Ensure the NetCDF file is closed when the function exits
  on.exit(ncdf4::nc_close(nc_data), add = TRUE)
  
  # Note: NetCDF variables are case-sensitive. 
  # Check for both 'time' and 'Time' in the dimensions
  var_name <- ifelse("Time" %in% names(nc_data$dim), "Time", "time")
  
  # Extract the time variable data and its units
  time_var <- ncdf4::ncvar_get(nc_data, var_name)
  time_units <- ncdf4::ncatt_get(nc_data, var_name, "units")$value
  
  # Print the time units for user awareness
  print(paste("Time units found:", time_units))
  
  # Parse based on dataset type
  if (type == "other") {
    # Parse the reference time from the units attribute
    ref_time <- sub(".*since ", "", time_units)
    ref_datetime <- lubridate::ymd(ref_time)
    
    # Convert the time variable to POSIXct (or Date) format
    timestamps <- ref_datetime + lubridate::days(time_var)
    
  } else if (type == "modis" || type == "chirps") {
    # Extract reference date from time_units and convert to Date object
    ref_time <- as.Date(sub(".*since ", "", time_units))
    
    # Convert time_var (days since reference) to actual dates
    timestamps <- ref_time + time_var
    
  } else if (type == "fldas_abh") {
    # time_var is integer YYYYMM (e.g., 202301)
    # Convert to character, append "01" (first day of month)
    time_str <- paste0(as.character(time_var), "01")
    
    # Note: Currently returning as a character string based on original logic
    timestamps <- time_str  
    
  } else {
    stop("Unsupported 'type' provided. Please use 'other', 'modis', 'chirps', or 'fldas_abh'.")
  }
  
  return(timestamps) 
}