#' Get the Dimensions of a NetCDF Variable
#'
#' @description 
#' Opens a NetCDF file, extracts a specific variable, prints its dimensions 
#' (e.g., longitude x latitude x time) to the console, and safely closes the file.
#'
#' @param nc_file Character string. The file path to the NetCDF file.
#' @param var_name Character string. The name of the variable inside the NetCDF file.
#'
#' @return An integer vector representing the dimensions (shape) of the extracted variable.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' dims <- get_nc_var_dims("path/to/your/netcdf_file.nc", "soil_moisture")
#' }
get_nc_var_dims <- function(nc_file, var_name) {
  
  # Check if the file exists
  if (!file.exists(nc_file)) {
    stop("The file '", nc_file, "' does not exist.")
  }
  
  # Open the NetCDF file
  nc_data <- ncdf4::nc_open(nc_file)
  
  # Ensure the NetCDF file is ALWAYS closed when the function exits (fixes previous bug)
  on.exit(ncdf4::nc_close(nc_data), add = TRUE)
  
  # Extract the variable's array data
  var_array <- ncdf4::ncvar_get(nc = nc_data, varid = var_name)
  
  # Extract the dimensions
  var_dims <- dim(var_array)
  
  # Print the dimensions to the console (maintaining your original functionality)
  print(var_dims)
  
  # Return the dimensions programmatically
  return(var_dims)
}