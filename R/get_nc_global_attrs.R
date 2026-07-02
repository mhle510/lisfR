#' Get and Print Global Attributes of a NetCDF File
#'
#' @description 
#' Opens a NetCDF file, extracts all global attributes, prints them to the console 
#' in a readable format, and returns them as a list for programmatic use.
#'
#' @param nc_file Character string. The file path to the NetCDF file.
#'
#' @return A list containing the global attributes of the NetCDF file.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' global_attrs <- get_nc_global_attrs("path/to/your/netcdf_file.nc")
#' }
get_nc_global_attrs <- function(nc_file) {
  
  # 1. Check if the file exists to provide a clear error message
  if (!file.exists(nc_file)) {
    stop("The file '", nc_file, "' does not exist.")
  }
  
  # 2. Open the NetCDF file
  nc_data <- ncdf4::nc_open(nc_file)
  
  # 3. Ensure the NetCDF file is closed when the function exits (even if it crashes)
  on.exit(ncdf4::nc_close(nc_data), add = TRUE)
  
  # 4. Get all global attributes (the '0' argument specifies global)
  global_attributes <- ncdf4::ncatt_get(nc_data, 0)
  
  # 5. Print a clean header
  cat("Global Attributes of NetCDF File:", nc_file, "\n")
  cat(rep("-", 50), "\n")
  
  # 6. Iterate over the global attributes and print each one
  for (attr_name in names(global_attributes)) {
    attr_value <- global_attributes[[attr_name]]
    cat(attr_name, ":", attr_value, "\n")
  }
  
  cat(rep("-", 50), "\n")
  
  # 7. Return the global attributes programmatically
  return(global_attributes)
}