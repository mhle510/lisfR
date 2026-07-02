#' Get Variables and Units from a NetCDF File
#'
#' @description 
#' Opens a NetCDF file, extracts the names of all variables, retrieves 
#' their corresponding units, and returns them as a formatted data frame.
#'
#' @param nc_file Character string. The file path to the NetCDF file.
#'
#' @return A data frame with two columns: \code{var_name} (the name of the variable) 
#'   and \code{units} (the units of the variable, or \code{NA} if not specified).
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' vars_df <- get_nc_vars("path/to/your/netcdf_file.nc")
#' print(vars_df)
#' }
get_nc_vars <- function(nc_file) {
  
  # Check if the file exists before trying to open it
  if (!file.exists(nc_file)) {
    stop("The file '", nc_file, "' does not exist.")
  }
  
  # Open the NetCDF file
  nc_data <- ncdf4::nc_open(nc_file)
  
  # Ensure the NetCDF file is closed when the function exits (even on error)
  on.exit(ncdf4::nc_close(nc_data), add = TRUE)
  
  # Extract variable names
  variable_names <- names(nc_data$var)
  
  # Initialize vectors to store the data
  var_names_vec <- character()
  units_vec <- character()
  
  # Loop through each variable to get its name and units
  for (var_name in variable_names) {
    var_units <- nc_data$var[[var_name]]$units
    
    # Handle missing or empty units
    if (is.null(var_units) || var_units == "") {
      var_units <- NA_character_
    }
    
    # Append to the vectors
    var_names_vec <- c(var_names_vec, var_name)
    units_vec <- c(units_vec, var_units)
  }
  
  # Create a data frame from the extracted data
  vars_df <- data.frame(
    var_name = var_names_vec, 
    units = units_vec, 
    stringsAsFactors = FALSE
  )
  
  return(vars_df)
}