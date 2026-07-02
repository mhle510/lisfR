#' Get the Length of a Specific NetCDF Dimension
#'
#' @description 
#' Iterates through a provided list of possible dimension names and returns the length 
#' of the first one that exists in the opened NetCDF object. This is highly useful 
#' when dealing with different NetCDF files that might use varying naming conventions 
#' (e.g., checking for both "lon" and "longitude").
#'
#' @param nc_data An object of class `ncdf4`, which is returned by `ncdf4::nc_open()`.
#' @param possible_names A character vector of dimension names to search for 
#'   (e.g., `c("time", "Time", "t")`).
#'
#' @return An integer representing the length of the matched dimension.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' nc_data <- ncdf4::nc_open("path/to/data.nc")
#' 
#' # Will check for "time" first, and if not found, will check for "Time"
#' time_len <- get_nc_dim_length(nc_data, c("time", "Time"))
#' 
#' ncdf4::nc_close(nc_data)
#' }
get_nc_dim_length <- function(nc_data, possible_names) {
  
  # Iterate through the vector of possible names
  for (name in possible_names) {
    if (!is.null(nc_data$dim[[name]])) {
      return(nc_data$dim[[name]]$len)
    }
  }
  
  # If the loop finishes without returning, none of the names were found
  stop(
    "None of the possible dimension names (", 
    paste(possible_names, collapse = ", "), 
    ") were found in the provided nc_data."
  )
}