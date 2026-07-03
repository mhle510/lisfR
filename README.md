# lisfR

**R tools for working with large-scale hydrological modeling output and processing satellite datasets.**

`lisfR` provides a collection of utility functions for reading, writing, and processing NetCDF outputs from land surface models (such as LIS and FLDAS) and satellite-derived datasets (including CHIRPS, MODIS, IMERG, and SWOT). It includes tools to:

- Convert NetCDF files into georeferenced raster stacks (`nc_to_raster`)
- Export raster stacks back into NetCDF format (`raster_to_nc`)
- Inspect NetCDF metadata, variables, dimensions, and timestamps
- Calculate model domain boundaries from shapefiles (`calc_ldt_domain_bounds`)
- Compare model outputs across ensembles and time (`lis_compare_outputs`)

---

## Installation

`lisfR` is not on CRAN and must be installed directly from GitHub.

### 1. Install the `remotes` package (if you don't already have it)

```r
install.packages("remotes")
```

### 2. Install `lisfR` from GitHub

```r
remotes::install_github("your-github-username/lisfR")
```

Alternatively, if you prefer `devtools`:

```r
install.packages("devtools")
devtools::install_github("your-github-username/lisfR")
```

### 3. Load the package

```r
library(lisfR)
```

---

## Dependencies

`lisfR` relies on the following R packages, which will be installed automatically as dependencies:

- [`ncdf4`](https://cran.r-project.org/package=ncdf4) — reading/writing NetCDF files
- [`terra`](https://cran.r-project.org/package=terra) — spatial raster operations
- [`sf`](https://cran.r-project.org/package=sf) — vector/shapefile operations
- [`lubridate`](https://cran.r-project.org/package=lubridate) — date/time parsing

---

## Basic Usage

### Convert a NetCDF file to a raster stack

```r
library(lisfR)

r <- nc_to_raster(
  nc_file  = "path/to/LIS_HIST_202301010000.d01.nc",
  var_names = "SoilMoist_tavg",
  nc_type  = "lis"
)

terra::plot(r)
```

### Export a raster stack back to NetCDF

```r
raster_to_nc(
  output_file   = "output.nc",
  raster_stack  = r,
  var_names     = "SoilMoist_tavg",
  units_netcdf  = "m3/m3",
  title_name    = "Example Output",
  date_strings  = "2023-01-01"
)
```

### Calculate LDT domain bounds from a shapefile

```r
domain <- calc_ldt_domain_bounds(
  shp_path = "path/to/basin_boundary.shp",
  dx = 0.05,
  dy = 0.05
)
```


