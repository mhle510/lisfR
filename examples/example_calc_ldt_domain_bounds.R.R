
library(lisfR)

shp_path <- "inst/RedRiver/Hong_TB_all.shp"

domain <- calc_ldt_domain_bounds(
  shp_path = shp_path,
  dx = 0.05,
  dy = 0.05,
  buffer_coef = 1.5,
  decimals = 4
)

