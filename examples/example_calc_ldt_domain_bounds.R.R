
library(lisfR)

shp_path <- "inst/RedRiver/Hong_TB_all.shp"

domain <- calc_ldt_domain_bounds(
  shp_path = shp_path,
  dx = 0.05,
  dy = 0.05,
  buffer_coef = 1.5,
  decimals = 4
)

cat(
  sprintf("Run domain lower left lat:                                       %s\n", domain$left_lat),
  sprintf("Run domain lower left lon:                                       %s\n", domain$left_lon),
  sprintf("Run domain upper right lat:                                      %s\n", domain$right_lat),
  sprintf("Run domain upper right lon:                                      %s\n", domain$right_lon),
  sprintf("Run domain resolution (dx):                                      %s\n", domain$dx),
  sprintf("Run domain resolution (dy):                                      %s\n", domain$dy)
)
