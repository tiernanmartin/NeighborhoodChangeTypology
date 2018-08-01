
# ACS TABLES --------------------------------------------------------------

loadd(acs_tables)

usethis::use_data(acs_tables, overwrite = TRUE, compress = "xz")


# KING COUNTY BOUNDARY ----------------------------------------------------

loadd(kc_boundary)

usethis::use_data(kc_boundary, overwrite = TRUE, compress = "xz")
