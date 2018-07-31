
# EXTERNAL DATASETS LIST --------------------------------------------------

exteral_datasets_list <- NULL

usethis::use_data(exteral_datasets_list)


# KING COUNTY BOUNDARY ----------------------------------------------------

loadd(kc_boundary)

usethis::use_data(kc_boundary, overwrite = TRUE, compress = "xz")
