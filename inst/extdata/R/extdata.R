
# EXTERNAL DATASETS LIST --------------------------------------------------

exteral_datasets_list <- NULL

usethis::use_data(exteral_datasets_list)



# KING COUNTY PARCEL BOUNDARIES -------------------------------------------

library(osfr)
library(tidyverse)
library(sf)


url <- "ftp://ftp.kingcounty.gov/gis-web/GISData/parcel_SHP.zip"

tmp <- tempfile()

download.file(url, tmp)

unzip( tmp,exdir = "extdata/parcel_SHP")

parcel <- read_sf("extdata/parcel_SHP/parcel")


parcel_fp <- "extdata/kc-parcels-spatial.gpkg"

st_write(parcel, dsn = parcel_fp, driver = "GPKG")


login() # this won'r work for anyone except Tiernan Martin

osfr::upload_files(id = "sj7n9", path = parcel_fp)
