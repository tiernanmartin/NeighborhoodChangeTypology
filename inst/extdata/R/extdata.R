
# EXTERNAL DATASETS LIST --------------------------------------------------

exteral_datasets_list <- NULL

usethis::use_data(exteral_datasets_list)


# KING COUNTY TAX ASSESSOR DATA -------------------------------------------

library(osfr)
library(janitor)
library(rgdal)
library(tidyverse)
library(sf)


val <- read_csv("/Users/UrbanDesigner/Desktop/Year2018/EXTR_ValueHistory_V.csv") %>%
  clean_names(case = "screaming_snake")

p <- read_csv("/Users/UrbanDesigner/Desktop/Year2018/EXTR_Parcel.csv") %>%
  clean_names(case = "screaming_snake")

condo <- read_csv("/Users/UrbanDesigner/Desktop/Year2018/EXTR_CondoUnit2.csv") %>%
  clean_names(case = "screaming_snake")

lut <- read_csv("/Users/UrbanDesigner/Desktop/Year2018/EXTR_LookUp.csv") %>%
  clean_names(case = "screaming_snake")


present_use_lut <- lut %>%
  filter(LU_TYPE %in% 102) %>%
  transmute(PRESENT_USE = LU_ITEM,
            PRESENT_USE_DESC = LU_DESCRIPTION)

p %>%
  transmute(PIN = NeighborhoodChangeTypology::make_pin(MAJOR, MINOR),
         PRESENT_USE) %>%
  left_join(present_use_lut, by = "PRESENT_USE") %>%
  drop_na %>%
  count(PRESENT_USE_DESC, sort = TRUE) %>% filter(n>100)

ogrListLayers("/Users/UrbanDesigner/Desktop/Year2005.gdb")

ogrListLayers("/Users/UrbanDesigner/Desktop/Year2010.gdb")

p_2005 <- st_read(dsn = "/Users/UrbanDesigner/Desktop/Year2005.gdb", layer = "parcel_extr")

condo_2005 <- st_read(dsn = "/Users/UrbanDesigner/Desktop/Year2005.gdb", layer = "condounit_extr")

lut_2005 <- st_read(dsn = "/Users/UrbanDesigner/Desktop/Year2005.gdb", layer = "lookup_extr")

p_2010 <- st_read(dsn = "/Users/UrbanDesigner/Desktop/Year2010.gdb", layer = "parcel_extr")

condo_2010 <- st_read(dsn = "/Users/UrbanDesigner/Desktop/Year2010.gdb", layer = "condounit_extr")


# WRITE

write_csv(p_2005, "/Users/UrbanDesigner/Desktop/kc-assessor-parcels-2005-2010-2018/EXTR_Parcel_2005.csv")

write_csv(condo_2005, "/Users/UrbanDesigner/Desktop/kc-assessor-parcels-2005-2010-2018/EXTR_Condo_Unit_2005.csv")

write_csv(lut_2005, "/Users/UrbanDesigner/Desktop/kc-assessor-parcels-2005-2010-2018/EXTR_LookUp_2005.csv")

write_csv(p_2010, "/Users/UrbanDesigner/Desktop/kc-assessor-parcels-2005-2010-2018/EXTR_Parcel_2010.csv")

write_csv(condo_2010, "/Users/UrbanDesigner/Desktop/kc-assessor-parcels-2005-2010-2018/EXTR_Condo_Unit_2010.csv")


# When the zip file is ready, update the osf.io file

login()

zip_fp <- "/Users/UrbanDesigner/Desktop/kc-assessor-parcels-2005-2010-2018.zip"


message(paste0("The following process takes ~ 30 minutes - check back at: ",Sys.time()+1815))

osfr:::upload_revision(id = "t7b8v",path = zip_fp)


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
