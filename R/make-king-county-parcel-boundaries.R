
#' @title Make King County's Parcel Boundaries (2018)
#' @description Download the polygons for the tax parcels in King County, WA from osf.io.
#' @return a MULTIPOLYGON simple feature (class =  `sf`)
#' @importFrom osfr download_files
#' @importFrom sf read_sf
#' @importFrom sf st_transform
#' @export
make_parcel_boundaries <- function(){

  # Original source: ftp://ftp.kingcounty.gov/gis-web/GISData/parcel_SHP.zip

  data_osfr_id <- "p7tq8"

  data_fp <- "extdata/"

  data_fp_full <- osfr::download_files(id = data_osfr_id, path = data_fp)

  parcel_raw <- sf::read_sf("extdata/kc-parcels-spatial.gpkg")

  parcel_ready <- parcel_raw %>%
    sf::st_transform(2926) %>%
    dplyr::rename_if(not_sfc, snakecase::to_screaming_snake_case)

  parcel_boundaries <- parcel_ready

  return(parcel_boundaries)

}
