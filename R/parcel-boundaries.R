
#' @title Make King County's Parcel Boundaries (2018)
#' @description Download the polygons for the tax parcels in King County, WA from osf.io.
#' @return a MULTIPOLYGON simple feature (class =  `sf`)
#' @importFrom osfr download_files
#' @importFrom sf read_sf
#' @importFrom sf st_transform

#' @rdname parcel-boundaries
#' @export
prepare_parcel_boundaries <- function(path){

  # GET DATA ----------------------------------------------------------------

  parcel_url <- "ftp://ftp.kingcounty.gov/gis-web/GISData/parcel_SHP.zip"

  download.file(parcel_url, path, quiet = TRUE)


  # RETURN ------------------------------------------------------------------

  parcel_prep_status <- NeighborhoodChangeTypology::get_modified_time(path)

  return(parcel_prep_status)


}

#' @rdname parcel-boundaries
#' @export
make_parcel_boundaries <- function(path){

  # Original source: ftp://ftp.kingcounty.gov/gis-web/GISData/parcel_SHP.zip

  unzip(path, exdir = "extdata/osf")

  parcel_raw <- suppressWarnings(suppressMessages(sf::read_sf("extdata/osf/parcel/parcel.shp")))

  parcel_ready <- parcel_raw %>%
    sf::st_transform(2926) %>%
    dplyr::rename_if(not_sfc, snakecase::to_screaming_snake_case)

  parcel_boundaries <- parcel_ready

  return(parcel_boundaries)

}
