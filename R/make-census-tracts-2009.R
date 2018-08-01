
#' @title Make King County's Census Tract Boundaries (2009)
#' @description Download the polygons for the census tracts in King County, WA from osf.io.
#' @return a MULTIPOLYGON simple feature (class =  `sf`)
#' @importFrom osfr download_files
#' @importFrom sf read_sf
#' @importFrom sf st_transform
#' @export
make_census_tracts_2009 <- function(){

  # Original source: ftp://ftp.census.gov/geo/tiger/TIGER2009/53_WASHINGTON/53033_King_County/tl_2009_53033_tract00.zip

  data_osfr_id <- "5gx8a"

  data_fp <- "extdata/"

  data_fp_full <- osfr::download_files(id = data_osfr_id, path = data_fp)

  unzip(data_fp_full, exdir = "extdata/census-tracts-2009")

  kc_tracts_2009_raw <- sf::read_sf("extdata/census-tracts-2009")

  kc_tracts_2009_ready <- kc_tracts_2009_raw %>%
    sf::st_transform(2926)

  census_tracts_2009 <- kc_tracts_2009_ready

}
