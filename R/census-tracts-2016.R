
#' @title Make King County's Census Tract Boundaries (2016)
#' @description Download the polygons for the census tracts in King County, WA
#'   sing the \code{\link[tigris]{tigris}} package.
#' @return a MULTIPOLYGON simple feature (class =  `sf`)

#' @rdname census-tracts-2016
#' @export
prepare_census_tracts_2016 <- function(path){


  # GET DATA ----------------------------------------------------------------

  options(tigris_class = "sf")

  kc_tracts_2016_prep <- tigris::tracts(state = "53", county = "033", year = 2016) %>%
    sf::st_transform(2926)


  # WRITE DATA --------------------------------------------------------------

  sf::st_write(obj = kc_tracts_2016_prep, dsn = path, driver = "GPKG", quiet = TRUE, layer_options = "OVERWRITE=true")

  # RETURN ------------------------------------------------------------------

  kc_tracts_2016_prep_status <- NeighborhoodChangeTypology::get_modified_time(path)

  return(kc_tracts_2016_prep_status)


}


#' @rdname census-tracts-2016
#' @export
make_census_tracts_2016 <- function(path){

  census_tracts_2016 <- suppressWarnings(suppressMessages(sf::st_read(path, quiet = TRUE))) %>%
    dplyr::mutate(GEOID = as.character(GEOID))

  return(census_tracts_2016)

}
