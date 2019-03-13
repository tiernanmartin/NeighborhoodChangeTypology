#' @title Make the Boundary of all Counties in Washington State
#' @description Get the polygon of boundaries of all counties in Washington State using the \code{\link[tigris]{tigris}} package.
#' @return a MULTIPOLYGON simple feature (class =  `sf`)

#' @rdname wa-counties-boundaries
#' @export
prepare_wa_counties_boundaries <- function(path){

  options(tigris_class = "sf")


# GET DATA ----------------------------------------------------------------

  wa_counties_boundaries_prep <- tigris::counties(state = "53") %>%
    sf::st_transform(2926)

# WRITE DATA --------------------------------------------------------------

  sf::st_write(wa_counties_boundaries_prep, path, driver = "GPKG", quiet = TRUE, layer_options = "OVERWRITE=true")


# RETURN ------------------------------------------------------------------

  wa_counties_boundaries_prep_status <- NeighborhoodChangeTypology::get_modified_time(path)

  return(wa_counties_boundaries_prep_status)

}

#' @rdname wa-counties-boundaries
#' @export
make_wa_counties_boundaries <- function(path){

  wa_counties_boundaries <- suppressWarnings(suppressMessages(sf::st_read(path,quiet = TRUE)))

  return(wa_counties_boundaries)

}
