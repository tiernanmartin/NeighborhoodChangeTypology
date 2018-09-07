#' @title Make White Center Place
#' @description This is a temporary function.
#' @return a MULTIPOLYGON simple feature (class =  `sf`)

#' @rdname white-center-place
#' @export
prepare_white_center_place <- function(path){


  # GET DATA ----------------------------------------------------------------


  options(tigris_class = "sf")

  white_center_place <- tigris::places(state = "WA") %>%
    dplyr::filter(NAME %in% "White Center") %>%
    sf::st_transform(4326)


  # WRITE -------------------------------------------------------------------

  sf::st_write(white_center_place, path, driver = "GPKG", quiet = TRUE, layer_options = "OVERWRITE=true")


  # RETURN ------------------------------------------------------------------

  white_center_place_prep_status <- NeighborhoodChangeTypology::get_modified_time(path)

  return(white_center_place_prep_status)

}

#' @rdname white-center-place
#' @export
make_white_center_place <- function(path){

  white_center_place <- suppressWarnings(suppressMessages(sf::st_read(path,quiet = TRUE)))

  return(white_center_place)



}
