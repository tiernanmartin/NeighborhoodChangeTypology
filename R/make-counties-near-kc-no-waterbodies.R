#' @title Make the Boundary of the Counties Near King County (Without Major Waterbodies)
#' @description Get the polygon of boundaries of all counties in Washington State with the boundaries of major
#'   waterbodies removed.
#' @param wa_counties_boundaries desc
#' @param waterbodies desc
#' @return a MULTIPOLYGON simple feature (class =  `sf`)
#' @export
make_counties_nearby_kc_no_waterbodies <- function(wa_counties_boundaries, waterbodies){

  wa_counties_boundaries <- sf::st_transform(wa_counties_boundaries, 4326)

  waterbodies <- sf::st_transform(waterbodies, 4326)

  counties_nearby_kc_no_waterbodies <- sf::st_crop(wa_counties_boundaries, sf::st_bbox(wa_counties_boundaries[wa_counties_boundaries$NAME %in% "King","geom"])) %>%
    sf::st_union(by_feature = TRUE) %>%
    sf::st_difference(sf::st_union(waterbodies))

  return(counties_nearby_kc_no_waterbodies)

}
