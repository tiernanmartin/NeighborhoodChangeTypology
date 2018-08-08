
#' @title Make Census Tract to Parcel Overlay
#' @description Temporary description
#' @param parcel_boundaries Simple feature, Temporary description
#' @param census_tracts_2016 Simple feature, Temporary description.
#' @return a tibble
#' @export
make_parcel_tract_overlay <- function(parcel_boundaries, census_tracts_2016
                                      ){

  # Get the parcel polygon centroids

  p_pt <- parcel_boundaries %>%
    dplyr::select(PIN) %>%
    sf::st_centroid(of_largest_polygon = TRUE) %>%
    dplyr::select(PIN)

  # Subdivide the tract polygon (improves performance)

  tract_subdivide <- lwgeom::st_subdivide(census_tracts_2016, 100) %>%
    sf::st_collection_extract() %>%
    dplyr::select(GEOID)

  # Perform a spatial join (st_intersects)

  parcel_tract_overlay <- sf::st_join(p_pt, tract_subdivide, join = sf::st_intersects) %>%
    sf::st_set_geometry(NULL) %>%
    tibble::as_tibble()

  return(parcel_tract_overlay)

}
