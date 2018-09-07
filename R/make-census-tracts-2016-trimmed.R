#' @title Make King County's Census Tract Boundaries (2016) Without Waterbodies
#' @description Create King County's census tract boundaries (2016) without waterbodies.
#' @return a MULTIPOLYGON simple feature (class =  `sf`)
#' @export
make_census_tracts_2016_trimmed <- function(census_tracts_2016, waterbodies){

  census_tracts_2016_trimmed <- sf::st_difference(census_tracts_2016, sf::st_union(waterbodies)) %>%
    dplyr::transmute(GEOID = as.character(GEOID))

  return(census_tracts_2016_trimmed)

}
