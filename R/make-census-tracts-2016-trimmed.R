#' @title Make King County's Census Tract Boundaries (2016) Without Waterbodies
#' @description Create King County's census tract boundaries (2016) without waterbodies.
#' @param census_tracts_2016 desc
#' @param waterbodies desc
#' @param acs_data desc
#' @return a MULTIPOLYGON simple feature (class =  `sf`)
#' @export
make_census_tracts_2016_trimmed <- function(census_tracts_2016, waterbodies, acs_data){



# REMOVE WATERBODIES FROM TRACT POLYGONS ----------------------------------

  census_tracts_2016_no_water <- sf::st_difference(census_tracts_2016, sf::st_union(waterbodies)) %>%
    dplyr::transmute(GEOGRAPHY_ID = as.character(GEOID))


# ADD GEOGRAPHY_* FIELDS --------------------------------------------------

  geog_fields <- acs_data %>%
    dplyr::filter(GEOGRAPHY_TYPE %in% "tract") %>% # drop counties
    dplyr::select(dplyr::starts_with("GEOGRAPHY")) %>%
    dplyr::distinct()

  census_tracts_2016_trimmed_ready <- geog_fields %>%
      dplyr::left_join(census_tracts_2016_no_water, by = "GEOGRAPHY_ID") %>%
      sf::st_as_sf()


  census_tracts_2016_trimmed <- census_tracts_2016_trimmed_ready

# RETURN ------------------------------------------------------------------

  return(census_tracts_2016_trimmed)

}
