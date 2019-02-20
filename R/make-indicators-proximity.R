#' @title Make The Proximity Indicators
#' @description Description
#' @param census_tracts_2016_trimmed desc
#' @param indicator_type_template desc
#' @return a `tibble`
#' @export
make_indicators_proximity <- function(census_tracts_2016_trimmed,
                                      indicator_type_template){


# ST_JOIN TRACTS WITH NEIGHBORS -------------------------------------------


  tr_geoid <- census_tracts_2016_trimmed %>%
    dplyr::select(GEOGRAPHY_ID)


  tract_neighbors <- census_tracts_2016_trimmed %>%
    sf::st_join(tr_geoid, sf::st_touches, suffix = c("","_NEIGHBOR")) %>%
    sf::st_set_geometry(NULL) %>%
    tibble::as_tibble() %>%
    dplyr::group_by_at(dplyr::vars(-dplyr::matches("NEIGHBOR"))) %>%
    tidyr::nest() %>%
    # combine the tract GEOGRAPHY_IDs into a comma-separated character string
    dplyr::mutate(INDICATOR_TYPE_VALUE_DESC = purrr::map_chr(data, ~stringr::str_c(.x$GEOGRAPHY_ID_NEIGHBOR, collapse = ", "))) %>%
    dplyr::mutate(INDICATOR_TYPE = "PROXIMITY",
                  INDICATOR_TYPE_THRESHOLD = NA_character_,
                  INDICATOR_TYPE_THRESHOLD_VALUE = NA_real_,
                  INDICATOR_TYPE_DESC = "PROXIMITY",
                  INDICATOR_TYPE_VALUE = NA_real_,
                  INDICATOR_TYPE_VALUE_DESC,
                  INDICATOR_TYPE_MODEL = INDICATOR_TYPE_VALUE_DESC) %>%
    dplyr::select(-data)


# REFORMAT ----------------------------------------------------------------

  indicators_proximity_ready <- indicator_type_template %>%
    dplyr::full_join(tract_neighbors,
              by = c("GEOGRAPHY_ID",
                     "GEOGRAPHY_ID_TYPE",
                     "GEOGRAPHY_NAME",
                     "GEOGRAPHY_TYPE",
                     "INDICATOR_TYPE",
                     "INDICATOR_TYPE_THRESHOLD",
                     "INDICATOR_TYPE_THRESHOLD_VALUE",
                     "INDICATOR_TYPE_DESC",
                     "INDICATOR_TYPE_VALUE",
                     "INDICATOR_TYPE_VALUE_DESC",
                     "INDICATOR_TYPE_MODEL"))

  indicators_proximity <- indicators_proximity_ready

  # RETURN ------------------------------------------------------------------

  return(indicators_proximity)

}
