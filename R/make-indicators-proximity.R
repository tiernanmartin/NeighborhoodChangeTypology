#' @title Make The Proximity Indicators
#' @description Description
#' @param census_tracts_2016_trimmed desc
#' @param indicator_value_template desc
#' @return a `tibble`
#' @export
make_indicators_proximity <- function(census_tracts_2016_trimmed,
                                      indicator_value_template){


# ST_JOIN TRACTS WITH NEIGHBORS -------------------------------------------


  tr_geoid <- census_tracts_2016_trimmed %>%
    # remove the letters, spaces, and commas
    dplyr::transmute(GEOGRAPHY_ID_SHORT = stringr::str_remove_all(GEOGRAPHY_NAME,"[[:alpha:]]|[[:space:]]|,"))


  tract_neighbors <- census_tracts_2016_trimmed %>%
    sf::st_join(tr_geoid, sf::st_touches) %>%
    sf::st_set_geometry(NULL) %>%
    tibble::as_tibble() %>%
    dplyr::group_by_at(dplyr::vars(-dplyr::matches("GEOGRAPHY_ID_SHORT"))) %>%
    tidyr::nest() %>%
    # combine the tract GEOGRAPHY_IDs into a comma-separated character string
    dplyr::mutate(PROXIMITY_DESC = purrr::map_chr(data, ~stringr::str_c(.x$GEOGRAPHY_ID_SHORT, collapse = ", "))) %>%
    dplyr::select(-data)


  indicators_proximity <- tract_neighbors

  # RETURN ------------------------------------------------------------------

  return(indicators_proximity)

}
