#' @title Make  Typology
#' @description This is a temporary function.
#' @param vulnerability_indicators Tibble, Temporary description.
#' @param demo_change_indicators Tibble, Temporary description.
#' @param previous_typology Tibble, Temporary description.
#' @return a MULTIPOLYGON simple feature (class =  `sf`)
#' @export
make_typology <- function(vulnerability_indicators, demo_change_indicators, housing_market_indicators, census_tracts_2016_trimmed){

  # prepare housing market indicators to be joined

housing_ready <- housing_market_indicators %>%
  dplyr::transmute(GEOID,
            HOUSING_MARKET_TYPE = stringr::str_c("HOUSING", toupper(HOUSING_MARKET_TYPE),"LGL",sep = "_"),
            VALUE = TRUE) %>%
  tidyr::spread(HOUSING_MARKET_TYPE,VALUE) %>%
  dplyr::mutate_at(dplyr::vars(tidyselect::ends_with("LGL")), ~ !is.na(.x))


# Make the typology

typo <- list(vulnerability_indicators, demo_change_indicators, housing_ready, census_tracts_2016_trimmed) %>%
    purrr::reduce(dplyr::inner_join, by = "GEOID") %>%
    dplyr::mutate(TYPO_CHR = dplyr::case_when(
      VULNERABLE_LGL %in% FALSE & DEMO_CHANGE_LGL & HOUSING_APPRECIATED_LGL ~ "Continued Loss",
      VULNERABLE_LGL & DEMO_CHANGE_LGL & HOUSING_APPRECIATED_LGL ~ "Late",
      VULNERABLE_LGL & DEMO_CHANGE_LGL & HOUSING_ACCELERATING_LGL ~ "Dynamic",
      VULNERABLE_LGL & DEMO_CHANGE_LGL & HOUSING_ADJACENT_LGL ~ "Early Type 2",
      VULNERABLE_LGL & DEMO_CHANGE_LGL %in% FALSE & HOUSING_ACCELERATING_LGL ~ "Early Type 1",
      VULNERABLE_LGL & DEMO_CHANGE_LGL %in% FALSE & HOUSING_ADJACENT_LGL ~ "Susceptible",
      TRUE ~ NA_character_
    )) %>%
    dplyr::mutate(TYPO_FCT = factor(TYPO_CHR,
                             levels = c("Susceptible",
                                        "Early Type 1",
                                        "Early Type 2",
                                        "Dynamic",
                                        "Late",
                                        "Continued Loss"
                                        ),
                             ordered = TRUE)) %>%
    sf::st_sf()

typology <- typo

}
