#' @title Make  Typology
#' @description This is a temporary function.
#' @return a MULTIPOLYGON simple feature (class =  `sf`)
#' @export
make_typology <- function(vulnerability_indicators,demo_change_indicators,previous_typology){

  prev_typo_join <- previous_typology %>%
    dplyr::filter(GEOGRAPHY %in% "tract") %>%
    dplyr::mutate(GEOID = stringr::str_c("53033",GEOID6,sep = ""),
                  NAME_FULL = stringr::str_replace(NAME_FULL,", King County, Washington","")) %>%
    dplyr::select(GEOID,
                  NAME_FULL,
                  tidyselect::matches("HOUS_APPR_KC_LGL|HOUS_ACCL_KC_LGL|HOUS_ADJ_KC_LGL")
    ) %>%
  sf::st_sf()

  typo <- list(vulnerability_indicators,demo_change_indicators,prev_typo_join) %>%
    purrr::reduce(dplyr::inner_join, by = "GEOID") %>%
    dplyr::mutate(TYPO_CHR = dplyr::case_when(
      VULNERABLE_LGL %in% FALSE & DEMO_CHANGE_LGL & TYPO_HOUS_APPR_KC_LGL ~ "Continued Loss",
      VULNERABLE_LGL & DEMO_CHANGE_LGL & TYPO_HOUS_APPR_KC_LGL ~ "Late",
      VULNERABLE_LGL & DEMO_CHANGE_LGL & TYPO_HOUS_ACCL_KC_LGL ~ "Dynamic",
      VULNERABLE_LGL & DEMO_CHANGE_LGL & TYPO_HOUS_ADJ_KC_LGL ~ "Early Type 2",
      VULNERABLE_LGL & DEMO_CHANGE_LGL %in% FALSE & TYPO_HOUS_ACCL_KC_LGL ~ "Early Type 1",
      VULNERABLE_LGL & DEMO_CHANGE_LGL %in% FALSE & TYPO_HOUS_ADJ_KC_LGL ~ "Susceptible",
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
    st_sf

  typology <- typo

}
