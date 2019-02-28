
#' @title Make The Models
#' @description Temporary description.
#' @param indicators_wide desc
#' @return a `tibble`

#' @rdname models
make_model_pdx16 <- function(indicators_wide,
                             census_tracts_2016_trimmed){


  # FILTER INDICATORS -------------------------------------------------------

  pdx_cols <- names(indicators_wide) %>% purrr::keep(~ stringr::str_detect(.x,"^PDX"))

  pdx_inds <- indicators_wide %>% dplyr::select(dplyr::starts_with("GEOGRAPHY"),
                                                tidyselect::vars_select(names(.),pdx_cols))


  # VULNERABILITY -----------------------------------------------------------

  pdx_vuln_type_inds <- pdx_inds %>%
    dplyr::select(dplyr::matches("GEOGRAPHY"),
                  dplyr::matches("PDX18_VULN.+RELLGL_PCT")) %>%
    dplyr::group_by_at(dplyr::vars(dplyr::matches("GEOGRAPHY"))) %>%
    tidyr::nest() %>%
    dplyr::mutate(PDX18_VULN_3OF4_LGL = purrr::map_lgl(data, ~ rowSums(.x, na.rm = TRUE) >= 3)) %>%
    dplyr::mutate(
      PDX18_VULN_TYPE = dplyr::case_when(
        GEOGRAPHY_TYPE %in% "county" ~ NA_character_,
        is.na(PDX18_VULN_3OF4_LGL) ~ NA_character_,
        PDX18_VULN_3OF4_LGL  ~ "Vulnerable",
        TRUE ~ "Not Vulnerable"
      )
    ) %>%
    dplyr::select(-data)

  # DEMOGRAPHIC_CHANGE ------------------------------------------------------


  is_demochng_white_highed <- function(x){
    x$PDX18_DEMOCHNG_WHITE_CHNGLGL_PCT_2007_2011_TO_2013_2017 &
      x$PDX18_DEMOCHNG_HIGHED_CHNGLGL_PCT_2007_2011_TO_2013_2017
  }

  pdx_demochng_type_inds <- pdx_inds %>%
    dplyr::select(dplyr::matches("GEOGRAPHY"),
                  dplyr::matches("PDX18_DEMOCHNG.+CHNGLGL")) %>%
    dplyr::group_by_at(dplyr::vars(dplyr::matches("GEOGRAPHY"))) %>%
    tidyr::nest() %>%
    dplyr::mutate(PDX18_DEMOCHNG_3OF4_LGL = purrr::map_lgl(data, ~ rowSums(.x, na.rm = TRUE) >= 3)) %>%
    dplyr::mutate(PDX18_DEMOCHNG_WHITEED_LGL = purrr::map_lgl(data, is_demochng_white_highed)) %>%
    dplyr::mutate(
      PDX18_DEMOCHNG_TYPE = dplyr::case_when(
        GEOGRAPHY_TYPE %in% "county" ~ NA_character_,
        is.na(PDX18_DEMOCHNG_3OF4_LGL) | is.na(PDX18_DEMOCHNG_WHITEED_LGL) ~ NA_character_,
        PDX18_DEMOCHNG_3OF4_LGL ~ "Changing",
        PDX18_DEMOCHNG_WHITEED_LGL ~ "Changing (White & High Ed.)",
        TRUE ~ "Not Changing"
      )
    ) %>%
    dplyr::select(-data)



  # HOUSING_MARKET ----------------------------------------------------------

  # create the type indicators needed to determine the housing market type

  pdx_housmrkt_type_inds <- pdx_inds %>%
    dplyr::transmute( GEOGRAPHY_ID,
                      GEOGRAPHY_ID_TYPE,
                      GEOGRAPHY_NAME,
                      GEOGRAPHY_TYPE,
                      PDX18_HOUSMRKT_LOVAL_2000_LGL = !PDX18_HOUSMRKT_HOMEVALACS_RELLGL_MED_2000_2000,
                      PDX18_HOUSMRKT_LOVAL_2017_LGL = !PDX18_HOUSMRKT_HOMEVALACS_RELLGL_MED_2013_2017,
                      PDX18_HOUSMRKT_HIVAL_2000_LGL = PDX18_HOUSMRKT_HOMEVALACS_RELLGL_MED_2000_2000,
                      PDX18_HOUSMRKT_HIVAL_2017_LGL = PDX18_HOUSMRKT_HOMEVALACS_RELLGL_MED_2013_2017,
                      PDX18_HOUSMRKT_LOVAL_2000_HIVAL_2017_LGL = PDX18_HOUSMRKT_HOMEVALACS_RELCHNGLGL_MED_2000_2000_TO_2013_2017,
                      PDX18_HOUSMRKT_LOAPPR_2010_2017_LGL = !PDX18_HOUSMRKT_HOMEVALACS_CHNGLGL_MED_2006_2010_TO_2013_2017,
                      PDX18_HOUSMRKT_HIAPPR_2010_2017_LGL = PDX18_HOUSMRKT_HOMEVALACS_CHNGLGL_MED_2006_2010_TO_2013_2017,
                      PDX18_HOUSMRKT_HIAPPR_2000_2017_LGL = PDX18_HOUSMRKT_HOMEVALACS_CHNGLGL_MED_2000_2000_TO_2013_2017,
                      PDX18_HOUSMRKT_HIVALHIAPPR_LGL = PDX18_HOUSMRKT_HIVAL_2017_LGL | PDX18_HOUSMRKT_HIAPPR_2010_2017_LGL
    )

  # the spillover indicator requires a spatial join

  pdx_housmrkt_type_inds_sf <- pdx_housmrkt_type_inds %>%
    dplyr::left_join(census_tracts_2016_trimmed,
                     by = c("GEOGRAPHY_ID",
                            "GEOGRAPHY_ID_TYPE",
                            "GEOGRAPHY_NAME",
                            "GEOGRAPHY_TYPE")) %>%
    sf::st_sf()

  pdx_housmrkt_type_inds_hivalappr_only_sf <- pdx_housmrkt_type_inds_sf %>%
    dplyr::transmute(TOUCHES_HIVALHIAPPR = PDX18_HOUSMRKT_HIVALHIAPPR_LGL)

  pdx_housmrkt_type_touchhivalhiappr <- pdx_housmrkt_type_inds_sf %>%
    sf::st_join(pdx_housmrkt_type_inds_hivalappr_only_sf, sf::st_touches) %>%
    sf::st_set_geometry(NULL) %>%
    tibble::as_tibble() %>%
    dplyr::group_by_at(dplyr::vars(-dplyr::matches("TOUCHES_HIVALHIAPPR"))) %>%
    tidyr::nest() %>%
    dplyr::mutate(PDX18_HOUSMRKT_TOUCHHIVALHIAPPR_LGL = purrr::map_lgl(data, ~ any(.x$TOUCHES_HIVALHIAPPR))) %>%
    dplyr::select(-data)

  pdx_housmrkt_type <- pdx_housmrkt_type_touchhivalhiappr %>%
    dplyr::mutate(PDX18_HOUSMRKT_ADJACENT_LGL =
                    PDX18_HOUSMRKT_LOVAL_2017_LGL &
                    PDX18_HOUSMRKT_LOAPPR_2010_2017_LGL &
                    PDX18_HOUSMRKT_TOUCHHIVALHIAPPR_LGL)  %>%
    dplyr::mutate(PDX18_HOUSMRKT_ACCELERATING_LGL =
                    PDX18_HOUSMRKT_LOVAL_2017_LGL &
                    PDX18_HOUSMRKT_HIAPPR_2010_2017_LGL) %>%
    dplyr::mutate(PDX18_HOUSMRKT_APPRECIATED_LGL =
                    PDX18_HOUSMRKT_LOVAL_2000_HIVAL_2017_LGL &
                    PDX18_HOUSMRKT_HIAPPR_2000_2017_LGL) %>%
    dplyr::mutate(PDX18_HOUSMRKT_LONGTERM_LOW_LGL =
                    PDX18_HOUSMRKT_LOVAL_2000_LGL &
                    PDX18_HOUSMRKT_LOVAL_2017_LGL &
                    PDX18_HOUSMRKT_LOAPPR_2010_2017_LGL) %>%
    dplyr::mutate(PDX18_HOUSMRKT_LONGTERM_HI_LGL =
                    PDX18_HOUSMRKT_HIVAL_2000_LGL &
                    PDX18_HOUSMRKT_HIVAL_2017_LGL) %>%
    dplyr::mutate(PDX18_HOUSMRKT_TYPE = dplyr::case_when(
      GEOGRAPHY_TYPE %in% c("county","community") ~ NA_character_,
      PDX18_HOUSMRKT_APPRECIATED_LGL ~ "Appreciated",
      PDX18_HOUSMRKT_ACCELERATING_LGL ~ "Accelerating",
      PDX18_HOUSMRKT_ADJACENT_LGL ~ "Adjacent",
      PDX18_HOUSMRKT_LONGTERM_LOW_LGL ~ "Long-term Low Cost",
      PDX18_HOUSMRKT_LONGTERM_HI_LGL ~ "Long-term High Cost",
      TRUE ~ "Other"
    ))



  # TYPOLOGY ----------------------------------------------------------------


  model_pdx16 <- list(pdx_vuln_type_inds,
     pdx_demochng_type_inds,
     pdx_housmrkt_type) %>%
    purrr::reduce(dplyr::full_join) %>%
    dplyr::mutate(PDX18_TYPE = dplyr::case_when(
      GEOGRAPHY_TYPE %in% c("county","community") ~ NA_character_,
      PDX18_VULN_TYPE %in% c("Not Vulnerable") &
        PDX18_DEMOCHNG_TYPE %in% c("Changing (White & High Ed.)") &
        PDX18_HOUSMRKT_TYPE %in% c("Appreciated") ~ "Continued Loss",
      PDX18_VULN_TYPE %in% c("Vulnerable") &
        PDX18_DEMOCHNG_TYPE %in% c("Changing") &
        PDX18_HOUSMRKT_TYPE %in% c("Appreciated") ~ "Late",
      PDX18_VULN_TYPE %in% c("Vulnerable") &
        PDX18_DEMOCHNG_TYPE %in% c("Changing") &
        PDX18_HOUSMRKT_TYPE %in% c("Accelerating") ~ "Dynamic",
      PDX18_VULN_TYPE %in% c("Vulnerable") &
        PDX18_DEMOCHNG_TYPE %in% c("Changing") &
        PDX18_HOUSMRKT_TYPE %in% c("Adjacent") ~ "Early Type 2",
      PDX18_VULN_TYPE %in% c("Vulnerable") &
        PDX18_DEMOCHNG_TYPE %in% c("Not Changing") &
        PDX18_HOUSMRKT_TYPE %in% c("Accelerating") ~ "Early Type 1",
      PDX18_VULN_TYPE %in% c("Vulnerable") &
        PDX18_DEMOCHNG_TYPE %in% c("Not Changing") &
        PDX18_HOUSMRKT_TYPE %in% c("Adjacent") ~ "Susceptible",
      TRUE ~ NA_character_
    ))




  # RETURN ------------------------------------------------------------------

  return(model_pdx16)

}
