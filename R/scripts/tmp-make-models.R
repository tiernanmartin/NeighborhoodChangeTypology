#' @rdname models
make_model_coorev18 <- function(indicators_wide,
                             census_tracts_2016_trimmed){


  # FILTER INDICATORS -------------------------------------------------------

  coorev18_cols <- names(indicators_wide) %>% purrr::keep(~ stringr::str_detect(.x,"^COOREV18"))

  coorev18_inds <- indicators_wide %>% dplyr::select(dplyr::starts_with("GEOGRAPHY"),
                                                tidyselect::vars_select(names(.),coorev18_cols))


  # VULNERABILITY -----------------------------------------------------------

  coorev18_vuln_type_inds <- coorev18_inds %>%
    dplyr::select(dplyr::matches("GEOGRAPHY"),
                  dplyr::matches("COOREV18_VULN.+RELLGL_PCT")) %>%
    dplyr::group_by_at(dplyr::vars(dplyr::matches("GEOGRAPHY"))) %>%
    tidyr::nest() %>%
    dplyr::mutate(COOREV18_VULN_3OF4_LGL = purrr::map_lgl(data, ~ rowSums(.x, na.rm = TRUE) >= 3)) %>%
    dplyr::mutate(
      COOREV18_VULN_TYPE = dplyr::case_when(
        GEOGRAPHY_TYPE %in% "county" ~ NA_character_,
        is.na(COOREV18_VULN_3OF4_LGL) ~ NA_character_,
        COOREV18_VULN_3OF4_LGL  ~ "Vulnerable",
        TRUE ~ "Not Vulnerable"
      )
    ) %>%
    dplyr::select(-data)

  # DEMOGRAPHIC_CHANGE ------------------------------------------------------


  is_demochng_white_highed <- function(x){
    x$COOREV18_DEMOCHNG_WHITE_CHNGLGL_PCT_2007_2011_TO_2013_2017 &
      x$COOREV18_DEMOCHNG_HIGHED_CHNGLGL_PCT_2007_2011_TO_2013_2017
  }

  coorev18_demochng_type_inds <- coorev18_inds %>%
    dplyr::select(dplyr::matches("GEOGRAPHY"),
                  dplyr::matches("COOREV18_DEMOCHNG.+CHNGLGL")) %>%
    dplyr::group_by_at(dplyr::vars(dplyr::matches("GEOGRAPHY"))) %>%
    tidyr::nest() %>%
    dplyr::mutate(COOREV18_DEMOCHNG_3OF4_LGL = purrr::map_lgl(data, ~ rowSums(.x, na.rm = TRUE) >= 3)) %>%
    dplyr::mutate(COOREV18_DEMOCHNG_WHITEED_LGL = purrr::map_lgl(data, is_demochng_white_highed)) %>%
    dplyr::mutate(
      COOREV18_DEMOCHNG_TYPE = dplyr::case_when(
        GEOGRAPHY_TYPE %in% "county" ~ NA_character_,
        is.na(COOREV18_DEMOCHNG_3OF4_LGL) | is.na(COOREV18_DEMOCHNG_WHITEED_LGL) ~ NA_character_,
        COOREV18_DEMOCHNG_3OF4_LGL ~ "Changing",
        COOREV18_DEMOCHNG_WHITEED_LGL ~ "Changing (White & High Ed.)",
        TRUE ~ "Not Changing"
      )
    ) %>%
    dplyr::select(-data)



  # HOUSING_MARKET ----------------------------------------------------------

  # create the type indicators needed to determine the housing market type
  # using the singlefamily-multifamily split

  coorev18_housmrkt_type_inds <- coorev18_inds %>%
    dplyr::transmute( GEOGRAPHY_ID,
                      GEOGRAPHY_ID_TYPE,
                      GEOGRAPHY_NAME,
                      GEOGRAPHY_TYPE,
                      COOREV18_HOUSMRKT_SF_LGL = !COOREV18_HOUSMRKT_MFUNITS_RELLGL_PCT_2013_2017,
                      COOREV18_HOUSMRKT_MF_LGL = COOREV18_HOUSMRKT_MFUNITS_RELLGL_PCT_2013_2017,
                      COOREV18_HOUSMRKT_MF_LORENT_2000_LGL =
                        COOREV18_HOUSMRKT_MF_LGL &
                        (!COOREV18_HOUSMRKT_RENTPRICE_RELLGL_MED_2000_2000),
                      COOREV18_HOUSMRKT_MF_LORENT_2017_LGL =
                        COOREV18_HOUSMRKT_MF_LGL &
                        !COOREV18_HOUSMRKT_RENTPRICE_RELLGL_MED_2013_2017,
                      COOREV18_HOUSMRKT_MF_HIRENT_2000_LGL =
                        COOREV18_HOUSMRKT_MF_LGL &
                        COOREV18_HOUSMRKT_RENTPRICE_RELLGL_MED_2000_2000,
                      COOREV18_HOUSMRKT_MF_HIRENT_2017_LGL =
                        COOREV18_HOUSMRKT_MF_LGL &
                        COOREV18_HOUSMRKT_RENTPRICE_RELLGL_MED_2013_2017,
                      COOREV18_HOUSMRKT_MF_LORENT_2000_HIRENT_2017_LGL =
                        COOREV18_HOUSMRKT_MF_LGL &
                        COOREV18_HOUSMRKT_RENTPRICE_RELCHNGLGL_MED_2000_2000_TO_2013_2017,
                      COOREV18_HOUSMRKT_MF_LORENTAPPR_2010_2017_LGL =
                        COOREV18_HOUSMRKT_MF_LGL &
                        (!COOREV18_HOUSMRKT_RENTPRICE_CHNGLGL_MED_2006_2010_TO_2013_2017),
                      COOREV18_HOUSMRKT_MF_HIRENTAPPR_2000_2017_LGL =
                        COOREV18_HOUSMRKT_MF_LGL &
                        COOREV18_HOUSMRKT_RENTPRICE_CHNGLGL_MED_2000_2000_TO_2013_2017,
                      COOREV18_HOUSMRKT_MF_HIRENTHIAPPR_LGL =
                        COOREV18_HOUSMRKT_MF_LGL &
                        (COOREV18_HOUSMRKT_MF_HIRENT_2017_LGL |
                        COOREV18_HOUSMRKT_MF_HIRENTAPPR_2000_2017_LGL),
                      COOREV18_HOUSMRKT_SF_LOVAL_2005_LGL =
                        COOREV18_HOUSMRKT_SF_LGL &
                        ((!COOREV18_HOUSMRKT_SALEPRICE_RELLGL_MED_2005_2005) |
                        (!COOREV18_HOUSMRKT_SALEPRICE_RELLGL_MED_2005_2005)),
                      COOREV18_HOUSMRKT_SF_LOVAL_2018_LGL =
                        COOREV18_HOUSMRKT_SF_LGL &
                        ((!COOREV18_HOUSMRKT_HOMEVALKC_RELLGL_MED_2018_2018) |
                        (!COOREV18_HOUSMRKT_SALEPRICE_RELLGL_MED_2018_2018)),
                      COOREV18_HOUSMRKT_SF_HIVAL_2005_LGL =
                        COOREV18_HOUSMRKT_SF_LGL &
                        (COOREV18_HOUSMRKT_HOMEVALKC_RELLGL_MED_2005_2005 |
                        COOREV18_HOUSMRKT_SALEPRICE_RELLGL_MED_2005_2005),
                      COOREV18_HOUSMRKT_SF_HIVAL_2018_LGL =
                        COOREV18_HOUSMRKT_SF_LGL &
                        (COOREV18_HOUSMRKT_HOMEVALKC_RELLGL_MED_2018_2018 |
                        COOREV18_HOUSMRKT_SALEPRICE_RELLGL_MED_2018_2018),
                      COOREV18_HOUSMRKT_SF_LOVAL_2005_HIVAL_2018_LGL =
                        COOREV18_HOUSMRKT_SF_LGL &
                        (COOREV18_HOUSMRKT_HOMEVALKC_RELCHNGLGL_MED_2005_2005_TO_2018_2018 |
                        COOREV18_HOUSMRKT_SALEPRICE_RELCHNGLGL_MED_2005_2005_TO_2018_2018),
                      COOREV18_HOUSMRKT_SF_LOAPPR_2010_2018_LGL =
                        COOREV18_HOUSMRKT_SF_LGL &
                        ((!COOREV18_HOUSMRKT_HOMEVALKC_CHNGLGL_MED_2010_2010_TO_2018_2018) |
                        (!COOREV18_HOUSMRKT_SALEPRICE_CHNGLGL_MED_2010_2010_TO_2018_2018)),
                      COOREV18_HOUSMRKT_SF_HIAPPR_2010_2018_LGL =
                        COOREV18_HOUSMRKT_SF_LGL &
                        (COOREV18_HOUSMRKT_HOMEVALKC_CHNGLGL_MED_2010_2010_TO_2018_2018 |
                        COOREV18_HOUSMRKT_SALEPRICE_CHNGLGL_MED_2010_2010_TO_2018_2018),
                      COOREV18_HOUSMRKT_SF_HIAPPR_2005_2018_LGL =
                        COOREV18_HOUSMRKT_SF_LGL &
                        (COOREV18_HOUSMRKT_HOMEVALKC_CHNGLGL_MED_2005_2005_TO_2018_2018 |
                        COOREV18_HOUSMRKT_SALEPRICE_CHNGLGL_MED_2005_2005_TO_2018_2018),
                      COOREV18_HOUSMRKT_SF_HIVALHIAPPR_LGL =
                        COOREV18_HOUSMRKT_SF_LGL &
                        (COOREV18_HOUSMRKT_SF_HIVAL_2018_LGL |
                        COOREV18_HOUSMRKT_SF_HIAPPR_2010_2018_LGL),
                      COOREV18_HOUSMRKT_SF_HISALERATE_2018_LGL = COOREV18_HOUSMRKT_SALERATE_RELLGL_PCT_2016_2018
    )


  # the spillover indicator requires a spatial join

  coorev18_housmrkt_type_inds_sf <- coorev18_housmrkt_type_inds %>%
    dplyr::left_join(census_tracts_2016_trimmed,
                     by = c("GEOGRAPHY_ID",
                            "GEOGRAPHY_ID_TYPE",
                            "GEOGRAPHY_NAME",
                            "GEOGRAPHY_TYPE")) %>%
    sf::st_sf()

  coorev18_housmrkt_type_inds_hivalappr_only_sf <- coorev18_housmrkt_type_inds_sf %>%
    dplyr::transmute(TOUCHES_HIVALHIAPPR = COOREV18_HOUSMRKT_MF_HIRENTHIAPPR_LGL | COOREV18_HOUSMRKT_SF_HIVALHIAPPR_LGL)

  coorev18_housmrkt_type_touchhivalhiappr <- coorev18_housmrkt_type_inds_sf %>%
    sf::st_join(coorev18_housmrkt_type_inds_hivalappr_only_sf, sf::st_touches) %>%
    sf::st_set_geometry(NULL) %>%
    tibble::as_tibble() %>%
    dplyr::group_by_at(dplyr::vars(-dplyr::matches("TOUCHES_HIVALHIAPPR"))) %>%
    tidyr::nest() %>%
    dplyr::mutate(COOREV18_HOUSMRKT_TOUCHHIVALHIAPPR_LGL = purrr::map_lgl(data, ~ any(.x$TOUCHES_HIVALHIAPPR))) %>%
    dplyr::select(-data)

  coorev18_housmrkt_type <-  coorev18_housmrkt_type_touchhivalhiappr %>%
    dplyr::mutate(COOREV18_HOUSMRKT_MF_ADJACENT_LGL =
                    COOREV18_HOUSMRKT_MF_LORENT_2017_LGL &
                    COOREV18_HOUSMRKT_MF_LORENTAPPR_2010_2017_LGL &
                    COOREV18_HOUSMRKT_TOUCHHIVALHIAPPR_LGL)  %>%
    dplyr::mutate(COOREV18_HOUSMRKT_MF_ACCELERATING_LGL =
                    COOREV18_HOUSMRKT_MF_LORENT_2017_LGL &
                    COOREV18_HOUSMRKT_MF_HIRENTAPPR_2000_2017_LGL) %>%
    dplyr::mutate(COOREV18_HOUSMRKT_MF_APPRECIATED_LGL =
                    COOREV18_HOUSMRKT_MF_LORENT_2000_HIRENT_2017_LGL &
                    COOREV18_HOUSMRKT_MF_HIRENTAPPR_2000_2017_LGL) %>%
    dplyr::mutate(COOREV18_HOUSMRKT_MF_LONGTERM_LOW_LGL =
                    COOREV18_HOUSMRKT_MF_LORENT_2000_LGL &
                    COOREV18_HOUSMRKT_MF_LORENT_2017_LGL &
                    COOREV18_HOUSMRKT_MF_LORENTAPPR_2010_2017_LGL) %>%
    dplyr::mutate(COOREV18_HOUSMRKT_MF_LONGTERM_HI_LGL =
                    COOREV18_HOUSMRKT_MF_HIRENT_2000_LGL &
                    COOREV18_HOUSMRKT_MF_HIRENT_2017_LGL) %>%
    dplyr::mutate(COOREV18_HOUSMRKT_SF_ADJACENT_LGL =
                    COOREV18_HOUSMRKT_SF_LOVAL_2018_LGL &
                    COOREV18_HOUSMRKT_SF_LOAPPR_2010_2018_LGL &
                    COOREV18_HOUSMRKT_TOUCHHIVALHIAPPR_LGL)  %>%
    dplyr::mutate(COOREV18_HOUSMRKT_SF_ACCELERATING_LGL =
                    COOREV18_HOUSMRKT_SF_LOVAL_2018_LGL &
                    (COOREV18_HOUSMRKT_SF_HIAPPR_2010_2018_LGL |
                       COOREV18_HOUSMRKT_SF_HISALERATE_2018_LGL)) %>%
    dplyr::mutate(COOREV18_HOUSMRKT_SF_APPRECIATED_LGL =
                    COOREV18_HOUSMRKT_SF_LOVAL_2005_HIVAL_2018_LGL &
                    COOREV18_HOUSMRKT_SF_HIAPPR_2005_2018_LGL) %>%
    dplyr::mutate(COOREV18_HOUSMRKT_SF_LONGTERM_LOW_LGL =
                    COOREV18_HOUSMRKT_SF_LOVAL_2005_LGL &
                    COOREV18_HOUSMRKT_SF_LOVAL_2018_LGL &
                    COOREV18_HOUSMRKT_SF_LOAPPR_2010_2018_LGL) %>%
    dplyr::mutate(COOREV18_HOUSMRKT_SF_LONGTERM_HI_LGL =
                    COOREV18_HOUSMRKT_SF_HIVAL_2005_LGL &
                    COOREV18_HOUSMRKT_SF_HIVAL_2018_LGL) %>%
    dplyr::mutate(COOREV18_HOUSMRKT_MF_TYPE = dplyr::case_when(
      GEOGRAPHY_TYPE %in% c("county","community") ~ NA_character_,
      COOREV18_HOUSMRKT_MF_APPRECIATED_LGL ~ "Appreciated",
      COOREV18_HOUSMRKT_MF_ACCELERATING_LGL ~ "Accelerating",
      COOREV18_HOUSMRKT_MF_ADJACENT_LGL ~ "Adjacent",
      COOREV18_HOUSMRKT_MF_LONGTERM_LOW_LGL ~ "Long-term Low Cost",
      COOREV18_HOUSMRKT_MF_LONGTERM_HI_LGL ~ "Long-term High Cost",
      TRUE ~ "Other"
    )) %>%
    dplyr::mutate(COOREV18_HOUSMRKT_SF_TYPE = dplyr::case_when(
      GEOGRAPHY_TYPE %in% c("county","community") ~ NA_character_,
      COOREV18_HOUSMRKT_SF_APPRECIATED_LGL ~ "Appreciated",
      COOREV18_HOUSMRKT_SF_ACCELERATING_LGL ~ "Accelerating",
      COOREV18_HOUSMRKT_SF_ADJACENT_LGL ~ "Adjacent",
      COOREV18_HOUSMRKT_SF_LONGTERM_LOW_LGL ~ "Long-term Low Cost",
      COOREV18_HOUSMRKT_SF_LONGTERM_HI_LGL ~ "Long-term High Cost",
      TRUE ~ "Other"
    ))



  # TYPOLOGY ----------------------------------------------------------------


  model_coorev18 <- list(coorev18_vuln_type_inds,
     coorev18_demochng_type_inds,
     coorev18_housmrkt_type) %>%
    purrr::reduce(dplyr::full_join) %>%
    dplyr::mutate(COOREV18_MF_TYPE = dplyr::case_when(
      GEOGRAPHY_TYPE %in% c("county","community") ~ NA_character_,
      COOREV18_VULN_TYPE %in% c("Not Vulnerable") &
        COOREV18_DEMOCHNG_TYPE %in% c("Changing (White & High Ed.)") &
        COOREV18_HOUSMRKT_MF_TYPE %in% c("Appreciated") ~ "Continued Loss",
      COOREV18_VULN_TYPE %in% c("Vulnerable") &
        COOREV18_DEMOCHNG_TYPE %in% c("Changing") &
        COOREV18_HOUSMRKT_MF_TYPE %in% c("Appreciated") ~ "Late",
      COOREV18_VULN_TYPE %in% c("Vulnerable") &
        COOREV18_DEMOCHNG_TYPE %in% c("Changing") &
        COOREV18_HOUSMRKT_MF_TYPE %in% c("Accelerating") ~ "Dynamic",
      COOREV18_VULN_TYPE %in% c("Vulnerable") &
        COOREV18_DEMOCHNG_TYPE %in% c("Changing") &
        COOREV18_HOUSMRKT_MF_TYPE %in% c("Adjacent") ~ "Early Type 2",
      COOREV18_VULN_TYPE %in% c("Vulnerable") &
        COOREV18_DEMOCHNG_TYPE %in% c("Not Changing") &
        COOREV18_HOUSMRKT_MF_TYPE %in% c("Accelerating") ~ "Early Type 1",
      COOREV18_VULN_TYPE %in% c("Vulnerable") &
        COOREV18_DEMOCHNG_TYPE %in% c("Not Changing") &
        COOREV18_HOUSMRKT_MF_TYPE %in% c("Adjacent") ~ "Susceptible",
      TRUE ~ NA_character_
    )) %>%
    dplyr::mutate(COOREV18_SF_TYPE = dplyr::case_when(
      GEOGRAPHY_TYPE %in% c("county","community") ~ NA_character_,
      COOREV18_VULN_TYPE %in% c("Not Vulnerable") &
        COOREV18_DEMOCHNG_TYPE %in% c("Changing (White & High Ed.)") &
        COOREV18_HOUSMRKT_SF_TYPE %in% c("Appreciated") ~ "Continued Loss",
      COOREV18_VULN_TYPE %in% c("Vulnerable") &
        COOREV18_DEMOCHNG_TYPE %in% c("Changing") &
        COOREV18_HOUSMRKT_SF_TYPE %in% c("Appreciated") ~ "Late",
      COOREV18_VULN_TYPE %in% c("Vulnerable") &
        COOREV18_DEMOCHNG_TYPE %in% c("Changing") &
        COOREV18_HOUSMRKT_SF_TYPE %in% c("Accelerating") ~ "Dynamic",
      COOREV18_VULN_TYPE %in% c("Vulnerable") &
        COOREV18_DEMOCHNG_TYPE %in% c("Changing") &
        COOREV18_HOUSMRKT_SF_TYPE %in% c("Adjacent") ~ "Early Type 2",
      COOREV18_VULN_TYPE %in% c("Vulnerable") &
        COOREV18_DEMOCHNG_TYPE %in% c("Not Changing") &
        COOREV18_HOUSMRKT_SF_TYPE %in% c("Accelerating") ~ "Early Type 1",
      COOREV18_VULN_TYPE %in% c("Vulnerable") &
        COOREV18_DEMOCHNG_TYPE %in% c("Not Changing") &
        COOREV18_HOUSMRKT_SF_TYPE %in% c("Adjacent") ~ "Susceptible",
      TRUE ~ NA_character_
    ))




  # RETURN ------------------------------------------------------------------

  return(model_coorev18)

}
