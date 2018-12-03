make_parcel_value <- function(data_template, zip_path, file_path){

  # NeighborhoodChangeTypology::extract_file(zip_path, file_path)
  #
  # parcel_value_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
  #   janitor::clean_names(case = "screaming_snake")

  parcel_value_filter <- suppressWarnings(suppressMessages(readr::read_csv("extdata/osf/kc-assessor-parcels-2005-2010-2018/EXTR_ValueHistory_V.csv"))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::filter(TAX_STATUS %in% "T") %>% # The object takes up too much memory! Need to filter it down.
    dplyr::filter(TAX_YR %in% c(2005, 2010, 2018))


  parcel_value_long <- parcel_value_filter %>%
    dplyr::rename(VALUE_LAND = LAND_VAL,
                  VALUE_IMPROVEMENT = IMPS_VAL,
                  APPRAISED_VALUE_LAND = APPR_LAND_VAL,
                  APPRAISED_VALUE_IMPROVEMENT = APPR_IMPS_VAL,
                  APPRAISED_VALUE_IMPROVEMENT_INCR = APPR_IMP_INCR
                  ) %>%
    tidyr::gather(VARIABLE, ESTIMATE, VALUE_LAND, VALUE_IMPROVEMENT, APPRAISED_VALUE_LAND, APPRAISED_VALUE_IMPROVEMENT, APPRAISED_VALUE_IMPROVEMENT_INCR) %>%
    dplyr::rename_at(dplyr::vars(-dplyr::matches("VARIABLE|ESTIMATE")), dplyr::funs(stringr::str_c("META_",.)))

  p_val_ready <- data_template %>%
    full_join(parcel_value_long, by = c("VARIABLE",
                                       "ESTIMATE",
                                       ENDYEAR = "META_TAX_YR")) %>%
    mutate(SOURCE = "ASSESSOR",
           GEOGRAPHY_ID = make_pin(META_MAJOR, META_MINOR),
           GEOGRAPHY_ID_TYPE = "PIN",
           GEOGRAPHY_NAME = NA_character_,
           GEOGRAPHY_TYPE = "parcel",
           ENDYEAR = ENDYEAR,
           VARIABLE = VARIABLE,
           MEASURE_TYPE = "VALUE",
           ESTIMATE = ESTIMATE)


  parcel_value <- parcel_value_ready

  return(parcel_value)

}
