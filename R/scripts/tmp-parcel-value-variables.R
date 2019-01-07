#' @rdname parcel-variables
#' @export
make_parcel_value_variables <- function(parcel_all_metadata,
                                        single_family_criteria,
                                        condo_criteria,
                                        cpi,
                                        parcel_value,
                                        variable_template
){


  # CONVERT TO 2018 DOLLARS -------------------------------------------------


  # Prepare parcel_value:
  #
  #   1. where a parcel has multiple values for a given year, add them together
  #      (this will create huge values for properties with many buildings
  #       but they will filtererd out by single_family_criteria$buildings_on_property)
  #   2. convert to 2018 dollars (inflation adjustment)

  convert_to_2018_dollars <- function(value, year){

    adj_rate <- cpi[as.character(2018)]/cpi[as.character(year)]

    as.integer(round(as.double(value * adj_rate) ,digits = -2) )
  }

  parcel_value_all_variables <- parcel_value %>%
    dplyr::group_by(SOURCE,
                    GEOGRAPHY_ID,
                    GEOGRAPHY_ID_TYPE,
                    GEOGRAPHY_NAME,
                    GEOGRAPHY_TYPE,
                    ENDYEAR,
                    VARIABLE) %>%
    dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                     MOE = dplyr::first(MOE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(VARIABLE = stringr::str_c(VARIABLE,"_2018"),
                  ESTIMATE = purrr::map2_int(ESTIMATE, ENDYEAR, convert_to_2018_dollars))

  parcel_value_total_wide <- parcel_value_all_variables %>%
    tidyr::spread(VARIABLE, ESTIMATE) %>%
    dplyr::mutate(VALUE_TOTAL_2018 = VALUE_LAND_2018 + VALUE_IMPROVEMENT_2018)

  parcel_value_ready <- parcel_value_total_wide


  # ASSIGN ROLES BY CRITERIA ------------------------------------------------

  # Create indicators telling the type of residential property as well as
  # whether the criteria have been met (see `single_family_criteria` or `condo_criteria`)

  message(paste0("The following process takes ~ 1 hour, 10 minutes - check back at: ",Sys.time() +4303))


  # tmp <- parcel_all_metadata %>%
  #   dplyr::inner_join(parcel_value_ready,  # warning: this is a filtering join
  #                    by = c("SOURCE",
  #                           "GEOGRAPHY_ID",
  #                           "GEOGRAPHY_ID_TYPE",
  #                           "GEOGRAPHY_NAME",
  #                           "GEOGRAPHY_TYPE",
  #                           "ENDYEAR")) %>%  # join the value history data
  #   tidyr::complete(GEOGRAPHY_ID, ENDYEAR)  %>%
  #   arrange(GEOGRAPHY_ID) %>%
  #   slice(1:10000) %>%
  #   group_by(GEOGRAPHY_ID, ENDYEAR)
  #
  # tmp2 <- tmp %>%
  # dplyr::mutate(META_USE_TYPE_SF_LGL = all(META_PRESENT_USE %in% single_family_criteria$present_uses & META_PROPERTY_CATEGORY %in% "res"),
  #                 META_NBR_BLDG_LGL = all(META_NBR_BUILDINGS <= single_family_criteria$buildings_on_property),
  #                 META_SQ_FT = all(dplyr::between(META_LOT_SQ_FT,
  #                                                  as.double(single_family_criteria$parcel_area$lower),
  #                                                  as.double(units::set_units(single_family_criteria$parcel_area$upper,"ft^2")))),
  #                 META_IMPR_VALUE_SF_LGL = all(VALUE_IMPROVEMENT_2018 >= single_family_criteria$min_impr_value),
  #                 META_USE_TYPE_CONDO_LGL = all(META_CONDO_UNIT_TYPE %in% condo_criteria$condo_unit_types & META_PROPERTY_CATEGORY %in% "condo"),
  #                 META_IMPR_VALUE_CONDO_LGL = all(VALUE_IMPROVEMENT_2018 >= condo_criteria$min_impr_value),
  #                 META_MEETS_CRITERIA_SF_LGL = META_USE_TYPE_SF_LGL & META_NBR_BLDG_LGL & META_SQ_FT & META_IMPR_VALUE_SF_LGL,
  #                 META_MEETS_CRITERIA_CONDO_LGL = META_USE_TYPE_CONDO_LGL & META_IMPR_VALUE_CONDO_LGL,
  #                 META_MEETS_CRITERIA_ALL_LGL = META_MEETS_CRITERIA_SF_LGL | META_MEETS_CRITERIA_CONDO_LGL
  #                 ) %>%
  #   dplyr::ungroup()
  #
  # tmp3 <- tmp2 %>%
  #   dplyr::mutate(META_HOME_TYPE = dplyr::case_when(  # create a single variable with the type of residence
  #     META_MEETS_CRITERIA_CONDO_LGL ~ "condo",
  #     META_MEETS_CRITERIA_SF_LGL ~ "single family",
  #     TRUE ~ NA_character_)) %>%
  #   dplyr::group_by(GEOGRAPHY_ID) %>% # by record
  #   dplyr::mutate(META_SF_COMPLETE_LGL = all(META_MEETS_CRITERIA_SF_LGL), # these records are complete (i.e., data for all three years)
  #                 META_CONDO_COMPLETE_LGL = all(META_MEETS_CRITERIA_CONDO_LGL))  %>%
  #   dplyr::group_by(GEOGRAPHY_ID, ENDYEAR) %>%
  #   dplyr::arrange(dplyr::desc(VALUE_TOTAL_2018)) %>%
  #   dplyr::slice(1) %>%
  #   dplyr::ungroup()

  p_grouped_id_year <- parcel_all_metadata %>%
    dplyr::inner_join(parcel_value_ready,  # warning: this is a filtering join
                     by = c("SOURCE",
                            "GEOGRAPHY_ID",
                            "GEOGRAPHY_ID_TYPE",
                            "GEOGRAPHY_NAME",
                            "GEOGRAPHY_TYPE",
                            "ENDYEAR")) %>%  # join the value history data
    tidyr::complete(GEOGRAPHY_ID, ENDYEAR)  %>%
    arrange(GEOGRAPHY_ID) %>%
    slice(1:10000) %>%
    group_by(GEOGRAPHY_ID, ENDYEAR)


  p_criteria <- p_grouped_id_year %>%
   dplyr::mutate(META_USE_TYPE_SF_LGL = all(META_PRESENT_USE %in% single_family_criteria$present_uses & META_PROPERTY_CATEGORY %in% "res"),
                  META_NBR_BLDG_LGL = all(META_NBR_BUILDINGS <= single_family_criteria$buildings_on_property),
                  META_SQ_FT = all(dplyr::between(META_LOT_SQ_FT,
                                                   as.double(single_family_criteria$parcel_area$lower),
                                                   as.double(units::set_units(single_family_criteria$parcel_area$upper,"ft^2")))),
                  META_IMPR_VALUE_SF_LGL = all(VALUE_IMPROVEMENT_2018 >= single_family_criteria$min_impr_value),
                  META_USE_TYPE_CONDO_LGL = all(META_CONDO_UNIT_TYPE %in% condo_criteria$condo_unit_types & META_PROPERTY_CATEGORY %in% "condo"),
                  META_IMPR_VALUE_CONDO_LGL = all(VALUE_IMPROVEMENT_2018 >= condo_criteria$min_impr_value),
                  META_MEETS_CRITERIA_SF_LGL = META_USE_TYPE_SF_LGL & META_NBR_BLDG_LGL & META_SQ_FT & META_IMPR_VALUE_SF_LGL,
                  META_MEETS_CRITERIA_CONDO_LGL = META_USE_TYPE_CONDO_LGL & META_IMPR_VALUE_CONDO_LGL,
                  META_MEETS_CRITERIA_ALL_LGL = META_MEETS_CRITERIA_SF_LGL | META_MEETS_CRITERIA_CONDO_LGL
                  ) %>%
    dplyr::ungroup()


  p_complete_records <- p_criteria %>%
    dplyr::mutate(META_HOME_TYPE = dplyr::case_when(  # create a single variable with the type of residence
      META_MEETS_CRITERIA_CONDO_LGL ~ "condo",
      META_MEETS_CRITERIA_SF_LGL ~ "single family",
      TRUE ~ NA_character_)) %>%
    dplyr::group_by(GEOGRAPHY_ID) %>% # by record
    dplyr::mutate(META_SF_COMPLETE_LGL = all(META_MEETS_CRITERIA_SF_LGL), # these records are complete (i.e., data for all three years)
                  META_CONDO_COMPLETE_LGL = all(META_MEETS_CRITERIA_CONDO_LGL))  %>%
    dplyr::group_by(GEOGRAPHY_ID, ENDYEAR) %>%
    dplyr::arrange(dplyr::desc(VALUE_TOTAL_2018)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  p_long <- p_complete_records %>%
    tidyr::gather(VARIABLE, ESTIMATE, matches("VALUE")) %>%
    dplyr::filter(VARIABLE %in% "VALUE_TOTAL_2018") %>%
    dplyr::mutate(MOE = NA_real_)


  # REFORMAT ----------------------------------------------------------------



  parcel_value_ready <- variable_template %>%
    dplyr::full_join(p_long,
                     by = c("SOURCE",
                            "GEOGRAPHY_ID",
                            "GEOGRAPHY_ID_TYPE",
                            "GEOGRAPHY_NAME",
                            "GEOGRAPHY_TYPE",
                            "ENDYEAR",
                            "VARIABLE",
                            "ESTIMATE",
                            "MOE")) %>%
    dplyr::mutate(INDICATOR = "VALUE",
                  VARIABLE_ROLE = dplyr::case_when(
                    META_SF_COMPLETE_LGL ~ "include",
                    META_CONDO_COMPLETE_LGL ~ "omit",  # note: this can be included at a later date
                    TRUE ~ "omit"
                  ),
                  MEASURE_TYPE = "VALUE")

  parcel_value_variables <- parcel_value_ready


  # RETURN ------------------------------------------------------------------

  return(parcel_value_variables)


}
