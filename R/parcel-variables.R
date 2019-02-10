#' @title Make Parcel Variables
#' @description King County Assessor data
#' @param parcel_sales Tibble, Temporary description
#' @param sales_lut_key_list Tibble, Temporary description
#' @param sales_criteria Tibble, Temporary description
#' @param present_use_key Tibble, Temporary description
#' @param single_family_criteria Tibble, Temporary description
#' @param condo_unit_type_key Tibble, Temporary description
#' @param condo_criteria Tibble, Temporary description
#' @param cpi Tibble, Temporary description
#' @param parcel_info_2005 Tibble, Temporary description.
#' @param parcel_info_2010 Tibble, Temporary description.
#' @param parcel_info_2018 Tibble, Temporary description.
#' @param condo_info_2010 Tibble, Temporary description.
#' @param condo_info_2005 Tibble, Temporary description.
#' @param condo_info_2018 Tibble, Temporary description.
#' @param res_bldg_2005 Tibble, Temporary description
#' @param res_bldg_2010 Tibble, Temporary description
#' @param res_bldg_2018 Tibble, Temporary description
#' @return a `tibble`
#' @export

#' @rdname parcel-variables
#' @export
make_parcel_sales_variables <- function(parcel_sales,
                                        parcel_all_metadata,
                                        sales_lut_key_list,
                                        sales_criteria,
                                        single_family_criteria,
                                        condo_criteria,
                                        cpi,
                                        variable_template){


  # PREP: SALES -------------------------------------------------------------

  sales_prep <- parcel_sales %>%
    dplyr::transmute(SOURCE,
                     GEOGRAPHY_ID,
                     GEOGRAPHY_ID_TYPE,
                     GEOGRAPHY_NAME,
                     GEOGRAPHY_TYPE,
                     DATE_GROUP_ID,
                     DATE_BEGIN,
                     DATE_END,
                     DATE_RANGE,
                     DATE_RANGE_TYPE,
                     VARIABLE,
                     VARIABLE_SUBTOTAL,
                     VARIABLE_SUBTOTAL_DESC,
                     MEASURE_TYPE,
                     ESTIMATE,
                     MOE,
                     META_PRINCIPAL_USE = as.character(META_PRINCIPAL_USE),
                     META_PROPERTY_CLASS = as.character(META_PROPERTY_CLASS),
                     META_PROPERTY_TYPE = as.character(META_PROPERTY_TYPE),
                     META_SALE_INSTRUMENT = as.character(META_SALE_INSTRUMENT),
                     META_SALE_REASON = as.character(META_SALE_REASON)
    ) %>%
    dplyr::left_join(sales_lut_key_list$META_PRINCIPAL_USE, by = "META_PRINCIPAL_USE") %>%
    dplyr::left_join(sales_lut_key_list$META_PROPERTY_CLASS, by = "META_PROPERTY_CLASS") %>%
    dplyr::left_join(sales_lut_key_list$META_PROPERTY_TYPE, by = "META_PROPERTY_TYPE") %>%
    dplyr::left_join(sales_lut_key_list$META_SALE_INSTRUMENT, by = "META_SALE_INSTRUMENT") %>%
    dplyr::left_join(sales_lut_key_list$META_SALE_REASON, by = "META_SALE_REASON") %>%
    dplyr::transmute(SOURCE,
                     GEOGRAPHY_ID,
                     GEOGRAPHY_ID_TYPE,
                     GEOGRAPHY_NAME,
                     GEOGRAPHY_TYPE,
                     DATE_GROUP_ID,
                     DATE_BEGIN,
                     DATE_END,
                     DATE_RANGE,
                     DATE_RANGE_TYPE,
                     VARIABLE,
                     VARIABLE_SUBTOTAL,
                     VARIABLE_SUBTOTAL_DESC,
                     MEASURE_TYPE,
                     ESTIMATE,
                     MOE,
                     META_PRINCIPAL_USE = META_PRINCIPAL_USE_DESC,
                     META_PROPERTY_CLASS = META_PROPERTY_CLASS_DESC,
                     META_PROPERTY_TYPE = META_PROPERTY_TYPE_DESC,
                     META_SALE_INSTRUMENT = META_SALE_INSTRUMENT_DESC,
                     META_SALE_REASON = META_SALE_REASON_DESC
    )

  # ADJUST FOR INFLATION ----------------------------------------------------

  sales_2018_dollars <- sales_prep %>%
    dplyr::mutate(VARIABLE = "SP", # SP is my shorthand for "sale price"
                  ESTIMATE = purrr::map2_dbl(ESTIMATE, DATE_END, convert_to_2018_dollars))  # note: the original SALE_PRICE variable is dropped



  # ASSIGN ROLES BY CRITERIA ------------------------------------------------------

  # remove the DATE_* columns from parcel_all_metadata so that they don't conflic with the parcel DATE_* cols
  # exception: DATE_GROUP_ID
  parcel_all_metadata_by_year <- parcel_all_metadata %>%
    dplyr::select(-DATE_BEGIN,
                  -DATE_END,
                  -DATE_RANGE,
                  -DATE_RANGE_TYPE)

  sales_all_wide <- sales_2018_dollars %>%
    dplyr::inner_join(parcel_all_metadata_by_year, by = c("SOURCE",
                                                  "GEOGRAPHY_ID",
                                                  "GEOGRAPHY_ID_TYPE",
                                                  "GEOGRAPHY_NAME",
                                                  "GEOGRAPHY_TYPE",
                                                  "DATE_GROUP_ID")) %>%
    dplyr::select(-MOE) %>%
    dplyr::mutate(RNUM = dplyr::row_number()) %>%
    tidyr::spread(VARIABLE, ESTIMATE) %>%
    dplyr::mutate(SP_SQFT = dplyr::case_when(
      is.na(SP) ~ NA_real_,
      TRUE ~ round(SP/META_LIVING_SQ_FT, 2)
    )) %>% dplyr::select(-RNUM)

  sales_all_long <- sales_all_wide %>%
    dplyr::mutate(
      META_USE_TYPE_SF_LGL = META_PRESENT_USE %in% single_family_criteria$present_uses & META_PROPERTY_CATEGORY %in% "res",
      META_USE_TYPE_CONDO_LGL = META_CONDO_UNIT_TYPE %in% condo_criteria$condo_unit_types & META_PROPERTY_CATEGORY %in% "condo",
      META_USE_TYPE_LGL = META_USE_TYPE_SF_LGL | META_USE_TYPE_CONDO_LGL,
      META_PROP_CLASS_SF_LGL = META_PROPERTY_CLASS %in% "Res-Improved property",
      META_PROP_CLASS_CONDO_LGL = META_PROPERTY_CLASS %in% "C/I-Condominium",
      META_PROP_CLASS_LGL = META_PROP_CLASS_SF_LGL | META_PROP_CLASS_CONDO_LGL
    ) %>%
    dplyr::mutate(
      META_SQFT_LGL = !is.na(META_LIVING_SQ_FT) & META_LIVING_SQ_FT >= sales_criteria$min_footage,
      META_PRICE_LGL = !is.na(SP) & SP >= sales_criteria$min_sale_price,
      META_USE_LGL = META_PRINCIPAL_USE %in% sales_criteria$principal_use,
      META_PROP_TYPE_LGL = META_PROPERTY_TYPE %in% sales_criteria$property_type,
      META_REASON_LGL = META_SALE_REASON %in% sales_criteria$sale_reason,
      META_NBR_BLDG_LGL = META_NBR_BUILDINGS <= sales_criteria$buildings_on_property,
      META_YEAR_LGL = as.numeric(stringr::str_extract(DATE_GROUP_ID,"\\d{4}$")) %in% sales_criteria$date, # get the end year from DATE_GROUP_ID
      META_SALE_CRITERIA_LGL = META_SQFT_LGL & META_PRICE_LGL & META_USE_LGL &  META_PROP_TYPE_LGL & META_REASON_LGL & META_NBR_BLDG_LGL & META_YEAR_LGL
    ) %>%
    dplyr::mutate(META_SALE_MEETS_CRITERIA_SF_LGL = META_USE_TYPE_SF_LGL & META_PROP_CLASS_SF_LGL & META_SALE_CRITERIA_LGL,
                  META_SALE_MEETS_CRITERIA_CONDO_LGL = META_USE_TYPE_CONDO_LGL & META_PROP_CLASS_CONDO_LGL & META_SALE_CRITERIA_LGL,
                  META_SALE_MEETS_CRITERIA_ALL_LGL =  META_USE_TYPE_LGL & META_PROP_CLASS_LGL & META_SALE_CRITERIA_LGL) %>%
    tidyr::gather(VARIABLE, ESTIMATE, SP, SP_SQFT) %>%
    dplyr::mutate(MOE = NA_real_)



  check_sales_filter <- function(){
    sales_all_long %>%
      dplyr::select(META_PROPERTY_CATEGORY, dplyr::matches("LGL")) %>%
      dplyr::group_by(META_PROPERTY_CATEGORY) %>%
      skimr::skim()
  }

  # ASSIGN VARIABLE ROLES ---------------------------------------------------


  sale_var_roles <- sales_all_long %>%
    dplyr::mutate(VARIABLE_ALT_1 = "SF",
                  VARIABLE_ALT_2 = "CONDO",
                  VARIABLE_ALT_3 = "ALL") %>%
    tidyr::gather(VAR, VARIABLE_ALT, dplyr::starts_with("VARIABLE_ALT")) %>%
    dplyr::select(-VAR) %>%
    dplyr::mutate(INDICATOR = "SALE_PRICE",
                  MEASURE_TYPE = "VALUE") %>%
    dplyr::mutate(VARIABLE_ROLE = dplyr::case_when(
      VARIABLE_ALT %in% "SF" & META_SALE_MEETS_CRITERIA_SF_LGL ~ "include",
      VARIABLE_ALT %in% "CONDO" & META_SALE_MEETS_CRITERIA_CONDO_LGL ~ "include",
      VARIABLE_ALT %in% "ALL" & META_SALE_MEETS_CRITERIA_ALL_LGL ~ "include",
      TRUE ~ "omit"
    )) %>%
    tidyr::unite("VARIABLE", c("VARIABLE","VARIABLE_ALT"))


  # CREATE VARIABLE_DESC ----------------------------------------------------

  sale_var_desc <- sale_var_roles %>%
    dplyr::mutate(VARIABLE_DESC = stringr::str_replace(VARIABLE, "SP","SALE_PRICE")
    )


  # ARRANGE COLUMNS WITH TEMPLATE -------------------------------------------


  sales_reformat <- variable_template %>%
    dplyr::full_join(sale_var_desc,
                     by = c("SOURCE",
                            "GEOGRAPHY_ID",
                            "GEOGRAPHY_ID_TYPE",
                            "GEOGRAPHY_NAME",
                            "GEOGRAPHY_TYPE",
                            "DATE_GROUP_ID",
                            "DATE_BEGIN",
                            "DATE_END",
                            "DATE_RANGE",
                            "DATE_RANGE_TYPE",
                            "INDICATOR",
                            "VARIABLE",
                            "VARIABLE_DESC",
                            "VARIABLE_SUBTOTAL",
                            "VARIABLE_SUBTOTAL_DESC",
                            "VARIABLE_ROLE",
                            "MEASURE_TYPE",
                            "ESTIMATE",
                            "MOE"))


  parcel_sales_variables <- sales_reformat

  # RETURN ------------------------------------------------------------------

  return(parcel_sales_variables)


}

#' @rdname parcel-variables
#' @export
make_parcel_value_variables_part1 <- function(parcel_all_metadata,
                                        single_family_criteria,
                                        condo_criteria,
                                        cpi,
                                        parcel_value
){

  # CONVERT TO 2018 DOLLARS -------------------------------------------------




  # Prepare parcel_value:
  #
  #   1. where a parcel has multiple values for a given year, add them together
  #      (this will create huge values for properties with many buildings
  #       but they will filtererd out by single_family_criteria$buildings_on_property)
  #   2. convert to 2018 dollars (inflation adjustment)


  # extract the several fields (will be joined back in later steps)


  parcel_value_join_cols <- parcel_value %>%
    dplyr::select(SOURCE,
                  dplyr::starts_with("GEOGRAPHY"),
                  dplyr::starts_with("DATE"),
                  dplyr::starts_with("VARIABLE_"),
                  MEASURE_TYPE) %>%
    dplyr::distinct()


  parcel_value_all_variables <- parcel_value %>%
    dplyr::group_by(GEOGRAPHY_ID,
                    DATE_GROUP_ID,
                    DATE_END, # this field will be passed to convert_to_2018_dollars()
                    VARIABLE) %>%
    dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                     MOE = dplyr::first(MOE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(VARIABLE = stringr::str_c(VARIABLE,"_2018"),
                  ESTIMATE = purrr::map2_int(ESTIMATE, DATE_END, convert_to_2018_dollars)) %>%
    dplyr::left_join(parcel_value_join_cols, by = c("GEOGRAPHY_ID",
                                                    "DATE_GROUP_ID",
                                                    "DATE_END"))
  parcel_value_total_wide <- parcel_value_all_variables %>%
    tidyr::spread(VARIABLE, ESTIMATE) %>%
    dplyr::rename(ASSESSED_TOTAL_VALUE = VALUE_TOTAL_2018)

  parcel_value_ready <- parcel_value_total_wide


  # ASSIGN ROLES BY CRITERIA ------------------------------------------------

  # Create indicators telling the type of residential property as well as
  # whether the criteria have been met (see `single_family_criteria` or `condo_criteria`)

  p_metadata_and_value <- parcel_value_ready %>%
    dplyr::inner_join(parcel_all_metadata,
                      by = c("GEOGRAPHY_ID",
                             "DATE_GROUP_ID",
                             "SOURCE",
                             "GEOGRAPHY_ID_TYPE",
                             "GEOGRAPHY_NAME",
                             "GEOGRAPHY_TYPE",
                             "DATE_BEGIN",
                             "DATE_END",
                             "DATE_RANGE",
                             "DATE_RANGE_TYPE")
                      )

  p_criteria <- p_metadata_and_value %>%
    dplyr::mutate(META_USE_TYPE_SF_LGL = META_PRESENT_USE %in% single_family_criteria$present_uses & META_PROPERTY_CATEGORY %in% "res",
                  META_NBR_BLDG_LGL = META_NBR_BUILDINGS <= single_family_criteria$buildings_on_property,
                  META_SQ_FT = dplyr::between(META_LOT_SQ_FT,
                                              as.double(single_family_criteria$parcel_area$lower),
                                              as.double(units::set_units(single_family_criteria$parcel_area$upper,"ft^2"))),
                  META_IMPR_VALUE_SF_LGL = VALUE_IMPROVEMENT_2018 >= single_family_criteria$min_impr_value,
                  META_USE_TYPE_CONDO_LGL = META_CONDO_UNIT_TYPE %in% condo_criteria$condo_unit_types & META_PROPERTY_CATEGORY %in% "condo",
                  META_IMPR_VALUE_CONDO_LGL = VALUE_IMPROVEMENT_2018 >= condo_criteria$min_impr_value,
                  META_MEETS_CRITERIA_SF_LGL = META_USE_TYPE_SF_LGL & META_NBR_BLDG_LGL & META_SQ_FT & META_IMPR_VALUE_SF_LGL,
                  META_MEETS_CRITERIA_CONDO_LGL = META_USE_TYPE_CONDO_LGL & META_IMPR_VALUE_CONDO_LGL,
                  META_MEETS_CRITERIA_ALL_LGL = META_MEETS_CRITERIA_SF_LGL | META_MEETS_CRITERIA_CONDO_LGL,
                  META_HOME_TYPE = dplyr::case_when(  # create a single variable with the type of residence
                    META_MEETS_CRITERIA_CONDO_LGL ~ "condo",
                    META_MEETS_CRITERIA_SF_LGL ~ "single family",
                    TRUE ~ NA_character_)
    )

  # note: having complete records isn't necessary for an indicators but it may be useful to know anyway


# CREATE A NESTED TIBBLE ------------------------------------------------------

  p_nested <- p_criteria %>%
    dplyr::group_by(GEOGRAPHY_ID, DATE_GROUP_ID) %>%
    dplyr::arrange(dplyr::desc(ASSESSED_TOTAL_VALUE)) %>%
    dplyr::slice(1) %>% # take the highest value record for each PIN and year
    dplyr::ungroup() %>%
    tidyr::nest(-GEOGRAPHY_ID)


  # RETURN ------------------------------------------------------------------

  parcel_value_variables_nested <- p_nested

  return(parcel_value_variables_nested)


}

#' @rdname parcel-variables
#' @export
make_parcel_value_variables_part2 <- function(parcel_value_variables_part1
){

  # ASSIGN ROLES BY CRITERIA ------------------------------------------------

  year_list <- parcel_value_variables_part1 %>%
    tidyr::unnest() %>%
    dplyr::distinct(DATE_GROUP_ID) %>%
    dplyr::arrange(DATE_GROUP_ID) %>%
    dplyr::pull("DATE_GROUP_ID")

check_criteria <- function(x, year_list, criteria_col){

   all(year_list %in% x$DATE_GROUP_ID) & all(x[[criteria_col]])

}

check_criteria_safely <- purrr::safely(check_criteria, otherwise = FALSE, quiet = TRUE)

check_criteria_results <- function(...){

  safe_list <- check_criteria_safely(...)

  safe_list[["result"]]
}


  p_complete <- parcel_value_variables_part1 %>%
    dplyr::mutate(META_SF_COMPLETE_LGL = purrr::map_lgl(data,
                                          check_criteria_results,
                                          year_list = year_list,
                                          criteria_col = "META_MEETS_CRITERIA_SF_LGL"),
           META_CONDO_COMPLETE_LGL = purrr::map_lgl(data,
                                          check_criteria_results,
                                          year_list = year_list,
                                          criteria_col = "META_MEETS_CRITERIA_CONDO_LGL")
           ) %>%
    tidyr::unnest()

  rm_gc(parcel_value_variables_part1)

  # convert the data back to long format and select only the ASSESSED_TOTAL_VALUE rows

  p_long <- p_complete %>%
    tidyr::gather(VARIABLE, ESTIMATE, dplyr::matches("VALUE")) %>%
    dplyr::filter(VARIABLE %in% "ASSESSED_TOTAL_VALUE") %>%
    dplyr::mutate(VARIABLE = "ATV")  # ATV is shorthand for ASSESSED_TOTAL_VALUE

  rm_gc(p_complete)


  # RETURN ------------------------------------------------------------------

  return(p_long)


}

#' @rdname parcel-variables
#' @export
make_parcel_value_variables <- function(parcel_value_variables_part2,
                                        variable_template
){



  # ASSIGN VARIABLE ROLES ---------------------------------------------------

  p_var_roles <- parcel_value_variables_part2 %>%
    dplyr::mutate(VARIABLE_ALT_1 = "SF",
                  VARIABLE_ALT_2 = "CONDO",
                  VARIABLE_ALT_3 = "ALL") %>%
    tidyr::gather(VAR, VARIABLE_ALT, dplyr::starts_with("VARIABLE_ALT")) %>%
    dplyr::select(-VAR) %>%
    dplyr::mutate(INDICATOR = "ASSESSED_VALUE",
                  MEASURE_TYPE = "VALUE") %>%
    dplyr::mutate(VARIABLE_ROLE = dplyr::case_when(
      VARIABLE_ALT %in% "SF" & META_MEETS_CRITERIA_SF_LGL ~ "include",
      VARIABLE_ALT %in% "CONDO" & META_MEETS_CRITERIA_CONDO_LGL ~ "include",
      VARIABLE_ALT %in% "ALL" & META_MEETS_CRITERIA_ALL_LGL ~ "include",
      TRUE ~ "omit"
    )) %>%
    tidyr::unite("VARIABLE", c("VARIABLE","VARIABLE_ALT"))

 rm_gc(p_long)

  # CREATE VARIABLE_DESC ----------------------------------------------------

  p_var_desc <- p_var_roles %>%
    dplyr::mutate(VARIABLE_DESC = stringr::str_replace(VARIABLE, "ATV","ASSESSED_TOTAL_VALUE")
    )

  rm_gc(p_var_roles)

  # ARRANGE COLUMNS WITH TEMPLATE -------------------------------------------



  parcel_value_reformat <- variable_template %>%
    dplyr::full_join(p_var_desc,
                     by = c("SOURCE",
                            "GEOGRAPHY_ID",
                            "GEOGRAPHY_ID_TYPE",
                            "GEOGRAPHY_NAME",
                            "GEOGRAPHY_TYPE",
                            "DATE_GROUP_ID",
                            "DATE_BEGIN",
                            "DATE_END",
                            "DATE_RANGE",
                            "DATE_RANGE_TYPE",
                            "INDICATOR",
                            "VARIABLE",
                            "VARIABLE_DESC",
                            "VARIABLE_SUBTOTAL",
                            "VARIABLE_SUBTOTAL_DESC",
                            "VARIABLE_ROLE",
                            "MEASURE_TYPE",
                            "ESTIMATE",
                            "MOE"))

  rm_gc(p_var_desc)

  # CHECK DATA --------------------------------------------------------------


  check_parcel_value_variable <- function(){

    # This function shows all of the INDICATOR values and their INDICATOR_ROLEs.
    # If any NA's are showing up then something needs to be fixed

    parcel_value_reformat %>% dplyr::count(DATE_GROUP_ID, INDICATOR, VARIABLE, VARIABLE_DESC, VARIABLE_ROLE) %>% print(n=Inf)
  }


  # RETURN ------------------------------------------------------------------

  return(parcel_value_reformat)


}


#' @rdname parcel-variables
#' @export
make_parcel_value_variables_archive <- function(parcel_value_variables_nested,
                                        variable_template
){

  # ASSIGN ROLES BY CRITERIA ------------------------------------------------

  year_list <- parcel_value_variables_nested %>%
    tidyr::unnest() %>%
    dplyr::distinct(DATE_GROUP_ID) %>%
    dplyr::arrange(DATE_GROUP_ID) %>%
    dplyr::pull("DATE_GROUP_ID")

check_criteria <- function(x, year_list, criteria_col){

   all(year_list %in% x$DATE_GROUP_ID) & all(x[[criteria_col]])

}

check_criteria_safely <- purrr::safely(check_criteria, otherwise = FALSE, quiet = TRUE)

check_criteria_results <- function(...){

  safe_list <- check_criteria_safely(...)

  safe_list[["result"]]
}


  p_complete <- parcel_value_variables_nested %>%
    dplyr::mutate(META_SF_COMPLETE_LGL = purrr::map_lgl(data,
                                          check_criteria_results,
                                          year_list = year_list,
                                          criteria_col = "META_MEETS_CRITERIA_SF_LGL"),
           META_CONDO_COMPLETE_LGL = purrr::map_lgl(data,
                                          check_criteria_results,
                                          year_list = year_list,
                                          criteria_col = "META_MEETS_CRITERIA_CONDO_LGL")
           ) %>%
    tidyr::unnest()

  rm_gc(parcel_value_variables_nested)

  # convert the data back to long format and select only the ASSESSED_TOTAL_VALUE rows

  p_long <- p_complete %>%
    tidyr::gather(VARIABLE, ESTIMATE, dplyr::matches("VALUE")) %>%
    dplyr::filter(VARIABLE %in% "ASSESSED_TOTAL_VALUE") %>%
    dplyr::mutate(VARIABLE = "ATV")  # ATV is shorthand for ASSESSED_TOTAL_VALUE

  rm_gc(p_complete)

  # ASSIGN VARIABLE ROLES ---------------------------------------------------

  p_var_roles <- p_long %>%
    dplyr::mutate(VARIABLE_ALT_1 = "SF",
                  VARIABLE_ALT_2 = "CONDO",
                  VARIABLE_ALT_3 = "ALL") %>%
    tidyr::gather(VAR, VARIABLE_ALT, dplyr::starts_with("VARIABLE_ALT")) %>%
    dplyr::select(-VAR) %>%
    dplyr::mutate(INDICATOR = "ASSESSED_VALUE",
                  MEASURE_TYPE = "VALUE") %>%
    dplyr::mutate(VARIABLE_ROLE = dplyr::case_when(
      VARIABLE_ALT %in% "SF" & META_MEETS_CRITERIA_SF_LGL ~ "include",
      VARIABLE_ALT %in% "CONDO" & META_MEETS_CRITERIA_CONDO_LGL ~ "include",
      VARIABLE_ALT %in% "ALL" & META_MEETS_CRITERIA_ALL_LGL ~ "include",
      TRUE ~ "omit"
    )) %>%
    tidyr::unite("VARIABLE", c("VARIABLE","VARIABLE_ALT"))

 rm_gc(p_long)

  # CREATE VARIABLE_DESC ----------------------------------------------------

  p_var_desc <- p_var_roles %>%
    dplyr::mutate(VARIABLE_DESC = stringr::str_replace(VARIABLE, "ATV","ASSESSED_TOTAL_VALUE")
    )

  rm_gc(p_var_roles)

  # ARRANGE COLUMNS WITH TEMPLATE -------------------------------------------



  parcel_value_reformat <- variable_template %>%
    dplyr::full_join(p_var_desc,
                     by = c("SOURCE",
                            "GEOGRAPHY_ID",
                            "GEOGRAPHY_ID_TYPE",
                            "GEOGRAPHY_NAME",
                            "GEOGRAPHY_TYPE",
                            "DATE_GROUP_ID",
                            "DATE_BEGIN",
                            "DATE_END",
                            "DATE_RANGE",
                            "DATE_RANGE_TYPE",
                            "INDICATOR",
                            "VARIABLE",
                            "VARIABLE_DESC",
                            "VARIABLE_SUBTOTAL",
                            "VARIABLE_SUBTOTAL_DESC",
                            "VARIABLE_ROLE",
                            "MEASURE_TYPE",
                            "ESTIMATE",
                            "MOE"))

  rm_gc(p_var_desc)

  # CHECK DATA --------------------------------------------------------------


  check_parcel_value_variable <- function(){

    # This function shows all of the INDICATOR values and their INDICATOR_ROLEs.
    # If any NA's are showing up then something needs to be fixed

    parcel_value_reformat %>% dplyr::count(DATE_GROUP_ID, INDICATOR, VARIABLE, VARIABLE_DESC, VARIABLE_ROLE) %>% print(n=Inf)
  }


  # RETURN ------------------------------------------------------------------

  return(parcel_value_reformat)


}
