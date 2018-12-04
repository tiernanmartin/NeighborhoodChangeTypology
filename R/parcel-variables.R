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
                                        sales_lut_key_list,
                                        sales_criteria,
                                        present_use_key,
                                        single_family_criteria,
                                        condo_unit_type_key,
                                        condo_criteria,
                                        cpi,
                                        parcel_info_2005,
                                        parcel_info_2010,
                                        parcel_info_2018,
                                        condo_info_2005,
                                        condo_info_2010,
                                        condo_info_2018,
                                        res_bldg_2005,
                                        res_bldg_2010,
                                        res_bldg_2018,
                                        model_table,
                                        variable_template){

  # PREP: RES_BLDG ----------------------------------------------------------


  tax_years <- c("2005","2010", "2017")

  parcel_list <- list(parcel_info_2005, parcel_info_2010, parcel_info_2018)

  prep_parcel <- function(x, tax_year){
    x %>% dplyr::transmute(SOURCE,
                           GEOGRAPHY_ID,
                           GEOGRAPHY_ID_TYPE,
                           GEOGRAPHY_NAME,
                           GEOGRAPHY_TYPE,
                           ENDYEAR,
                           META_TAX_YEAR = tax_year,
                           META_PRESENT_USE = META_PRESENTUSE
    ) %>%
      dplyr::left_join(present_use_key, by = "META_PRESENT_USE") %>%
      dplyr::transmute(SOURCE,
                       GEOGRAPHY_ID,
                       GEOGRAPHY_ID_TYPE,
                       GEOGRAPHY_NAME,
                       GEOGRAPHY_TYPE,
                       ENDYEAR,
                       META_TAX_YEAR = tax_year,
                       META_PRESENT_USE = META_PRESENT_USE_DESC
      )

  }


  p_all <- purrr::pmap_dfr(list(parcel_list,tax_years), prep_parcel)

  prep_res_bldg <- function(x, tax_year){

    x %>% dplyr::transmute(SOURCE,
                           GEOGRAPHY_ID,
                           GEOGRAPHY_ID_TYPE,
                           GEOGRAPHY_NAME,
                           GEOGRAPHY_TYPE,
                           ENDYEAR,
                           META_PROPERTY_CATEGORY = "res",
                           META_BLDG_NBR,
                           META_SQ_FT = units::set_units(META_SQ_FT_TOT_LIVING,"ft^2"),
                           META_TAX_YEAR = tax_year) %>%
      dplyr::group_by(GEOGRAPHY_ID, META_TAX_YEAR) %>%
      dplyr::mutate(META_NBR_BUILDINGS = dplyr::n()) %>%
      dplyr::ungroup()


  }

  res_bldg <- list(res_bldg_2005, res_bldg_2010, res_bldg_2018)



  res_bldg_all <- purrr::pmap_df(list(res_bldg, tax_years), prep_res_bldg)  %>%
    dplyr::left_join(p_all, by = c("SOURCE", "GEOGRAPHY_ID", "GEOGRAPHY_ID_TYPE", "GEOGRAPHY_NAME", "GEOGRAPHY_TYPE", "ENDYEAR", "META_TAX_YEAR"))



  # PREP: CONDO_UNIT --------------------------------------------------------

  prep_condo_unit <- function(x, tax_year){

    x %>% dplyr::left_join(condo_unit_type_key, by = c(META_UNIT_TYPE = "META_CONDO_UNIT_TYPE")) %>%
      dplyr::transmute(SOURCE,
                       GEOGRAPHY_ID,
                       GEOGRAPHY_ID_TYPE,
                       GEOGRAPHY_NAME,
                       GEOGRAPHY_TYPE,
                       ENDYEAR,
                       META_PROPERTY_CATEGORY = "condo",
                       META_CONDO_UNIT_TYPE = META_CONDO_UNIT_TYPE_DESC,
                       META_NBR_BUILDINGS = 1L,
                       META_SQ_FT = units::set_units(META_FOOTAGE,"ft^2"),
                       META_TAX_YEAR = tax_year)
  }

  condo_list <- list(condo_info_2005, condo_info_2010, condo_info_2018)

  tax_years <- c("2005","2010", "2017")

  condo_unit_all <- purrr::pmap_df(list(condo_list, tax_years), prep_condo_unit)



  # PREP: PROPERTY ----------------------------------------------------------


  # Note: this makes it possible to filter out PRESENT_USE or CONDO_UNIT_TYPE cases
  #       that need to be excluded from the analysis (e.g., commerical condos)

  prop_all <- dplyr::bind_rows(res_bldg_all,
                               condo_unit_all) %>%
    dplyr::mutate(META_PRESENT_USE = dplyr::case_when(
      META_PROPERTY_CATEGORY %in% "condo" ~ "not res",
      TRUE ~ META_PRESENT_USE
    ),
    META_CONDO_UNIT_TYPE = dplyr::case_when(
      META_PROPERTY_CATEGORY %in% "res" ~ "not condo",
      TRUE ~ META_CONDO_UNIT_TYPE
    )
    )

  # PREP: SALES -------------------------------------------------------------

  sales_prep <- parcel_sales %>%
    dplyr::transmute(SOURCE,
                     GEOGRAPHY_ID,
                     GEOGRAPHY_ID_TYPE,
                     GEOGRAPHY_NAME,
                     GEOGRAPHY_TYPE,
                     ENDYEAR,
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
                     ENDYEAR,
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


  # Note: this function can only convery sales after the year 1999
  #       -- earlier years will return NA

  convert_to_2018_dollars <- function(value, year){

    adj_rate <- cpi[as.character(2018)]/cpi[as.character(year)]

    as.integer(round(as.double(value) * adj_rate ,digits = -2) )
  }

  sales_2018_dollars <- sales_prep %>%
    dplyr::mutate(VARIABLE = "SALE_PRICE_2018",
                  ESTIMATE = purrr::map2_dbl(ESTIMATE, ENDYEAR, convert_to_2018_dollars))  # note: the original SALE_PRICE variable is dropped



  # ASSIGN ROLES BY CRITERIA ------------------------------------------------------

  sales_all <- sales_2018_dollars %>%
    dplyr::inner_join(prop_all, by = c("SOURCE", "GEOGRAPHY_ID", "GEOGRAPHY_ID_TYPE", "GEOGRAPHY_NAME", "GEOGRAPHY_TYPE", "ENDYEAR")) %>%
    dplyr::select(-MOE) %>%
    dplyr::mutate(RNUM = dplyr::row_number()) %>%
    tidyr::spread(VARIABLE, ESTIMATE) %>%
    dplyr::mutate(SALE_PRICE_2018_SQFT = dplyr::case_when(
      is.na(SALE_PRICE_2018) ~ NA_real_,
      TRUE ~ round(SALE_PRICE_2018/META_SQ_FT, 2)
    ))  %>%
    dplyr::mutate(
      META_PRESENT_USE_LGL = META_PRESENT_USE %in% single_family_criteria$present_uses | META_PRESENT_USE %in% "not res",
      META_CONDO_UNIT_TYPE_LGL = META_CONDO_UNIT_TYPE %in% condo_criteria$condo_unit_types | META_CONDO_UNIT_TYPE %in% "not condo",
      META_SQFT_LGL = !is.na(META_SQ_FT) & META_SQ_FT >= sales_criteria$min_footage,
      META_PRICE_LGL = !is.na(SALE_PRICE_2018) & SALE_PRICE_2018 >= sales_criteria$min_sale_price,
      META_USE_LGL = META_PRINCIPAL_USE %in% sales_criteria$principal_use,
      META_PROP_CLASS_LGL = META_PROPERTY_CLASS %in% sales_criteria$property_class,
      META_PROP_TYPE_LGL = META_PROPERTY_TYPE %in% sales_criteria$property_type,
      META_REASON_LGL = META_SALE_REASON %in% sales_criteria$sale_reason,
      META_NBR_BLDG_LGL = META_NBR_BUILDINGS == sales_criteria$buildings_on_property,
      META_YEAR_LGL = META_TAX_YEAR %in% sales_criteria$date
    ) %>%
    dplyr::mutate(META_SALE_MEETS_CRITERIA_LGL =  META_PRESENT_USE_LGL & META_CONDO_UNIT_TYPE_LGL & META_SQFT_LGL & META_PRICE_LGL & META_USE_LGL & META_PROP_CLASS_LGL & META_PROP_TYPE_LGL & META_REASON_LGL & META_NBR_BLDG_LGL & META_YEAR_LGL) %>%
    tidyr::gather(VARIABLE, ESTIMATE, SALE_PRICE_2018, SALE_PRICE_2018_SQFT) %>%
    dplyr::mutate(MOE = NA_real_)


  check_sales_filter <- function(){
    sales_all %>%
      dplyr::select(META_PROPERTY_CATEGORY, dplyr::matches("LGL")) %>%
      dplyr::group_by(META_PROPERTY_CATEGORY) %>%
      skimr::skim()
  }



  # ARRANGE COLUMNS WITH TEMPLATE -------------------------------------------


  sales_ready <- variable_template %>%
    dplyr::full_join(sales_all,
                     by = c("SOURCE",
                            "GEOGRAPHY_ID",
                            "GEOGRAPHY_ID_TYPE",
                            "GEOGRAPHY_NAME",
                            "GEOGRAPHY_TYPE",
                            "ENDYEAR",
                            "VARIABLE",
                            "VARIABLE_SUBTOTAL",
                            "VARIABLE_SUBTOTAL_DESC",
                            "MEASURE_TYPE",
                            "ESTIMATE",
                            "MOE")) %>%
    dplyr::mutate(INDICATOR = "SALE PRICE",
                  VARIABLE_ROLE = dplyr::case_when(
      META_SALE_MEETS_CRITERIA_LGL ~ "include",
      TRUE ~ "omit"
    ))

  parcel_sales_variables <- sales_ready

  # RETURN ------------------------------------------------------------------

  return(parcel_sales_variables)


}
