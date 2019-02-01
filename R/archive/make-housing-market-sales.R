#' @title Make Housing Market Sales Velocity Data
#' @description Temporary description
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
make_housing_market_sales <- function(parcel_sales,
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
                                      res_bldg_2018){

  # PREP: RES_BLDG ----------------------------------------------------------


tax_years <- c("2005","2010", "2017")

parcel_list <- list(parcel_info_2005, parcel_info_2010, parcel_info_2018)

prep_parcel <- function(x, tax_year){
  x %>% dplyr::transmute(PIN = NeighborhoodChangeTypology::make_pin(MAJOR, MINOR),
                         TAX_YEAR = tax_year,
                         PRESENT_USE = PRESENTUSE
  ) %>%
    dplyr::left_join(present_use_key, by = "PRESENT_USE") %>%
    dplyr::transmute(PIN,
                     TAX_YEAR,
                     PRESENT_USE = PRESENT_USE_DESC
    )
}


p_all <- purrr::pmap_dfr(list(parcel_list,tax_years), prep_parcel)

prep_res_bldg <- function(x, tax_year){

  x %>% dplyr::transmute(PIN = NeighborhoodChangeTypology::make_pin(MAJOR, MINOR),
                         PROPERTY_CATEGORY = "res",
                         BLDG_NBR,
                         SQ_FT = units::set_units(SQ_FT_TOT_LIVING,"ft^2"),
                         TAX_YEAR = tax_year) %>%
    dplyr::group_by(PIN, TAX_YEAR) %>%
    dplyr::mutate(NBR_BUILDINGS = dplyr::n()) %>%
    dplyr::ungroup()


}

res_bldg <- list(res_bldg_2005, res_bldg_2010, res_bldg_2018)



res_bldg_all <- purrr::pmap_df(list(res_bldg, tax_years), prep_res_bldg) %>%
  dplyr::left_join(p_all, by = c("PIN", "TAX_YEAR"))



# PREP: CONDO_UNIT --------------------------------------------------------

prep_condo_unit <- function(x, tax_year){

  x %>% dplyr::left_join(condo_unit_type_key, by = c(UNITTYPE = "CONDO_UNIT_TYPE")) %>%
    dplyr::transmute(PIN = NeighborhoodChangeTypology::make_pin(MAJOR, MINOR),
                     PROPERTY_CATEGORY = "condo",
                     CONDO_UNIT_TYPE = CONDO_UNIT_TYPE_DESC,
                     NBR_BUILDINGS = 1L,
                     SQ_FT = units::set_units(FOOTAGE,"ft^2"),
                     TAX_YEAR = tax_year)
}

condo_list <- list(condo_info_2005, condo_info_2010, condo_info_2018)

tax_years <- c("2005","2010", "2017")

condo_unit_all <- purrr::pmap_df(list(condo_list, tax_years), prep_condo_unit)



# PREP: PROPERTY ----------------------------------------------------------


# Note: this makes it possible to filter out PRESENT_USE or CONDO_UNIT_TYPE cases
#       that need to be excluded from the analysis (e.g., commerical condos)

prop_all <- dplyr::bind_rows(res_bldg_all,
                             condo_unit_all) %>%
  dplyr::mutate(PRESENT_USE = dplyr::case_when(
    PROPERTY_CATEGORY %in% "condo" ~ "not res",
    TRUE ~ PRESENT_USE
  ),
  CONDO_UNIT_TYPE = dplyr::case_when(
    PROPERTY_CATEGORY %in% "res" ~ "not condo",
    TRUE ~ CONDO_UNIT_TYPE
  )
  )



# PREP: SALES -------------------------------------------------------------

sales_prep <- parcel_sales %>%
  dplyr::transmute(PIN = NeighborhoodChangeTypology::make_pin(MAJOR, MINOR),
                   SALE_YEAR = stringr::str_extract(DOCUMENT_DATE,".{4}$"),
                   SALE_PRICE,
                   PRINCIPAL_USE = as.character(PRINCIPAL_USE),
                   PROPERTY_CLASS = as.character(PROPERTY_CLASS),
                   PROPERTY_TYPE = as.character(PROPERTY_TYPE),
                   SALE_INSTRUMENT = as.character(SALE_INSTRUMENT),
                   SALE_REASON = as.character(SALE_REASON)
  ) %>%
  dplyr::left_join(sales_lut_key_list$PRINCIPAL_USE, by = "PRINCIPAL_USE") %>%
  dplyr::left_join(sales_lut_key_list$PROPERTY_CLASS, by = "PROPERTY_CLASS") %>%
  dplyr::left_join(sales_lut_key_list$PROPERTY_TYPE, by = "PROPERTY_TYPE") %>%
  dplyr::left_join(sales_lut_key_list$SALE_INSTRUMENT, by = "SALE_INSTRUMENT") %>%
  dplyr::left_join(sales_lut_key_list$SALE_REASON, by = "SALE_REASON")

sales_prep_decoded <- sales_prep %>%
  dplyr::mutate(ROWNUM = dplyr::row_number()) %>%
  tidyr::gather(VAR, VALUE, -PIN, -ROWNUM) %>%
  dplyr::filter(stringr::str_detect(VAR, "SALE_PRICE|SALE_YEAR|DESC")) %>% # drop old columns
  dplyr::mutate(VAR = stringr::str_remove(VAR,"_DESC")) %>%
  tidyr::spread(VAR, VALUE) %>%
  dplyr::select(-ROWNUM)


# Note: this function can only convery sales after the year 1999
#       -- earlier years will return NA

convert_to_2018_dollars <- function(value, year){

  adj_rate <- cpi[as.character(2018)]/cpi[as.character(year)]

  as.integer(round(as.double(value) * adj_rate ,digits = -2) )
}

sales_2018_dollars <- sales_prep_decoded %>%
  dplyr::mutate(SALE_PRICE_2018 = purrr::map2_dbl(SALE_PRICE, SALE_YEAR, convert_to_2018_dollars))


# dplyr::filter BY CRITERIA ------------------------------------------------------

sales_all <- prop_all %>%
  dplyr::inner_join(sales_2018_dollars, by = "PIN") %>%
  dplyr::mutate(SALE_PRICE_SQFT = dplyr::case_when(
    is.na(SALE_PRICE_2018) ~ NA_real_,
    TRUE ~ round(SALE_PRICE_2018/1800, 2)
  ))  %>%
  dplyr::mutate(
    PRESENT_USE_LGL = PRESENT_USE %in% single_family_criteria$present_uses | PRESENT_USE %in% "not res",
    CONDO_UNIT_TYPE_LGL = CONDO_UNIT_TYPE %in% condo_criteria$condo_unit_types | CONDO_UNIT_TYPE %in% "not condo",
    SQFT_LGL = !is.na(SQ_FT) & SQ_FT >= sales_criteria$min_footage,
    PRICE_LGL = !is.na(SALE_PRICE_2018) & SALE_PRICE_2018 >= sales_criteria$min_sale_price,
    USE_LGL = PRINCIPAL_USE %in% sales_criteria$principal_use,
    PROP_CLASS_LGL = PROPERTY_CLASS %in% sales_criteria$property_class,
    PROP_TYPE_LGL = PROPERTY_TYPE %in% sales_criteria$property_type,
    REASON_LGL = SALE_REASON %in% sales_criteria$sale_reason,
    NBR_BLDG_LGL = NBR_BUILDINGS == sales_criteria$buildings_on_property,
    YEAR_LGL = SALE_YEAR %in% sales_criteria$date
  ) %>%
  dplyr::mutate(SALE_MEETS_CRITERIA_LGL =  PRESENT_USE_LGL & CONDO_UNIT_TYPE_LGL & SQFT_LGL & PRICE_LGL & USE_LGL & PROP_CLASS_LGL & PROP_TYPE_LGL & REASON_LGL & NBR_BLDG_LGL & YEAR_LGL)


check_sales_filter <- function(){
  sales_all %>%
    dplyr::select(PROPERTY_CATEGORY, dplyr::matches("LGL")) %>%
    dplyr::group_by(PROPERTY_CATEGORY) %>%
    skimr::skim()
}

sales <- sales_all %>%
  dplyr::filter(SALE_MEETS_CRITERIA_LGL) %>%
  dplyr::select(PIN,
                PROPERTY_CATEGORY,
                PRESENT_USE,
                CONDO_UNIT_TYPE,
                SQ_FT,
                NBR_BUILDINGS,
                SALE_PRINCIPAL_USE = PRINCIPAL_USE,
                SALE_PROPERTY_CLASS = PROPERTY_CLASS,
                SALE_PROPERTY_TYPE = PROPERTY_TYPE,
                SALE_INSTRUMENT,
                SALE_REASON,
                SALE_YEAR,
                SALE_PRICE_2018,
                SALE_PRICE_SQFT
  )

check_sales <- function(){

  sales %>%
    dplyr::group_by(PROPERTY_CATEGORY, SALE_YEAR) %>%
    dplyr::select(matches("SALE_PRICE")) %>%
    skimr::skim()

}


# RETURN ------------------------------------------------------------------

housing_market_sales <- sales

return(housing_market_sales)


}


