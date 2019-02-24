
#' @title Make Housing Market Parcel Value Data
#' @description Temporary description
#' @param present_use_key Tibble, Temporary description
#' @param condo_unit_type_key Tibble, Temporary description
#' @param single_family_criteria Tibble, Temporary description.
#' @param condo_criteria Tibble, Temporary description.
#' @param cpi Tibble, Temporary description.
#' @param parcel_value Tibble, Temporary description.
#' @param parcel_info_2005 Tibble, Temporary description.
#' @param parcel_info_2010 Tibble, Temporary description.
#' @param parcel_info_2018 Tibble, Temporary description.
#' @param condo_info_2010 Tibble, Temporary description.
#' @param condo_info_2005 Tibble, Temporary description.
#' @param condo_info_2018 Tibble, Temporary description.
#' @return a `tibble`
#' @export
make_housing_market_parcel_value <- function(present_use_key,
                                             condo_unit_type_key,
                                             single_family_criteria,
                                             condo_criteria,
                                             cpi,
                                             parcel_value,
                                             parcel_info_2005,
                                             parcel_info_2010,
                                             parcel_info_2018,
                                             condo_info_2005,
                                             condo_info_2010,
                                             condo_info_2018
){


  # Create the function that will prepare each year's parcel data,
# and the two lists to map over

prep_parcels  <- function(p, tax_year){

  p %>% dplyr::transmute(PIN = make_pin(MAJOR,MINOR),
                         PRESENT_USE = PRESENTUSE,
                         SQFT_LOT = units::set_units(SQFTLOT,"ft^2"),
                         TAX_YEAR = tax_year)

}

p <- list( parcel_info_2005,
           parcel_info_2010,
           parcel_info_2018)

tax_year <- list(2005,2010,2018)


# Run the function on all three years of parcel data and then bind the results together

p_all <- purrr::pmap_df(list(p, tax_year), prep_parcels) %>%
  dplyr::left_join(present_use_key, by = "PRESENT_USE") %>%
  dplyr::transmute(PIN,
                   TAX_YEAR,
                   PRESENT_USE = PRESENT_USE_DESC,
                   SQFT_LOT = units::set_units(SQFT_LOT,"ft^2"))



condo_list <- list(condo_info_2005,
                   condo_info_2010,
                   condo_info_2018)


prep_condos <- function(condo, tax_year){
  condo %>% dplyr::transmute(TAX_YEAR = tax_year,
                             PIN = make_pin(MAJOR, MINOR),
                             CONDO_UNIT_TYPE = UNITTYPE)
}

condo_all <- purrr::pmap_dfr(list(condo_list, tax_year), prep_condos) %>%
  dplyr::left_join(condo_unit_type_key, by = "CONDO_UNIT_TYPE") %>%
  dplyr::transmute(PIN,
                   TAX_YEAR,
                   CONDO_UNIT_TYPE = CONDO_UNIT_TYPE_DESC

  )


# Prepare parcel_value:
#
#   1. where a parcel has multiple values for a given year, add them together
#   2. convert to 2018 dollars (inflation adjustment)

convert_to_2018_dollars <- function(value, year){

  adj_rate <- cpi[as.character(2018)]/cpi[as.character(year)]

  as.integer(round(as.double(value * adj_rate) ,digits = -2) )
}

parcel_value_ready <- parcel_value %>%
  dplyr::group_by(PIN, TAX_YEAR) %>%
  dplyr::summarise_all(sum) %>%
  dplyr::ungroup() %>%
  tidyr::gather(TYPE, VALUE, tidyselect::matches("VALUE")) %>%
  dplyr::mutate(VALUE = purrr::map2_int(VALUE,TAX_YEAR, convert_to_2018_dollars)) %>%
  tidyr::spread(TYPE, VALUE)


# Create indicators telling the type of residential property as well as
# whether the criteria have been met (see `single_family_criteria` or `condo_criteria`)

message(paste0("The following process takes ~ 1 hour, 10 minutes - check back at: ",Sys.time() +4303))

p_lgl <- list(p_all, condo_all) %>%
  purrr::reduce(dplyr::bind_rows) %>%  # rowbind parcels and condo units
  dplyr::left_join(parcel_value_ready, by = c("PIN", "TAX_YEAR" )) %>%  # join the value history data
  tidyr::complete(PIN, TAX_YEAR) %>%   # make sure each PIN has all three tax years (even if there isn't data from each year)
  dplyr::group_by(PIN, TAX_YEAR) %>%  # by record by year
  dplyr::mutate(SF_LGL = all(PRESENT_USE %in% single_family_criteria$present_uses,
                             dplyr::between(SQFT_LOT,
                                            as.double(single_family_criteria$parcel_area$lower),
                                            as.double(units::set_units(single_family_criteria$parcel_area$upper,"ft^2"))),
                             VALUE_IMPROVEMENT >= single_family_criteria$min_impr_value),
                CONDO_LGL = all(CONDO_UNIT_TYPE %in% condo_criteria$condo_unit_types,
                                VALUE_IMPROVEMENT >= condo_criteria$min_impr_value)
                  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(HOME_TYPE = dplyr::case_when(  # create a single variable with the type of residence
      CONDO_LGL ~ "condo",
      SF_LGL ~ "single family",
      TRUE ~ NA_character_)) %>%
  dplyr::group_by(PIN) %>% # by record
  dplyr::mutate(SF_COMPLETE_LGL = all(SF_LGL), # these records are complete (i.e., data for all three years)
         CONDO_COMPLETE_LGL = all(CONDO_LGL))  %>%
  dplyr::group_by(PIN, TAX_YEAR) %>%
  dplyr::arrange(dplyr::desc(VALUE_TOTAL)) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()


# This helps show the breakdown of parcel by year by home type
check_counts <- function(x = p_lgl){
  p_lgl %>% dplyr::count(TAX_YEAR, HOME_TYPE)
}

housing_market_parcel_value <- p_lgl

return(housing_market_parcel_value)

}
