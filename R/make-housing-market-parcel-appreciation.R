
#' @title Make Housing Market Parcel Value Appreciation Data
#' @description Temporary description
#' @param present_use_key Tibble, Temporary description
#' @param single_family_criteria Tibble, Temporary description.
#' @param cpi Tibble, Temporary description.
#' @param parcel_value Tibble, Temporary description.
#' @param parcel_info_2005 Tibble, Temporary description.
#' @param parcel_info_2010 Tibble, Temporary description.
#' @param parcel_info_2018 Tibble, Temporary description.
#' @return a `tibble`
#' @export
make_housing_market_parcel_appr <- function(housing_market_parcel_value){


  housing_market_parcel_appr <- housing_market_parcel_value %>%
    dplyr::transmute(PIN,
              TAX_YEAR = stringr::str_c("YEAR_",TAX_YEAR,sep = ""),
              VALUE_TOTAL) %>%
    tidyr::spread(TAX_YEAR, VALUE_TOTAL) %>%
    dplyr::mutate(YEARS_05_10 = YEAR_2010/YEAR_2005 - 1,
           YEARS_05_18 = YEAR_2018/YEAR_2005 - 1,
           YEARS_10_18 = YEAR_2018/YEAR_2010 - 1) %>%
    tidyr::gather(TAX_YEAR_RANGE, APPRECIATION, tidyselect::matches("YEARS")) %>%
    dplyr::select(PIN, TAX_YEAR_RANGE, APPRECIATION)


  return(housing_market_parcel_appr)

}
