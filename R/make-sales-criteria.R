#' @title Make the Criteria for Real Property Sales
#' @description Make the criteria used to identify the real propert sales
#'   that will be used to identify housing market characteristics.
#' @return a named list
#' @export
make_sales_criteria <- function(){

  # The following criteria were determined by:
  #    1. joining sales_lut_key_list to parcel_sales
  #    2. iterating through each column, checking what % it was of the total
  #    3. putting together a list of criteria that seems to indicative of typical
  #       sales of either sf homes or condo units
  #
  #    * see R/check-sales-criteria.R

  sales_criteria <- list(
    "min_footage" = 300,
    "min_sale_price" = 25000,
    "principal_use" = c("RESIDENTIAL", "CONDOMINIUM"),
    "property_class" = c("Res-Improved property", "C/I-Condominium"),
    "property_type" = c("LAND WITH PREV USED BLDG", "LAND WITH NEW BUILDING", "Household, single family units", "Residential condominiums"),
    "sale_reason" = c("None","Other", "Trust", "Property Settlement", "Estate Settlement"),
    "buildings_on_property" = 2L,
    "date" = c(2005, 2010, 2018)
  )

  return(sales_criteria)
}
