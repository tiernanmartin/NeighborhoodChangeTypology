#' @title Make the Criteria for Single Family Housing Parcels
#' @description Make the criteria used to identify the single family
#'   parcels that will be used to identify housing market characteristics.
#' @return a list
#' @importFrom units set_units
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom purrr pluck
#' @export
make_single_family_criteria <- function(present_use_key){

  sf_uses <- present_use_key %>%
    dplyr::filter(stringr::str_detect(PRESENT_USE_DESC, "Single Family")) %>% # this excludes `Vacant(Single-family)`
    purrr::pluck("PRESENT_USE_DESC")

  single_family_criteria <-
    list(
      "present_uses" = sf_uses,
      "tax_years" = c(2005, 2010, 2018),
      "remained_sf" = TRUE,
      "min_impr_value" = 10000,
      "parcel_area" = list(
        "upper" = units::set_units(5, "acre"),
        "lower" = units::set_units(3000, "ft^2")
      )
    )

  return(single_family_criteria)
}
