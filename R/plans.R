
# EXTERNAL DATA -----------------------------------------------------------

#' @title Get the External Data Plan
#' @description Use \code{\link[drake]{drake_plan}} to create the external data plan.
#' @return a `drake` plan
#' @export
#' @examples
#'
#' # Print the plan
#'
#' get_external_data_plan()
#'
#'
#' # Make the plan, load a target, print the target
#'
#' \dontrun{
#'
#' make(get_external_data_plan())
#'
#' loadd(kc_boundary)
#'
#' print(kc_boundary)
#' }
get_external_data_plan <- function(){
  drake::drake_plan(
    kc_boundary = make_kc_boundary(),
    parcel_boundaries = make_parcel_boundaries(),
    waterbodies = make_waterbodies(kc_boundary),
    census_tracts_2009 = make_census_tracts_2009(),
    census_tracts_2016 = make_census_tracts_2016(),
    acs_tables = make_acs_tables(),
    acs_data = make_acs_data(acs_tables),
    dl_parcel_data = make_dl_parcel_data(),
    parcel_value = make_parcel_value(dl_parcel_data),
    parcel_lut_2005 = make_parcel_lut_2005(dl_parcel_data),
    parcel_lut_2018 = make_parcel_lut_2018(dl_parcel_data),
    parcel_info_2005 = make_parcel_info_2005(dl_parcel_data),
    parcel_info_2010 = make_parcel_info_2010(dl_parcel_data),
    parcel_info_2018 = make_parcel_info_2018(dl_parcel_data),
    white_center_place = make_white_center_place(),
    cpi = make_cpi(),
    previous_typology = make_previous_typology()

  )
}

# INDICATOR PLAN ------------------------------------------------------

#' @title Get the Indicator Plan
#' @description Use \code{\link[drake]{drake_plan}} to create the indicator plan.
#' @return a `drake` plan
#' @export
#' @examples
#'
#' # Print the plan
#'
#' get_indicator_plan()
#'
#'
#' # Make the plan, load a target, print the target
#'
#' \dontrun{
#'
#' make(get_indicator_plan())
#'
#' loadd(acs_indicators)
#'
#' print(acs_indicators)
#' }
get_indicator_plan <- function(){
  drake::drake_plan(
    present_use_key = make_present_use_key(parcel_lut_2005, parcel_lut_2018),
    single_family_criteria = make_single_family_criteria(present_use_key),
    housing_market_parcels = make_housing_market_parcels(present_use_key,
                                                         single_family_criteria,
                                                         cpi,
                                                         parcel_value,
                                                         parcel_info_2005,
                                                         parcel_info_2010,
                                                         parcel_info_2018),
    acs_indicators = make_acs_indicators(acs_data, acs_tables),
    vulnerability_indicators = make_vulnerability_indicators(acs_indicators),
    demo_change_indicators = make_demo_change_indicators(acs_indicators)
  )
}


# TYPOLOGY PLAN ------------------------------------------------------

#' @title Get the Typology Plan
#' @description Use \code{\link[drake]{drake_plan}} to create the typology plan.
#' @return a `drake` plan
#' @export
#' @examples
#'
#' # Print the plan
#'
#' get_typology_plan()
#'
#'
#' # Make the plan, load a target, print the target
#'
#' \dontrun{
#'
#' make(get_indicator_plan())
#'
#' loadd(acs_indicators)
#'
#' print(acs_indicators)
#' }
get_typology_plan <- function(){
  drake::drake_plan(
    typology = make_typology(vulnerability_indicators,demo_change_indicators,previous_typology)
  )
}
