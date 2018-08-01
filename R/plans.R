
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
  parcel_info_2018 = make_parcel_info_2018(dl_parcel_data)

)
}
