
#' @title Make The General Tax Data For King County Condominium Units (2005)
#' @description Make the general tax data for condominium units in King County, Washington.
#' @param dl_parcel_data NULL, a \code{\link[drake]{target}} whos purpose is only
#'   to make sure that the external parcel data file has been downloaded before
#'   `condo_info_2005` is created.
#' @return a `tibble`
#' @importFrom readr read_csv
#' @importFrom janitor clean_names
#' @export
make_condo_info_2005 <- function(dl_parcel_data){

  dl_parcel_data # ignore

  condo_info_2005 <- readr::read_csv("extdata/kc-assessor-parcels-2005-2010-2018/EXTR_Condo_Unit_2005.csv") %>%
    janitor::clean_names(case = "screaming_snake")

  return(condo_info_2005)
}
