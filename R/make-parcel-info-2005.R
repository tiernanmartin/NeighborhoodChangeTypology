
#' @title Make The General Tax Data For King County Parcels (2005)
#' @description Make the general tax data for King County parcels
#'   (Tax Year 2005, Calendar Year 2004)
#' @param dl_parcel_data NULL, a \code{\link[drake]{target}} whos purpose is only
#'   to make sure that the external parcel data file has been downloaded before
#'   `parcel_info_2005` is created.
#' @return a `tibble`
#' @importFrom readr read_csv
#' @importFrom janitor clean_names
#' @export
make_parcel_info_2005 <- function(dl_parcel_data){

  dl_parcel_data # ignore

  parcel_info_2005 <- readr::read_csv("extdata/kc-assessor-parcels-2005-2010-2018/EXTR_Parcel_2005.csv") %>%
    janitor::clean_names(case = "screaming_snake")

  return(parcel_info_2005)
}
