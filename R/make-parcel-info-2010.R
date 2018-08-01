
#' @title Make The General Tax Data For King County Parcels (2010)
#' @description Make the general tax data for King County parcels
#'   (Tax Year 2010, Calendar Year 2009)
#' @return a `tibble`
#' @importFrom readr read_csv
#' @importFrom janitor clean_names
#' @export
make_parcel_info_2010 <- function(dl_parcel_data){

  dl_parcel_data # ignore

  parcel_info_2010 <- readr::read_csv("extdata/kc-assessor-parcels-2005-2010-2018/EXTR_Parcel_2010.csv") %>%
    janitor::clean_names(case = "screaming_snake")
}
