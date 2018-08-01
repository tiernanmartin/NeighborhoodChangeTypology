
#' @title Make The General Tax Data For King County Parcels (2018)
#' @description Make the general tax data for King County parcels
#'   (Tax Year 2018, Calendar Year 2017)
#' @return a `tibble`
#' @importFrom readr read_csv
#' @importFrom janitor clean_names
#' @export
make_parcel_info_2018 <- function(dl_parcel_data){

  dl_parcel_data # ignore

  parcel_info_2018 <- readr::read_csv("extdata/kc-assessor-parcels-2005-2010-2018/EXTR_Parcel_2018.csv") %>%
    janitor::clean_names(case = "screaming_snake")
}
