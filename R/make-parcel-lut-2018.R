
#' @title Make The Lookup Table for King County Tax Data (2018)
#' @description Make the lookup table for King County parcel tax data (2018).
#' @param dl_parcel_data NULL, a \code{\link[drake]{target}} whos purpose is only
#'   to make sure that the external parcel data file has been downloaded before
#'   `parcel_lut_2018` is created.
#' @return a `tibble`
#' @importFrom readr read_csv
#' @importFrom janitor clean_names
#' @export
make_parcel_lut_2018 <- function(dl_parcel_data){

  dl_parcel_data # ignore

  parcel_lut_2018 <- readr::read_csv("extdata/kc-assessor-parcels-2005-2010-2018/EXTR_LookUp_2018.csv") %>%
    janitor::clean_names(case = "screaming_snake")
}
