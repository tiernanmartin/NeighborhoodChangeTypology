
#' @title Make The Value History For King County Parcels
#' @description Download the history of Tax Assessor appraisals for
#'   all parcels in King County, WA (1998 - 2017).
#' @param dl_parcel_data NULL, a \code{\link[drake]{target}} whos purpose is only
#'   to make sure that the external parcel data file has been downloaded before
#'   `parcel_lut_2005` is created.
#' @return a `tibble`
#' @importFrom janitor clean_names
#' @export
make_parcel_value <- function(dl_parcel_data){

  dl_parcel_data # ignore

  parcel_value_raw <- readr::read_csv("extdata/kc-assessor-parcels-2005-2010-2018/EXTR_ValueHistory_V_2018.csv") %>%
    janitor::clean_names(case = "screaming_snake")

  parcel_value_ready <- parcel_value_raw %>%
    filter(TAX_STATUS %in% "T") %>%
    filter(TAX_YR %in% c(2005, 2010, 2018)) %>%
    transmute(PIN = make_pin(MAJOR, MINOR),
              VALUE_LAND = LAND_VAL,
              VALUE_IMPROVEMENT = IMPS_VAL,
              VALUE_TOTAL = VALUE_LAND + VALUE_IMPROVEMENT,
              TAX_YEAR = TAX_YR)

  parcel_value <- parcel_value_ready

}
