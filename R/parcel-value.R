
#' @title Make The Value History For King County Parcels
#' @description Download the history of Tax Assessor appraisals for
#'   all parcels in King County, WA (1998 - 2017).
#' @param zip_path Character, the file path of the archive file
#' @param file_path Character, the file path of the file that is to be extracted
#' @return a `tibble`
#' @export
make_parcel_value <- function(zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_value_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake")

  parcel_value_ready <- parcel_value_raw %>%
    dplyr::filter(TAX_STATUS %in% "T") %>%
    dplyr::filter(TAX_YR %in% c(2005, 2010, 2018)) %>%
    dplyr::transmute(PIN = make_pin(MAJOR, MINOR),
              VALUE_LAND = LAND_VAL,
              VALUE_IMPROVEMENT = IMPS_VAL,
              VALUE_TOTAL = VALUE_LAND + VALUE_IMPROVEMENT,
              TAX_YEAR = TAX_YR)

  parcel_value <- parcel_value_ready

  return(parcel_value)

}
