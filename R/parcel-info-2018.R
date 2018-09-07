
#' @title Make The General Tax Data For King County Parcels (2018)
#' @description Make the general tax data for King County parcels
#'   (Tax Year 2018, Calendar Year 2017)
#' @param zip_path Character, the file path of the archive file
#' @param file_path Character, the file path of the file that is to be extracted
#' @return a `tibble`
#' @export
make_parcel_info_2018 <- function(zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_info_2018 <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename(PRESENTUSE = PRESENT_USE,
                  SQFTLOT = SQ_FT_LOT)

  return(parcel_info_2018)
}
