
#' @title Make The General Tax Data For King County Parcels (2010)
#' @description Make the general tax data for King County parcels
#'   (Tax Year 2010, Calendar Year 2009)
#' @param zip_path Character, the file path of the archive file
#' @param file_path Character, the file path of the file that is to be extracted
#' @return a `tibble`
#' @export
make_parcel_info_2010 <- function(zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_info_2010 <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake")

  return(parcel_info_2010)

}
