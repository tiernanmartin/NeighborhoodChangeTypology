
#' @title Make The Lookup Table for King County Tax Data (2005)
#' @description Make the lookup table for King County parcel tax data (2005).
#' @param zip_path Character, the file path of the archive file
#' @param file_path Character, the file path of the file that is to be extracted
#' @return a `tibble`
#' @export
make_parcel_lut_2005 <- function(zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_lut_2005 <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename(LU_TYPE = LUTYPE,
                  LU_ITEM = LUITEM,
                  LU_DESCRIPTION = LUDESCRIPT)

}
