
#' @title Make The General Tax Data For King County Condominium Units (2010)
#' @description Make the general tax data for condominium units in King County, Washington.
#' @param zip_path Character, the file path of the archive file
#' @param file_path Character, the file path of the file that is to be extracted
#' @return a `tibble`
#' @export
make_condo_info_2010 <- function(zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  condo_info_2010 <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake")

  return(condo_info_2010)
}
