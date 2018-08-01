
#' @title Download the Zipped King County Parcels Data
#' @description Download a compressed archive of the tax assessor's data for
#'   all parcels in King County, WA (1998 - 2017).
#' @return `NULL`
#' @importFrom osfr download_files
#' @export
make_dl_parcel_data <- function(){

  # Original source: http://aqua.kingcounty.gov/extranet/assessor/Value%20History.zip

  data_osfr_id <- "t7b8v"

  data_fp <- "extdata/"

  data_fp_full <- osfr::download_files(id = data_osfr_id, path = data_fp)

  unzip(data_fp_full, exdir = "extdata")

  file.remove(data_fp_full)

  NULL

}
