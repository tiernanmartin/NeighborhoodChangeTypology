#' @title Make the Key For Decoding Present Use Categories
#' @description Make the key for decoding the King County Assessor's
#'   "Present Use" categories.
#' @param parcel_lut_2005 Tibble, The lookup table data for the 2005
#'   King County Assessor parcel data.
#' @param parcel_lut_2018 Tibble, The lookup table data for the 2018
#'   King County Assessor parcel data.
#' @return a `tibble`
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr transmute
#' @importFrom dplyr bind_rows
#' @export
make_present_use_key <- function(parcel_lut_2005, parcel_lut_2018){

  # Present Use LU Items Only (#102)

  present_use_key <- dplyr::bind_rows(parcel_lut_2005, parcel_lut_2018) %>%
    dplyr::filter(META_LU_TYPE %in% 102) %>%
    dplyr::transmute(META_PRESENT_USE = META_LU_ITEM,
              META_PRESENT_USE_DESC = META_LU_DESCRIPTION) %>%
    dplyr::arrange(META_PRESENT_USE) %>%
    dplyr::distinct()
}
