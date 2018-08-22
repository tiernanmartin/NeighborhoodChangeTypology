#' @title Make the Key For Decoding Condo Unit Categories
#' @description Make the key for decoding the King County Assessor's
#'   "Condo Unit" categories.
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
make_condo_unit_type_key <- function(parcel_lut_2005, parcel_lut_2018){

  # Condo Unit LU Items Only (#150)

 key <- dplyr::bind_rows(parcel_lut_2005, parcel_lut_2018) %>%
    dplyr::filter(LU_TYPE %in% 150) %>%
    dplyr::transmute(CONDO_UNIT_TYPE = LU_ITEM,
              CONDO_UNIT_TYPE_DESC = LU_DESCRIPTION) %>%
    dplyr::arrange(CONDO_UNIT_TYPE) %>%
    dplyr::distinct()

 return(key)

}
