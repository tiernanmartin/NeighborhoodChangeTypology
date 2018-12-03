#' @title Make the Key For Decoding Condo Unit Categories
#' @description Make the key for decoding the King County Assessor's
#'   "Condo Unit" categories.
#' @param parcel_lut_2005 Tibble, The lookup table data for the 2005
#'   King County Assessor parcel data.
#' @param parcel_lut_2018 Tibble, The lookup table data for the 2018
#'   King County Assessor parcel data.
#' @return a `tibble`
#' @export
make_condo_unit_type_key <- function(parcel_lut_2005, parcel_lut_2018){

  # Condo Unit LU Items Only (#150)

 key <- dplyr::bind_rows(parcel_lut_2005, parcel_lut_2018) %>%
    dplyr::filter(META_LU_TYPE %in% 150) %>%
    dplyr::transmute(META_CONDO_UNIT_TYPE = META_LU_ITEM,
              META_CONDO_UNIT_TYPE_DESC = META_LU_DESCRIPTION) %>%
    dplyr::arrange(META_CONDO_UNIT_TYPE) %>%
    dplyr::distinct()

 return(key)

}
