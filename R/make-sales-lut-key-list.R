#' @title Make Housing Market Sales Lookup Table Key List
#' @description Temporary description
#' @param parcel_lut_2018 Temporary description
#' @return A named list of tibbles
#' @export
make_sales_lut_key_list <- function(parcel_lut_2018){

  sales_lut_join_key <-   tibble::tribble(
    ~COL_NAME, ~META_LU_TYPE,
    "META_PROPERTY_TYPE",        1,
    "META_PRINCIPAL_USE",        2,
    "META_SALE_INSTRUMENT",        6,
    "META_SALE_REASON",        5,
    "META_PROPERTY_CLASS",        4
  )

  sales_lut_key_list <-
    dplyr::inner_join(parcel_lut_2018, sales_lut_join_key, by = "META_LU_TYPE") %>%
    dplyr::transmute(RNUM = dplyr::row_number(),
              COL = COL_NAME,
              COL_NAME,
              CODE = META_LU_ITEM,
              DESC = META_LU_DESCRIPTION
    ) %>%
    tidyr::gather(TYPE, VAL, -matches("COL|RNUM")) %>%
    tidyr::unite(NEW, c("COL_NAME", "TYPE"), sep = "_") %>%
    dplyr::mutate(NEW = stringr::str_replace(NEW, "_CODE", "")) %>%
    split(.$COL) %>%
    purrr::map(~ tidyr::spread(.x, NEW, VAL) %>% dplyr::select(-matches("RNUM|COL")))

  return(sales_lut_key_list)
}
