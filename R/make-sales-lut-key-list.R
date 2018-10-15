#' @title Make Housing Market Sales Lookup Table Key List
#' @description Temporary description
#' @param parcel_lut_2018 Temporary description
#' @return A named list of tibbles
#' @export
make_sales_lut_key_list <- function(parcel_lut_2018){

  sales_lut_join_key <-   tibble::tribble(
    ~COL_NAME, ~LU_TYPE,
    "PROPERTY_TYPE",        1,
    "PRINCIPAL_USE",        2,
    "SALE_INSTRUMENT",        6,
    "SALE_REASON",        5,
    "PROPERTY_CLASS",        4
  )

  make_sales_lut_key_list <-
    dplyr::inner_join(parcel_lut_2018, sales_lut_join_key, by = "LU_TYPE") %>%
    dplyr::transmute(RNUM = dplyr::row_number(),
              COL = COL_NAME,
              COL_NAME,
              CODE = LU_ITEM,
              DESC = LU_DESCRIPTION
    ) %>%
    tidyr::gather(TYPE, VAL, -matches("COL|RNUM")) %>%
    tidyr::unite(NEW, c("COL_NAME", "TYPE"), sep = "_") %>%
    dplyr::mutate(NEW = stringr::str_replace(NEW, "_CODE", "")) %>%
    split(.$COL) %>%
    purrr::map(~ tidyr::spread(.x, NEW, VAL) %>% dplyr::select(-matches("RNUM|COL")))

  return(make_sales_lut_key_list)
}
