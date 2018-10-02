#' @title Make Housing Market Sales Lookup Table Key List
#' @description Temporary description
#' @param parcel_lut_2018
#' @return A named list of tibbles
#' @export
make_sales_lut_key_list <- function(parcel_lut_2018){

  sales_lut_join_key <-   tibble::tribble(
    ~COL_NAME, ~LU_TYPE,
    "PROP_TYPE",        1,
    "PRIMARY_USE",        2,
    "SALES_INSTRUMENT",        6,
    "SALE_REASON",        5,
    "PROP_CLASS",        4
  )

  make_sales_lut_key_list <-
    inner_join(parcel_lut_2018, sales_lut_join_key, by = "LU_TYPE") %>%
    transmute(RNUM = row_number(),
              COL = COL_NAME,
              COL_NAME,
              CODE = LU_ITEM,
              DESC = LU_DESCRIPTION
    ) %>%
    gather(TYPE, VAL, -matches("COL|RNUM")) %>%
    unite(NEW, c("COL_NAME", "TYPE"), sep = "_") %>%
    mutate(NEW = str_replace(NEW, "_CODE", "")) %>%
    split(.$COL) %>%
    map(~ spread(.x, NEW, VAL) %>% select(-matches("RNUM|COL")))

  return(make_sales_lut_key_list)
}
