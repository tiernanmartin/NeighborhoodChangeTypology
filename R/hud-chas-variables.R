
#' @title Make A Tibble of The Project's HUD CHAS Variables
#' @description Return a `tibble` of all of the HUD CHAS data variables
#'   that are used in the Neighborhood Change Typology model for both 5-year spans
#'   (2006-2010 and 2011-2015).
#' @param hud_chas_data Tibble, desc
#' @param hud_chas_data_lut Tibble, desc
#' @param model_table Tibble, desc
#' @param variable_template Tibble, desc
#' @return a `tibble`

#' @rdname hud-chas-variables
#' @export
make_hud_chas_variables <- function(hud_chas_data, hud_chas_data_lut, model_table, variable_template){

  # PREPARE HUD CHAS DATA ROLES --------------------------------------------------------

  chas_inds <- model_table %>%
    dplyr::filter(SOURCE %in% "CHAS") %>%  # right now, CHAS data is only used for the INCOME indicator
    dplyr::select(SOURCE, INDICATOR) %>%
    dplyr::distinct()


  hud_chas_roles <- hud_chas_data_lut %>%
    dplyr::filter(LINE_TYPE %in% c("Total", "Subtotal")) %>%
    dplyr::filter(is.na(HOUSEHOLD_TYPE) & is.na(COST_BURDEN)) %>%
    dplyr::mutate(LOW_INCOME = dplyr::case_when(
      is.na(HOUSEHOLD_INCOME) ~ "all incomes",
      stringr::str_detect(HOUSEHOLD_INCOME,"less than or equal to 30%") ~ "low income",
      stringr::str_detect(HOUSEHOLD_INCOME,"greater than 30%") ~ "low income",
      stringr::str_detect(HOUSEHOLD_INCOME,"greater than 50%") ~ "low income",
      TRUE ~ "mod/high income"
    )) %>%
    dplyr::transmute(SOURCE = "CHAS",
                     VARIABLE_SUBTOTAL = stringr::str_c("T7_",stringr::str_pad(stringr::str_extract(COLUMN_NAME,'\\d{1,3}$'),width = 3,side = 'left', pad = '0'),sep = ""),
                     VARIABLE_ROLE = dplyr::case_when(
                       LINE_TYPE %in% "Total" ~ "total",
                       LOW_INCOME %in% "low income" ~ "count",
                       TRUE ~ "omit"
                     )) %>%
    dplyr::left_join(chas_inds, by = "SOURCE")


  # JOIN ROLES TO HUD CHAS DATA ---------------------------------------------

  hud_chas_vars_data <- hud_chas_data %>%
    dplyr::inner_join(hud_chas_roles, by = c("SOURCE","VARIABLE_SUBTOTAL")) # this filters out any variables not in the hud_chas_roles


  # CREATE VARIABLE_DESC ----------------------------------------------------

  hud_chas_vars_desc <- hud_chas_vars_data %>%
     dplyr::mutate(VARIABLE_DESC = stringr::str_c(INDICATOR, SOURCE, sep = "_"))


  # ARRANGE COLUMNS WITH TEMPLATE -------------------------------------------


  hud_chas_vars_ready <- variable_template %>%
    dplyr::full_join(hud_chas_vars_desc,
                     by = c("SOURCE",
                            "GEOGRAPHY_ID",
                            "GEOGRAPHY_ID_TYPE",
                            "GEOGRAPHY_NAME",
                            "GEOGRAPHY_TYPE",
                            "ENDYEAR",
                            "INDICATOR",
                            "VARIABLE",
                            "VARIABLE_DESC",
                            "VARIABLE_SUBTOTAL",
                            "VARIABLE_SUBTOTAL_DESC",
                            "VARIABLE_ROLE",
                            "MEASURE_TYPE",
                            "ESTIMATE",
                            "MOE"))

  hud_chas_variables <- hud_chas_vars_ready

  check_hud_chas_vars_ready <- function(){

    # This function shows all of the INDICATOR values and their INDICATOR_ROLEs.
    # If any NA's are showing up then something needs to be fixed

     hud_chas_variables %>% dplyr::count(ENDYEAR,INDICATOR, VARIABLE, VARIABLE_DESC, VARIABLE_ROLE)
  }

  # RETURN ------------------------------------------------------------------

  return(hud_chas_variables)


}
