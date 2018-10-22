
#' @title Make A Tibble of The Project's HUD CHAS Variables
#' @description Return a `tibble` of all of the HUD CHAS data variables
#'   that are used in the Neighborhood Change Typology model for both 5-year spans
#'   (2006-2010 and 2011-2015).
#' @param path Character, the path or connection to write to.
#' @return a `tibble`
#' @note Data source: \link{https://www.huduser.gov/portal/datasets/cp.html}

#' @rdname hud-chas-data
#' @export
prepare_hud_chas_data <- function(path){


  # GET DATA ----------------------------------------------------------------

  # The data need to be manually downloaded from their source: https://www.huduser.gov/portal/datasets/cp.html


  # PREPARE DATA: DATA DICTIONARY -------------------------------------------

  dict_filepath <- "extdata/source/2006thru2010-050-csv/CHAS data dictionary 06-10.xlsx"

  data_dictionary <- readxl::read_xlsx(path = dict_filepath, sheet = "Table 7") %>%
    janitor::clean_names(case= "screaming_snake")

  dictionary_filter <- data_dictionary %>%
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
                     INDICATOR = "INCOME",
                     VARIABLE = stringr::str_c("T7_",stringr::str_pad(stringr::str_extract(COLUMN_NAME,'\\d{1,3}$'),width = 3,side = 'left', pad = '0'),sep = ""),
                     ROLE = dplyr::case_when(
                       LINE_TYPE %in% "Total" ~ "DENOMINATOR",
                       LOW_INCOME %in% "low income" ~ "NUMERATOR",
                       TRUE ~ "OMIT"
                     ))


  # PREPARE CHAS DATA -------------------------------------------------------

  county_2006_2010_fp <- "extdata/source/2006thru2010-050-csv/Table7.csv"

  tr_2006_2010_fp <- "extdata/source/2006thru2010-080-csv/Table7.csv"

  county_2011_2015_fp <- "extdata/source/2011thru2015-050-csv/050/Table7.csv"

  tr_2011_2015_fp <- "extdata/source/2011thru2015-140-csv/140/Table7.csv"

  fp_list <- list(county_2006_2010_fp,
                  tr_2006_2010_fp,
                  county_2011_2015_fp,
                  tr_2011_2015_fp)

  endyear_list <- c(2010L,2010L,2015L,2015L)

  read_chas_csv <- function(x, endyear){

    # this function resolves column type inconsistencies between the data sets
    # 'T7_est1' is the pattern for the columns with count estimates

    readr::read_csv(x) %>%
      janitor::clean_names("screaming_snake") %>%
      dplyr::mutate_at(dplyr::vars(-matches("T7")),as.character) %>%
      dplyr::mutate(ENDYEAR = endyear)
  }


  hud_chas_table7_all <- purrr::map2_dfr(fp_list, endyear_list, read_chas_csv) %>%
    dplyr::filter(ST %in% "53" & CNTY %in% "033") %>% # these correspond with WA and King County
    dplyr::select(ENDYEAR, GEOID, NAME, dplyr::matches("T7")) %>%
    tidyr::gather(VAR, VALUE, dplyr::matches("T7"))  %>%
    dplyr::transmute(GEOID = stringr::str_extract(GEOID, "(?<=US).*"),
                     ENDYEAR,
                     VALUE,
                     TYPE = dplyr::case_when(
                       stringr::str_detect(VAR,"EST") ~ "ESTIMATE",
                       stringr::str_detect(VAR, "MOE") ~ "MOE",
                       TRUE ~ NA_character_
                     ),
                     VARIABLE = stringr::str_c("T7_",stringr::str_pad(stringr::str_extract(VAR,'\\d{1,3}$'),width = 3,side = 'left', pad = '0'),sep = "")) %>%
    tidyr::spread(TYPE, VALUE)


  # FILTER CHAS DATA --------------------------------------------------------

  hud_chas_low_income <- hud_chas_table7_all %>%
    dplyr::left_join(dictionary_filter, by = "VARIABLE") %>%
    dplyr::filter(ROLE %in% c("DENOMINATOR","NUMERATOR"))


  # WRITE DATA --------------------------------------------------------------

  readr::write_csv(x = hud_chas_low_income, path = path)

  # RETURN ------------------------------------------------------------------

  hud_chas_data_prep_status <- NeighborhoodChangeTypology::get_modified_time(path)

  return(hud_chas_data_prep_status)

}


#' @rdname hud-chas-data
#' @export
make_hud_chas_data <- function(path){

  hud_chas_data <- suppressWarnings(suppressMessages(readr::read_csv(path))) %>%
    dplyr::mutate(GEOID = as.character(GEOID))

  return(hud_chas_data)

}


