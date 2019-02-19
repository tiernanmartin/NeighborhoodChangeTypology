
#' @title Make A Tibble of The Project's US Census Variables from American Factfinder
#' @description Return a `tibble` of all of the US Census data variables
#'   that are obtained from the American Factfinder user interface: \link{https://factfinder.census.gov/}
#' @param data_template Tibble, the `data_template` object
#' @param acs_tables Tibble, the `acs_table` object
#' @param path Character, the path or connection to write to.
#' @return a `tibble`
#' @note Data source: \link{https://factfinder.census.gov/faces/nav/jsf/pages/searchresults.xhtml?refresh=t}

#' @rdname factfinder-data
#' @export
prepare_factfinder_data <- function(data_template, acs_tables, path){


  # GET DATA ----------------------------------------------------------------

  # The data need to be manually downloaded from their source: https://factfinder.census.gov/faces/nav/jsf/pages/searchresults.xhtml?refresh=t


  # PREPARE DATA ------------------------------------------------------------

  kc_2000_lut_fp <- "extdata/source/american-factfinder-downloads/KING_COUNTY_DEC_00_SF4_DP4/DEC_00_SF4_DP4_metadata.csv"

  kc_2000_lut <- readr::read_csv(kc_2000_lut_fp) %>%
    magrittr::set_names(c("VARIABLE","DESCRIPTION"))


  kc_2000_fp <- "extdata/source/american-factfinder-downloads/KING_COUNTY_DEC_00_SF4_DP4/DEC_00_SF4_DP4.csv"

  kc_2000_colnames <- readr::read_csv(kc_2000_fp) %>%
    janitor::clean_names("screaming_snake") %>%
    names()

  kc_2000_raw <- readr::read_csv(kc_2000_fp, skip = 2,col_names = FALSE) %>%
    magrittr::set_colnames(kc_2000_colnames) %>%
    tidyr::gather(VARIABLE, ESTIMATE, dplyr::starts_with("HC")) %>%
    dplyr::mutate(GEO_ID2 = as.character(GEO_ID2),
                  ESTIMATE = as.double(ESTIMATE)) %>%
    dplyr::left_join(kc_2000_lut, by = "VARIABLE")

  # get the ACS code for the VALUE variable ("B25077")
  acs_variables_value <- acs_tables %>%
    dplyr::filter(INDICATOR %in% c("VALUE", "RENT")) %>%
    dplyr::pull(VARIABLE)

  acs_ltdb_join <- tibble::tibble(VARIABLE_ACS = acs_variables_value,
                                  VARIABLE_LTDB = c("HC01_VC73",  "HC01_VC104"))

  kc_median_home_value_2000 <- kc_2000_raw %>%
    dplyr::filter(DESCRIPTION %in% c("Number; Specified owner-occupied units - VALUE - Median (dollars)",
                                     "Number; Specified renter-occupied units - GROSS RENT - Median (dollars)")) %>%
    dplyr::left_join(acs_ltdb_join, by = c(VARIABLE = "VARIABLE_LTDB")) %>%
    dplyr::select(-VARIABLE) %>%
    dplyr::rename(VARIABLE = VARIABLE_ACS)

  # REFORMAT DATA -----------------------------------------------------------

  kc_median_home_value_2000_transformed <- kc_median_home_value_2000 %>%
    dplyr::transmute(SOURCE = "FACTFINDER",
                     GEOGRAPHY_ID = GEO_ID2,
                     GEOGRAPHY_ID_TYPE = "GEOID",
                     GEOGRAPHY_NAME = GEO_DISPLAY_LABEL,
                     GEOGRAPHY_TYPE = "county",
                     DATE_BEGIN = get_date_begin("2000"), # creates the first day of the 5-year span
                     DATE_END = get_date_end("2000"), # creates the last day of the 5-year span
                     DATE_GROUP_ID = create_range_year(DATE_BEGIN,DATE_END),
                     DATE_RANGE = create_range_date(DATE_BEGIN, DATE_END),
                     DATE_RANGE_TYPE = "one year",
                     VARIABLE,
                     VARIABLE_SUBTOTAL = VARIABLE,
                     VARIABLE_SUBTOTAL_DESC = DESCRIPTION,
                     MEASURE_TYPE = "MEDIAN",
                     ESTIMATE,
                     MOE = NA_real_
    ) %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)


  kc_median_home_value_2000_formatted <-  data_template %>%
    dplyr::full_join(kc_median_home_value_2000_transformed,
                     by = c("SOURCE",
                            "GEOGRAPHY_ID",
                            "GEOGRAPHY_ID_TYPE",
                            "GEOGRAPHY_NAME",
                            "GEOGRAPHY_TYPE",
                            "DATE_GROUP_ID",
                            "DATE_BEGIN",
                            "DATE_END",
                            "DATE_RANGE",
                            "DATE_RANGE_TYPE",
                            "VARIABLE",
                            "VARIABLE_SUBTOTAL",
                            "VARIABLE_SUBTOTAL_DESC",
                            "MEASURE_TYPE",
                            "ESTIMATE",
                            "MOE"))

  kc_median_home_value_2000_ready <- kc_median_home_value_2000_formatted

  # WRITE DATA --------------------------------------------------------------

  readr::write_csv(x = kc_median_home_value_2000_ready, path = path)

  # RETURN ------------------------------------------------------------------

  factfinder_data_prep_status <- NeighborhoodChangeTypology::get_modified_time(path)

  return(factfinder_data_prep_status)

}


#' @rdname factfinder-data
#' @export
make_factfinder_data <- function(path){

  factfinder_data <- suppressWarnings(suppressMessages(readr::read_csv(path))) %>%
    dplyr::mutate(GEOGRAPHY_ID = as.character(GEOGRAPHY_ID),
                  DATE_BEGIN = as.character(DATE_BEGIN),
                  DATE_END = as.character(DATE_END),
                  MOE = as.numeric(MOE))

  return(factfinder_data)

}

