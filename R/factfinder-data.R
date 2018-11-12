
#' @title Make A Tibble of The Project's US Census Variables from American Factfinder
#' @description Return a `tibble` of all of the US Census data variables
#'   that are obtained from the American Factfinder user interface: \link{https://factfinder.census.gov/}
#' @param indicator_template Tibble, the `indicator_template` object
#' @param acs_tables Tibble, the `acs_table` object
#' @param path Character, the path or connection to write to.
#' @return a `tibble`
#' @note Data source: \link{https://factfinder.census.gov/faces/nav/jsf/pages/searchresults.xhtml?refresh=t}

#' @rdname factfinder-data
#' @export
prepare_factfinder_data <- function(indicator_template, acs_tables, path){


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
    dplyr::filter(INDICATOR %in% "VALUE") %>%
    dplyr::pull(VARIABLE)

  kc_median_home_value_2000 <- kc_2000_raw %>%
    dplyr::filter(DESCRIPTION %in% "Number; Specified owner-occupied units - VALUE - Median (dollars)") %>%
    dplyr::mutate(VARIABLE = acs_variables_value) # change the VARIABLE from Factfinder format to ACS API format

  # REFORMAT DATA -----------------------------------------------------------

 kc_median_home_value_2000_ready <-  indicator_template %>%
    dplyr::full_join(kc_median_home_value_2000,
                     c(GEOGRAPHY_ID = "GEO_ID2",
                       GEOGRAPHY_NAME = "GEO_DISPLAY_LABEL",
                       "VARIABLE",
                       "ESTIMATE")) %>%
    dplyr::transmute(SOURCE = "FACTFINDER",
                     GEOGRAPHY_ID,
                     GEOGRAPHY_ID_TYPE = "GEOID",
                     GEOGRAPHY_NAME,
                     GEOGRAPHY_TYPE = "county",
                     ENDYEAR = 2000L,
                     VARIABLE,
                     VARIABLE_SUBTOTAL = VARIABLE,
                     MEASURE_TYPE = "VALUE",
                     ESTIMATE,
                     MOE = NA_real_
    )

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
    dplyr::mutate(GEOGRAPHY_ID = as.character(GEOGRAPHY_ID))

  return(factfinder_data)

}


