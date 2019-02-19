
#' @title Make A Tibble of The Project's US Census Variables from LTDB
#' @description Return a `tibble` of all of the US Census data variables
#'   that are obtained from the Brown University Longitudinal Tract Database (LTDB).
#' @param data_template Tibble, the `data_template` object
#' @param acs_tables Tibble, the `acs_table` object
#' @param path Character, the path or connection to write to.
#' @return a `tibble`
#' @note Data source: \link{https://s4.ad.brown.edu/projects/diversity/Researcher/LTBDDload/DataList.aspx}

#' @rdname ltdb-data
#' @export
prepare_ltdb_data <- function(data_template, acs_tables, path){


  # GET DATA ----------------------------------------------------------------

  # The data need to be manually downloaded from their source: https://s4.ad.brown.edu/projects/diversity/Researcher/LTBDDload/DataList.aspx


  # PREPARE DATA ------------------------------------------------------------

  tr_2000_fp <- "extdata/source/LTDB_Std_2000_Sample.csv"

  tr_2000_coltypes <- "cccccccdddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd" # sets the column data types quickly

  tract_2000_raw <- readr::read_csv(tr_2000_fp, col_types = tr_2000_coltypes) %>%
    janitor::clean_names("screaming_snake")

  # get the ACS code for the VALUE and RENT variables ("B25077" and "B25058")
  acs_variables_value <- acs_tables %>%
    dplyr::filter(INDICATOR %in% c("VALUE", "RENT")) %>%
    dplyr::pull(VARIABLE)

  acs_ltdb_join <- tibble::tibble(VARIABLE_ACS = acs_variables_value,
                                  VARIABLE_LTDB = c("MHMVAL00",  "MRENT00"))


  kc_tr_2000_long <- tract_2000_raw %>%
    dplyr::filter(STATE %in% "WA" & COUNTY %in% "King County") %>%
    dplyr::select(GEOGRAPHY_ID = TRTID10,
                  MHMVAL00,
                  MRENT00) %>%
    tidyr::gather(VARIABLE, ESTIMATE, -GEOGRAPHY_ID) %>%
    dplyr::left_join(acs_ltdb_join, by = c(VARIABLE = "VARIABLE_LTDB")) %>%
    dplyr::select(-VARIABLE) %>%
    dplyr::rename(VARIABLE = VARIABLE_ACS)

  kc_tr_2000_long

  kc_tr_2000_long_complete <- kc_tr_2000_long %>%
    dplyr::transmute(SOURCE = "LTDB",
                     GEOGRAPHY_ID,
                     GEOGRAPHY_ID_TYPE = "GEOID",
                     GEOGRAPHY_NAME = NA_character_, # leave this blank for now and later fill it in withe the ACS name strings
                     GEOGRAPHY_TYPE = "tract",
                     DATE_BEGIN = as.character(get_date_begin(2000L)), # creates the first day of the 5-year span
                     DATE_END = as.character(get_date_end(2000L)), # creates the last day of the 5-year span
                     DATE_GROUP_ID = create_range_year(DATE_BEGIN, DATE_END),
                     DATE_RANGE = create_range_date(DATE_BEGIN, DATE_END),
                     DATE_RANGE_TYPE = "one year",
                     VARIABLE,
                     VARIABLE_SUBTOTAL = VARIABLE,
                     VARIABLE_SUBTOTAL_DESC = NA_character_,
                     MEASURE_TYPE = "MEDIAN",
                     ESTIMATE,
                     MOE = NA_real_

    ) %>%
     dplyr::mutate_if(lubridate::is.Date, as.character)

  # REFORMAT DATA -----------------------------------------------------------

  tract_2000_formatted <- data_template %>%
    dplyr::full_join(kc_tr_2000_long_complete,
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

  tract_2000_ready <- tract_2000_formatted

  # WRITE DATA --------------------------------------------------------------

  readr::write_csv(x = tract_2000_ready, path = path)

  # RETURN ------------------------------------------------------------------

  ltdb_data_prep_status <- NeighborhoodChangeTypology::get_modified_time(path)

  return(ltdb_data_prep_status)

}


#' @rdname ltdb-data
#' @export
make_ltdb_data <- function(path){

  ltdb_data <- suppressWarnings(suppressMessages(readr::read_csv(path))) %>%
    dplyr::mutate(GEOGRAPHY_ID = as.character(GEOGRAPHY_ID),
                  DATE_GROUP_ID = as.character(DATE_GROUP_ID),
                  DATE_BEGIN = as.character(DATE_BEGIN),
                  DATE_END = as.character(DATE_END),
                  VARIABLE_SUBTOTAL_DESC = as.character(VARIABLE_SUBTOTAL_DESC),
                  MOE = as.numeric(MOE))

  return(ltdb_data)

}
