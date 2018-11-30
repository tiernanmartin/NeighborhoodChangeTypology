
#' @title Make A Tibble of The Project's US Census Variables from LTDB
#' @description Return a `tibble` of all of the US Census data variables
#'   that are obtained from the Brown University Longitudinal Tract Database (LTDB).
#' @param indicator_template Tibble, the `indicator_template` object
#' @param acs_tables Tibble, the `acs_table` object
#' @param path Character, the path or connection to write to.
#' @return a `tibble`
#' @note Data source: \link{https://s4.ad.brown.edu/projects/diversity/Researcher/LTBDDload/DataList.aspx}

#' @rdname ltdb-data
#' @export
prepare_ltdb_data <- function(indicator_template, acs_tables, path){


  # GET DATA ----------------------------------------------------------------

  # The data need to be manually downloaded from their source: https://s4.ad.brown.edu/projects/diversity/Researcher/LTBDDload/DataList.aspx


  # PREPARE DATA ------------------------------------------------------------

  tr_2000_fp <- "extdata/source/LTDB_Std_2000_Sample.csv"

  tr_2000_coltypes <- "cccccccdddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd" # sets the column data types quickly

  tract_2000_raw <- readr::read_csv(tr_2000_fp, col_types = tr_2000_coltypes) %>%
    janitor::clean_names("screaming_snake")

  # get the ACS code for the VALUE variable ("B25077")
  acs_variables_value <- acs_tables %>%
    dplyr::filter(INDICATOR %in% "VALUE") %>%
    dplyr::pull(VARIABLE)


  tract_2000 <- tract_2000_raw %>%
    dplyr::filter(STATE %in% "WA" & COUNTY %in% "King County") %>%
    dplyr::transmute(GEOGRAPHY_ID = TRTID10,
                     NAME = TRACT,
                     VARIABLE = acs_variables_value,
                     ESTIMATE = as.integer(MHMVAL00),
                     MOE = NA_real_,
                     ENDYEAR = 2000L
    )

  # REFORMAT DATA -----------------------------------------------------------

  tract_2000_ready <- indicator_template %>%
    dplyr::full_join(tract_2000,
                     c("GEOGRAPHY_ID",
                       GEOGRAPHY_NAME = "NAME",
                       "ENDYEAR",
                       "VARIABLE",
                       "ESTIMATE",
                       "MOE")) %>%
    dplyr::transmute(SOURCE = "LTDB",
                     GEOGRAPHY_ID,
                     GEOGRAPHY_ID_TYPE = "GEOID",
                     GEOGRAPHY_NAME,
                     GEOGRAPHY_TYPE = "tract",
                     ENDYEAR,
                     VARIABLE,
                     VARIABLE_SUBTOTAL = VARIABLE,
                     VARIABLE_SUBTOTAL_DESC,
                     MEASURE_TYPE = "VALUE",
                     ESTIMATE,
                     MOE = NA_real_
    )

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
                  MOE = as.numeric(MOE))

  return(ltdb_data)

}


