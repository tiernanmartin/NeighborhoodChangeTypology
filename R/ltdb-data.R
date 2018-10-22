
#' @title Make A Tibble of The Project's US Census Variables from LTDB
#' @description Return a `tibble` of all of the US Census data variables
#'   that are obtained from the Brown University Longitudinal Tract Database (LTDB).
#' @param path Character, the path or connection to write to.
#' @return a `tibble`
#' @note Data source: \link{https://s4.ad.brown.edu/projects/diversity/Researcher/LTBDDload/DataList.aspx}

#' @rdname ltdb-data
#' @export
prepare_ltdb_data <- function(path){


  # GET DATA ----------------------------------------------------------------

  # The data need to be manually downloaded from their source: https://s4.ad.brown.edu/projects/diversity/Researcher/LTBDDload/DataList.aspx


  # PREPARE DATA ------------------------------------------------------------

  tr_2000_fp <- "extdata/source/LTDB_Std_2000_Sample.csv"

  tr_2000_coltypes <- "cccccccdddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd" # sets the column data types quickly

  tract_2000_raw <- readr::read_csv(tr_2000_fp, col_types = tr_2000_coltypes) %>%
    janitor::clean_names("screaming_snake")

  tract_2000_ready <- tract_2000_raw %>%
    dplyr::filter(STATE %in% "WA" & COUNTY %in% "King County") %>%
    dplyr::transmute(GEOID = TRTID10,
              NAME = TRACT,
              VARIABLE = "B25077",
              ESTIMATE = as.integer(MHMVAL00),
              MOE = NA_real_,
              ENDYEAR = 2000L
              )

  # WRITE DATA --------------------------------------------------------------

  readr::write_csv(x = tract_2000_ready, path = path)

  # RETURN ------------------------------------------------------------------

  ltdb_data_prep_status <- NeighborhoodChangeTypology::get_modified_time(path)

  return(ltdb_data_prep_status)

}


#' @rdname hud-chas-data
#' @export
make_ltdb_data <- function(path){

  ltdb_data <- suppressWarnings(suppressMessages(readr::read_csv(path))) %>%
    dplyr::mutate(GEOID = as.character(GEOID))

  return(ltdb_data)

}


