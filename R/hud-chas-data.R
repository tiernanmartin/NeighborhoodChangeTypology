
#' @title Make A Tibble of The Project's HUD CHAS Variables
#' @description Return a `tibble` of all of the HUD CHAS data variables
#'   that are used in the Neighborhood Change Typology model for both 5-year spans
#'   (2006-2010 and 2011-2015).
#' @param data_template Tibble, the `data_template` object
#' @param zip_path Character, the path or connection to write to.
#' @param file_path Character, the file path of the file that is to be extracted
#' @return a `tibble`
#' @note Data source: \link{https://www.huduser.gov/portal/datasets/cp.html}
#'   The 2006-2010 tract data uses the '080' geographic summary level,
#'   which was discontinued by the Census in 2014. Tract data from 2009-2013
#'   onward uses the '140' summary level.
#'
#'   After exploring the data, it became clear that '080' can be combined to
#'   form '140' and this approach is implemented in `make_hud_chas_data()`.
#'
#'   Links:
#'   \itemize{
#'     \item Summary level code list \link{https://factfinder.census.gov/help/en/summary_level_code_list.htm}
#'     \item Discontinuation of Summary Level 080 \link{https://www.census.gov/programs-surveys/acs/technical-documentation/user-notes/2013-11.html}
#'   }

#' @rdname hud-chas-data
#' @export
prepare_hud_chas_data <- function(zip_path){

  # GET DATA ----------------------------------------------------------------

  # The data need to be manually downloaded from their source: https://www.huduser.gov/portal/datasets/cp.html

  # WRITE DATA --------------------------------------------------------------

  # The following files should be uploaded to OSF:
  #   - all Table7.csv files (these contain the crosstabulations for determining low income hh counts)
  #   - one data-dictionary.xlsx file

  target_dir <- tools::file_path_sans_ext(zip_path)

  suppressWarnings(suppressMessages(dir.create(target_dir)))


  dict_filepath <- "extdata/source/2006thru2010-050-csv/CHAS data dictionary 06-10.xlsx"

  data_dictionary <- readxl::read_xlsx(path = dict_filepath, sheet = "Table 7") %>%
    janitor::clean_names(case= "screaming_snake")

  readr::write_csv(data_dictionary, "extdata/source/2006thru2010-050-csv/hud-chas-datadictionary-table7.csv")


  filepath_copy_tbl <- tibble::tribble(
    ~from,                                               ~to,
    "extdata/source/2006thru2010-050-csv/Table7.csv", file.path(target_dir,"hud-chas-050-20062010-table7.csv"),
    "extdata/source/2006thru2010-080-csv/Table7.csv", file.path(target_dir,"hud-chas-080-20062010-table7.csv"),
    "extdata/source/2011thru2015-050-csv/050/Table7.csv", file.path(target_dir,"hud-chas-050-20112015-table7.csv"),
    "extdata/source/2011thru2015-140-csv/140/Table7.csv", file.path(target_dir,"hud-chas-140-20112015-table7.csv"),
    "extdata/source/2006thru2010-050-csv/hud-chas-datadictionary-table7.csv", file.path(target_dir,"hud-chas-datadictionary-table7.csv")
  )


  purrr::pwalk(filepath_copy_tbl, file.copy) # copies the file paths to new, renamed files

  zip_subdirectory(zip_path = zip_path, dir_path = target_dir)


  # RETURN ------------------------------------------------------------------

  hud_chas_data_prep_status <- NeighborhoodChangeTypology::get_modified_time(zip_path)

  return(hud_chas_data_prep_status)

}


#' @rdname hud-chas-data
#' @export
make_hud_chas_data <- function(data_template, zip_path, file_path){


  # LOAD DATA ---------------------------------------------------------------

  unzip(zip_path, exdir = "extdata/osf")

  # LOAD CHAS DATA -------------------------------------------------------


  county_2006_2010_fp <- "extdata/osf/hud-chas-data/hud-chas-050-20062010-table7.csv"

  tr_2006_2010_fp <- "extdata/osf/hud-chas-data/hud-chas-080-20062010-table7.csv"

  county_2011_2015_fp <- "extdata/osf/hud-chas-data/hud-chas-050-20112015-table7.csv"

  tr_2011_2015_fp <- "extdata/osf/hud-chas-data/hud-chas-140-20112015-table7.csv"

  read_chas_csv <- function(x){

    # this function resolves column type inconsistencies between the data sets
    # 'T7_est1' is the pattern for the columns with count estimates

    readr::read_csv(x) %>%
      janitor::clean_names("screaming_snake") %>%
      dplyr::mutate_at(dplyr::vars(-matches("T7")),as.character)
  }


  get_tr_number <- function(x){
    stringr::str_c("Census Tract",as.character(as.double(stringr::str_extract(x,".{6}$"))*1e-2),"King County, Washington", sep = " ")
  }

  county_2006_2010 <- read_chas_csv(county_2006_2010_fp) %>%
    dplyr::mutate(DATE_BEGIN = get_date_begin(2010L - 4L), # creates the first day of the 5-year span
                  DATE_END = get_date_end(2010L), # creates the last day of the 5-year span
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "five years",
                  ST =  stringr::str_extract(GEOID,"(?<=US).{2}"),
                  CNTY = stringr::str_extract(GEOID,"(?<=US.{2}).{3}"),
                  GEOGRAPHY_TYPE = "county") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character) %>% # convert Date cols to character
    dplyr::filter(ST %in% "53" & CNTY %in% "033") %>%
    dplyr::select(DATE_BEGIN, DATE_END, DATE_RANGE, DATE_RANGE_TYPE, NAME, GEOID, GEOGRAPHY_TYPE, dplyr::matches("T7"))

  tr_2006_2010 <- read_chas_csv(tr_2006_2010_fp) %>%
    dplyr::mutate(DATE_BEGIN = get_date_begin(2010L - 4L), # creates the first day of the 5-year span
                  DATE_END = get_date_end(2010L), # creates the last day of the 5-year span
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "five years",
                  ST =  stringr::str_extract(GEOID,"(?<=US).{2}"),
                  CNTY = stringr::str_extract(GEOID,"(?<=US.{2}).{3}"),
                  GEOGRAPHY_TYPE = "tract",
                  GEOID = stringr::str_c(stringr::str_extract(GEOID,"^.{12}"),stringr::str_extract(GEOID,".{6}$")),
                  NAME = get_tr_number(GEOID)) %>%
    dplyr::mutate_if(lubridate::is.Date, as.character) %>% # convert Date cols to character
    dplyr::filter(ST %in% "53" & CNTY %in% "033") %>%
    dplyr::select(DATE_BEGIN, DATE_END, DATE_RANGE, DATE_RANGE_TYPE, NAME, GEOID, GEOGRAPHY_TYPE, dplyr::matches("T7"))

  county_2011_2015 <- read_chas_csv(county_2011_2015_fp) %>%
    dplyr::mutate(DATE_BEGIN = get_date_begin(2015L - 4L), # creates the first day of the 5-year span
                  DATE_END = get_date_end(2015L), # creates the last day of the 5-year span
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "five years",
                  GEOGRAPHY_TYPE = "county") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character) %>% # convert Date cols to character
    dplyr::filter(ST %in% "53" & CNTY %in% "033") %>%
    dplyr::select(DATE_BEGIN, DATE_END, DATE_RANGE, DATE_RANGE_TYPE, NAME, GEOID, GEOGRAPHY_TYPE, dplyr::matches("T7"))

  tr_2011_2015 <- read_chas_csv(tr_2011_2015_fp) %>%
    dplyr::mutate(DATE_BEGIN = get_date_begin(2015L - 4L), # creates the first day of the 5-year span
                  DATE_END = get_date_end(2015L), # creates the last day of the 5-year span
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "five years",
                  GEOGRAPHY_TYPE = "tract") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character) %>% # convert Date cols to character
    dplyr::filter(ST %in% "53" & CNTY %in% "033") %>%
    dplyr::select(DATE_BEGIN, DATE_END, DATE_RANGE, DATE_RANGE_TYPE, NAME, GEOID, GEOGRAPHY_TYPE, dplyr::matches("T7"))



  # COMBINE CHAS DATA -------------------------------------------------------

  hud_chas_table7_all <- list(county_2006_2010, tr_2006_2010, county_2011_2015, tr_2011_2015) %>%
    purrr::reduce(dplyr::bind_rows)

  hud_chas_table7_all_long <- hud_chas_table7_all %>%
    dplyr::mutate(GEOID = dplyr::case_when(  # remove the unnecessary digits from GEOID
      nchar(GEOID) == 12 ~ stringr::str_extract(GEOID,".{5}$"),
      nchar(GEOID) == 18 ~ stringr::str_extract(GEOID,".{11}$"),
      TRUE ~ NA_character_
    )) %>%
    tidyr::gather(VAR, VALUE, dplyr::matches("T7")) %>%
    dplyr::mutate(VARIABLE = stringr::str_c("T7_",stringr::str_pad(stringr::str_extract(VAR,'\\d{1,3}$'),width = 3,side = 'left', pad = '0'),sep = ""),
                  TYPE = dplyr::case_when(
                    stringr::str_detect(VAR,"EST") ~ "ESTIMATE",
                    stringr::str_detect(VAR, "MOE") ~ "MOE",
                    TRUE ~ NA_character_)
    ) %>%
    dplyr::group_by(DATE_BEGIN, DATE_END, DATE_RANGE, DATE_RANGE_TYPE, GEOID, NAME, GEOGRAPHY_TYPE, VARIABLE, TYPE) %>%
    dplyr::mutate(N = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(-VAR) %>%
    tidyr::spread(TYPE, VALUE) %>%
    dplyr::group_by(DATE_BEGIN, DATE_END, DATE_RANGE, DATE_RANGE_TYPE, GEOID, NAME, GEOGRAPHY_TYPE, VARIABLE) %>%
    dplyr::summarize(ESTIMATE = sum(ESTIMATE, na.rm =  TRUE),
                     MOE = tidycensus::moe_sum(MOE,ESTIMATE, na.rm = TRUE)) %>%
    dplyr::ungroup()


  # REFORMAT DATA -----------------------------------------------------------

  hud_chas_table7_all_long_complete <- hud_chas_table7_all_long %>%
    dplyr::mutate(VARIABLE_SUBTOTAL = VARIABLE) %>%
    dplyr::transmute(SOURCE = "CHAS",
                     GEOGRAPHY_ID = GEOID,
                     GEOGRAPHY_ID_TYPE = "GEOID",
                     GEOGRAPHY_NAME = NAME,
                     GEOGRAPHY_TYPE,
                     DATE_BEGIN,
                     DATE_END,
                     DATE_RANGE,
                     DATE_RANGE_TYPE,
                     VARIABLE = "T7",
                     VARIABLE_SUBTOTAL,
                     VARIABLE_SUBTOTAL_DESC = NA_character_,
                     MEASURE_TYPE = "COUNT",
                     ESTIMATE,
                     MOE)

  hud_chas_table7_all_long_formatted <- data_template %>%
    dplyr::full_join(hud_chas_table7_all_long_complete,
                     by = c("SOURCE",
                            "GEOGRAPHY_ID",
                            "GEOGRAPHY_ID_TYPE",
                            "GEOGRAPHY_NAME",
                            "GEOGRAPHY_TYPE",
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

  hud_chas_table7_all_long_ready <- hud_chas_table7_all_long_formatted

  # WRITE DATA --------------------------------------------------------------

  readr::write_csv(hud_chas_table7_all_long_ready, file_path)


  # RETURN ------------------------------------------------------------------

  return(hud_chas_table7_all_long_ready)

}

#' @rdname hud-chas-data
#' @export
make_hud_chas_data_lut <- function(zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  hud_chas_data_lut <- readr::read_csv(file_path) %>%
    janitor::clean_names(case= "screaming_snake")

  return(hud_chas_data_lut)
}
