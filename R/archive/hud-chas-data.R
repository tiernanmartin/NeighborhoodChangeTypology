
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



  # NOTE ON CHAS DATA -------------------------------------------------------

  # Note: the 2006-2010 tract data uses the '080' geographic summary level,
  # which was discontinued by the Census in 2014. Tract data from 2009-2013
  # onward uses the '140' summary level.
  #
  # After exploring the data, it became clear that '080' can be combined to
  # form '140' and this approach is implemented in the script below.
  #
  # Links:
  #   - Summary level code list <https://factfinder.census.gov/help/en/summary_level_code_list.htm>
  #   - Discontinuation of Summary Level 080 <https://www.census.gov/programs-surveys/acs/technical-documentation/user-notes/2013-11.html>

  # LOAD CHAS DATA -------------------------------------------------------



  county_2006_2010_fp <- "extdata/source/2006thru2010-050-csv/Table7.csv"

  tr_2006_2010_fp <- "extdata/source/2006thru2010-080-csv/Table7.csv"

  county_2011_2015_fp <- "extdata/source/2011thru2015-050-csv/050/Table7.csv"

  tr_2011_2015_fp <- "extdata/source/2011thru2015-140-csv/140/Table7.csv"

  read_chas_csv <- function(x){

    # this function resolves column type inconsistencies between the data sets
    # 'T7_est1' is the pattern for the columns with count estimates

    readr::read_csv(x) %>%
      janitor::clean_names("screaming_snake") %>%
      dplyr::mutate_at(dplyr::vars(-matches("T7")),as.character)
  }


  county_2006_2010 <- read_chas_csv(county_2006_2010_fp) %>%
    dplyr::mutate(ENDYEAR = 2010L,
                  ST =  stringr::str_extract(GEOID,"(?<=US).{2}"),
                  CNTY = stringr::str_extract(GEOID,"(?<=US.{2}).{3}")) %>%
    dplyr::filter(ST %in% "53" & CNTY %in% "033") %>%
    dplyr::select(ENDYEAR, GEOID, dplyr::matches("T7"))

  tr_2006_2010 <- read_chas_csv(tr_2006_2010_fp) %>%
    dplyr::mutate(ENDYEAR = 2010L,
                  ST =  stringr::str_extract(GEOID,"(?<=US).{2}"),
                  CNTY = stringr::str_extract(GEOID,"(?<=US.{2}).{3}"),
                  GEOID = stringr::str_c(stringr::str_extract(GEOID,"^.{12}"),stringr::str_extract(GEOID,".{6}$"))) %>%
    dplyr::filter(ST %in% "53" & CNTY %in% "033") %>%
    dplyr::select(ENDYEAR, GEOID, dplyr::matches("T7"))

  county_2011_2015 <- read_chas_csv(county_2011_2015_fp) %>%
    dplyr::mutate(ENDYEAR = 2015L) %>%
    dplyr::filter(ST %in% "53" & CNTY %in% "033") %>%
    dplyr::select(ENDYEAR, GEOID, dplyr::matches("T7"))

  tr_2011_2015 <- read_chas_csv(tr_2011_2015_fp) %>%
    dplyr::mutate(ENDYEAR = 2015L) %>%
    dplyr::filter(ST %in% "53" & CNTY %in% "033") %>%
    dplyr::select(ENDYEAR, GEOID, dplyr::matches("T7"))



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
    dplyr::group_by(ENDYEAR, GEOID, VARIABLE, TYPE) %>%
    dplyr::mutate(N = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(-VAR) %>%
    tidyr::spread(TYPE, VALUE) %>%
    dplyr::group_by(ENDYEAR, GEOID, VARIABLE) %>%
    dplyr::summarize(ESTIMATE = sum(ESTIMATE, na.rm =  TRUE),
                     MOE = tidycensus::moe_sum(MOE,ESTIMATE, na.rm = TRUE)) %>%
    dplyr::ungroup()


  # FILTER CHAS DATA --------------------------------------------------------

  hud_chas_low_income <- hud_chas_table7_all_long %>%
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


