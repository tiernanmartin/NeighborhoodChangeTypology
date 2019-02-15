
#' @title Make A Tibble of The Project's ACS Variables
#' @description Return a `tibble` of all of the American Community Survey data variables
#'   that are used in the Neighborhood Change Typology model for both 5-year spans
#'   (2006-2010 and 2013-2017).
#' @param data_template Tibble, the `data_template` object
#' @param model_table_inputs Tibble, the `model_table_inputs` object
#' @param acs_tables Tibble, the `acs_table` object
#' @param path Character, the path or connection to write to.
#' @return a `tibble`

#' @rdname acs-data
#' @export
prepare_acs_data <- function(data_template, model_table_inputs, acs_tables, path){


  # GET DATA ----------------------------------------------------------------

  all_census_vars <- tidycensus::load_variables(2017, "acs5", cache = TRUE) %>%
    dplyr::transmute(VARIABLE = stringr::str_extract(name,".*(?=_\\d{3})"), # regex lookahead for '_001'
                     VARIABLE_SUBTOTAL = name,
                     LABEL = label,
                     TOPIC = concept)

  data_key <- model_table_inputs %>%
    dplyr::select(MODEL, TOPIC, -MEASURE_TYPE, INDICATOR, SOURCE, DATE_END) %>%
    dplyr::inner_join(dplyr::select(acs_tables,-TOPIC), by = "INDICATOR") %>%
    dplyr::inner_join(all_census_vars, by = "VARIABLE")

  variable_subtotal_descriptions <- all_census_vars %>%
    dplyr::transmute(VARIABLE_SUBTOTAL,
                     VARIABLE_SUBTOTAL_DESC = LABEL)

  geographies <- c("tract","county")

  years <- data_key %>%
    dplyr::filter(as.numeric(DATE_END) >= 2010L) %>% # the tidycensus package only queries data from 2010 - present
    dplyr::arrange(DATE_END) %>%
    dplyr::pull(DATE_END) %>%
    purrr::map_dbl(as.numeric) %>%
    unique()

  variables_types <- data_key %>%
    dplyr::select(VARIABLE_SUBTOTAL, MEASURE_TYPE) %>%
    dplyr::distinct()

  get_data <- function(geographies, years){

    acs_data_download <- tidycensus::get_acs(geography = geographies,
                                             variables = variables_types$VARIABLE_SUBTOTAL,
                                             year = years,
                                             state = "53",
                                             county = "033",
                                             survey = "acs5",
                                             geometry = FALSE) %>%
      janitor::clean_names(case = "screaming_snake") %>%
      dplyr::rename(VARIABLE_SUBTOTAL = VARIABLE) %>% # VARIABLE is reserved for the ACS table name (e.g., B03002)
      dplyr::left_join(variables_types, by = "VARIABLE_SUBTOTAL")

    acs_data_transformed <- acs_data_download %>%
      dplyr::left_join(variable_subtotal_descriptions, by = "VARIABLE_SUBTOTAL") %>%
      dplyr::transmute(SOURCE = "ACS",
                       GEOGRAPHY_ID = GEOID,
                       GEOGRAPHY_ID_TYPE = "GEOID",
                       GEOGRAPHY_NAME = NAME,
                       GEOGRAPHY_TYPE = geographies,
                       DATE_BEGIN = get_date_begin(as.numeric(years) - 4L), # creates the first day of the 5-year span
                       DATE_END = get_date_end(years), # creates the last day of the 5-year span
                       DATE_GROUP_ID = create_range_year(DATE_BEGIN, DATE_END),
                       DATE_RANGE = create_range_date(DATE_BEGIN, DATE_END),
                       DATE_RANGE_TYPE = "five years",
                       VARIABLE = stringr::str_extract(VARIABLE_SUBTOTAL,".*(?=_\\d{3})"),
                       VARIABLE_SUBTOTAL,
                       VARIABLE_SUBTOTAL_DESC,
                       MEASURE_TYPE,
                       ESTIMATE,
                       MOE
      ) %>%
      dplyr::mutate_if(lubridate::is.Date, as.character) # convert Date cols to character

    # Use `full_join()` to transform the acs data to the
    # column format in `data_template`
    acs_data_formatted <- data_template %>%
      dplyr::full_join(acs_data_transformed, by = c("SOURCE",
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
    acs_data <- acs_data_formatted

    return(acs_data)

  }

  acs_data_prep <- list(geographies = geographies,
                        years = years) %>%
    purrr::cross_df() %>%
    purrr::pmap_dfr(get_data)




  # WRITE DATA --------------------------------------------------------------

  readr::write_csv(x = acs_data_prep, path = path)

  # RETURN ------------------------------------------------------------------

  acs_data_prep_status <- NeighborhoodChangeTypology::get_modified_time(path)

  return(acs_data_prep_status)

}


#' @rdname acs-data
#' @export
make_acs_data <- function(path){

  acs_data <- suppressWarnings(suppressMessages(readr::read_csv(path))) %>%
    dplyr::mutate(GEOGRAPHY_ID = as.character(GEOGRAPHY_ID),
                  DATE_BEGIN = as.character(DATE_BEGIN),
                  DATE_END = as.character(DATE_END)
                  )

  return(acs_data)

}


