
#' @title Make A Tibble of The Project's ACS Variables
#' @description Return a `tibble` of all of the American Community Survey data variables
#'   that are used in the Neighborhood Change Typology model for both 5-year spans
#'   (2006-2010 and 2012-2016).
#' @param data_template Tibble, the `data_template` object
#' @param model_table Tibble, the `model_table` object
#' @param acs_tables Tibble, the `acs_table` object
#' @param path Character, the path or connection to write to.
#' @return a `tibble`

#' @rdname acs-data
#' @export
prepare_acs_data <- function(data_template, model_table, acs_tables, path){


  # GET DATA ----------------------------------------------------------------

  all_census_vars <- tidycensus::load_variables(2016, "acs5", cache = TRUE) %>%
    dplyr::transmute(VARIABLE = stringr::str_extract(name,".*(?=_\\d{3})"), # regex lookahead for '_001'
                     VARIABLE_SUBTOTAL = name,
                     LABEL = label,
                     TOPIC = concept)

  data_key <- model_table %>%
    dplyr::select(MODEL, TOPIC, -MEASURE_TYPE, INDICATOR, SOURCE, ENDYEAR) %>%
    dplyr::inner_join(dplyr::select(acs_tables,-TOPIC), by = "INDICATOR") %>%
    dplyr::inner_join(all_census_vars, by = "VARIABLE")

  variable_subtotal_descriptions <- all_census_vars %>%
    dplyr::transmute(VARIABLE_SUBTOTAL,
           VARIABLE_SUBTOTAL_DESC = LABEL)

  geographies <- c("tract","county")

  years <- data_key %>%
    dplyr::filter(ENDYEAR >= 2010L) %>% # the tidycensus package only queries data from 2010 - present
    dplyr::arrange(ENDYEAR) %>%
    dplyr::pull(ENDYEAR) %>%
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
                                             survey = "acs5") %>%
      janitor::clean_names(case = "screaming_snake") %>%
      dplyr::rename(VARIABLE_SUBTOTAL = VARIABLE) %>% # VARIABLE is reserved for the ACS table name (e.g., B03002)
      dplyr::left_join(variables_types, by = "VARIABLE_SUBTOTAL")


    # Use `full_join()` to transform the acs data to the
    # column format in `data_template`
    acs_data_formatted <- data_template %>%
      dplyr::select(-VARIABLE_SUBTOTAL_DESC) %>%
      dplyr::full_join(acs_data_download,
                       c(GEOGRAPHY_ID = "GEOID",
                         GEOGRAPHY_NAME = "NAME",
                         "VARIABLE_SUBTOTAL",
                         "MEASURE_TYPE",
                         "ESTIMATE",
                         "MOE")) %>%
      dplyr::left_join(variable_subtotal_descriptions, by = "VARIABLE_SUBTOTAL") %>%
      dplyr::transmute(SOURCE = "ACS",
                       GEOGRAPHY_ID,
                       GEOGRAPHY_ID_TYPE = "GEOID",
                       GEOGRAPHY_NAME,
                       GEOGRAPHY_TYPE = geographies,
                       ENDYEAR = years,
                       VARIABLE = stringr::str_extract(VARIABLE_SUBTOTAL,".*(?=_\\d{3})"),
                       VARIABLE_SUBTOTAL,
                       VARIABLE_SUBTOTAL_DESC,
                       MEASURE_TYPE,
                       ESTIMATE,
                       MOE
      )

    return(acs_data_formatted)

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
    dplyr::mutate(GEOGRAPHY_ID = as.character(GEOGRAPHY_ID))

  return(acs_data)

}


