
#' @title Make A Tibble of The Project's ACS Variables
#' @description Return a `tibble` of all of the American Community Survey data variables
#'   that are used in the Neighborhood Change Typology model for both 5-year spans
#'   (2006-2010 and 2012-2016).
#' @param model_table Tibble, the `model_table` object
#' @param acs_tables Tibble, the `acs_table` object
#' @param path Character, the path or connection to write to.
#' @return a `tibble`

#' @rdname acs-data
#' @export
prepare_acs_data <- function(model_table, acs_tables, path){


  # GET DATA ----------------------------------------------------------------

  all_census_vars <- tidycensus::load_variables(2016, "acs5", cache = TRUE) %>%
    dplyr::transmute(NAME = stringr::str_extract(name,".*(?=_\\d{3})"), # regex lookahead for '_001'
                     FULL_NAME = name,
                     LABEL = label,
                     TOPIC = concept)

  data_key <- dplyr::inner_join(model_table, acs_tables, by = "INDICATOR") %>%
    dplyr::inner_join(all_census_vars, by = "NAME")

  geographies <- c("tract","county")

  years <- data_key %>%
    dplyr::filter(ENDYEAR >= 2010L) %>% # the tidycensus package only queries data from 2010 - present
    dplyr::arrange(ENDYEAR) %>%
    dplyr::pull(ENDYEAR) %>%
    unique()

  variables_types <- data_key %>%
    dplyr::select(FULL_NAME, MEASURE_TYPE = MEASURE_TYPE.x) %>%
    dplyr::distinct()

  get_data <- function(geographies, years){
    tidycensus::get_acs(geography = geographies,
                        variables = variables_types$FULL_NAME,
                        year = years,
                        state = "53",
                        county = "033",
                        survey = "acs5") %>%
      janitor::clean_names(case = "screaming_snake") %>%
      dplyr::left_join(variables_types, by = c(VARIABLE = "FULL_NAME")) %>% # join the MEASURE_TYPE column
      dplyr::mutate(MEASURE_TYPE = dplyr::case_when(
        MEASURE_TYPE %in% "PERCENT" ~ "count",  # the count variables will be transformed into percent variables in the model
        MEASURE_TYPE %in% "VALUE" ~ "value",
        TRUE ~ NA_character_
      )) %>%
      dplyr::transmute(GEOID,
                       NAME,
                       GEOGRAPHY = geographies,
                       ENDYEAR = years,
                       VARIABLE,
                       MEASURE_TYPE,
                       ESTIMATE,
                       MOE
      )

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
    dplyr::mutate(GEOID = as.character(GEOID))

  return(acs_data)

}


