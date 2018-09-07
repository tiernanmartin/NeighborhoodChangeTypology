
#' @title Make A Tibble of The Project's ACS Variables
#' @description Return a `tibble` of all of the American Community Survey data variables
#'   that are used in the Neighborhood Change Typology model for both 5-year spans
#'   (2006-2010 and 2012-2016).
#' @param acs_tables Tibble, the `acs_table` object
#' @param path Character, the path or connection to write to.
#' @return a `tibble`
#' @seealso make_acs_table

#' @rdname acs-data
#' @export
prepare_acs_data <- function(acs_tables, path){


# GET DATA ----------------------------------------------------------------

  all_census_vars <- tidycensus::load_variables(2016, "acs5", cache = TRUE) %>%
    dplyr::transmute(NAME = stringr::str_extract(name,".*(?=_\\d{3})"), # regex lookahead for '_001'
                     FULL_NAME = name,
                     LABEL = label,
                     TOPIC = concept)

  target_census_vars <- all_census_vars %>%
    dplyr::semi_join(acs_tables, by = "NAME")

  variables <- target_census_vars %>% dplyr::pull(FULL_NAME)

  geographies <- c("tract","county")

  years <- c(2010, 2016)

  get_data <- function(geographies, years){
    tidycensus::get_acs(geography = geographies,
                        variables = variables,
                        year = years,
                        state = "53",
                        county = "033",
                        survey = "acs5") %>%
      dplyr::mutate(ENDYEAR = years)
  }

  acs_data_prep <- list(geographies = geographies,
                   years = years) %>%
    purrr::cross_df() %>%
    purrr::pmap_dfr(get_data) %>%
    janitor::clean_names(case = "screaming_snake")



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


