
#' @title Make A Tibble of The Project's ACS Variables
#' @description Return a `tibble` of all of the American Community Survey data variables
#'   that are used in the Neighborhood Change Typology model for both 5-year spans
#'   (2006-2010 and 2012-2016).
#' @param acs_tables Tibble, the `acs_table` object
#' @return a `tibble`
#' @importFrom tidycensus load_variables
#' @importFrom tidycensus get_acs
#' @importFrom stringr str_extract
#' @importFrom purrr map_df
#' @importFrom dplyr transmute
#' @importFrom dplyr pull
#' @importFrom dplyr semi_join
#' @importFrom dplyr mutate
#' @seealso make_acs_table
#' @export
make_acs_data <- function(acs_tables){

  all_census_vars <- tidycensus::load_variables(2016, "acs5", cache = TRUE) %>%
    dplyr::transmute(NAME = stringr::str_extract(name,".*(?=_\\d{3})"), # regex lookahead for '_001'
              FULL_NAME = name,
              LABEL = label,
              TOPIC = concept)

  target_census_vars <- all_census_vars %>%
    dplyr::semi_join(acs_tables, by = "NAME")

  variables <- target_census_vars %>% dplyr::pull(FULL_NAME)

  years <- c(2010, 2016)

  get_tract_data <- function(years){
    tidycensus::get_acs(geography = "tract",
            variables = variables,
            year = years,
            state = "53",
            county = "033",
            survey = "acs5") %>%
      dplyr::mutate(ENDYEAR = years)
  }

  acs_data <- purrr::map_df(years, get_tract_data)
}





