
#' @title Make Housing Market Indicators
#' @description Temporary description
#' @param census_tracts_2016 Simple feature, Temporary description
#' @param excluded_tract_geoids Character string, Temporary description
#' @param parcel_boundaries Simple feature, Temporary description
#' @param parcel_tract_overlay Tibble, Temporary description
#' @param housing_market_parcel_value Tibble, Temporary description
#' @param housing_market_parcel_appr Tibble, Temporary description
#' @return a simple feature
#' @export
make_housing_market_indicators <- function(census_tracts_2016, excluded_tract_geoids, parcel_boundaries, parcel_tract_overlay, housing_market_parcel_value, housing_market_parcel_appr
){

  # Create a "long" data object with PIN, TAX_YEAR, and the two values of interest (total value, appreciation)

parcel_appr <- housing_market_parcel_appr %>%
  dplyr::transmute(PIN,
                   HOME_TYPE,
                   SF_LGL,
                   CONDO_LGL,
                   SF_COMPLETE_LGL,
                   CONDO_COMPLETE_LGL,
                   TAX_YEAR = TAX_YEAR_RANGE,
                   TYPE = "APPRECIATION",
                   VALUE = APPRECIATION)

parcel_value <- housing_market_parcel_value %>%
  dplyr::transmute(PIN,
                   HOME_TYPE,
                   SF_LGL,
                   CONDO_LGL,
                   SF_COMPLETE_LGL,
                   CONDO_COMPLETE_LGL,
                   TAX_YEAR = as.character(TAX_YEAR),
                   TYPE = "VALUE_TOTAL",
                   VALUE = VALUE_TOTAL)

parcel_value_appr <- dplyr::bind_rows(parcel_value, parcel_appr)


# join the census tracts and "widen" the data

tract_summary_stats <- parcel_value_appr %>%
  dplyr::mutate(PIN_JOIN = dplyr::case_when(
    CONDO_LGL ~ stringr::str_pad(stringr::str_extract(PIN, "^.{6}"),width = 10,side = "right",pad = "0"),
    TRUE ~ PIN
  )) %>%
  dplyr::inner_join(parcel_tract_overlay, c(PIN_JOIN = "PIN")) %>% # this removes any records *without* a matching PIN in 2018
  dplyr::mutate(TAX_YEAR = stringr::str_replace(TAX_YEAR, "YEAR_","")) %>%
  dplyr::group_by(GEOID, TYPE, TAX_YEAR) %>%
  dplyr::summarise(COUNT = sum(!is.na(VALUE)),
                   MEDIAN = median(VALUE, na.rm = TRUE)) %>%
  dplyr::group_by(TYPE, TAX_YEAR) %>%
  dplyr::mutate(QUINTILE = dplyr::ntile(MEDIAN, n = 5),
                GROUP = dplyr::if_else(QUINTILE > 3, "HIGH", "LOW/MED")) %>%
  dplyr::ungroup() %>%
  tidyr::gather(SUMMARY_STAT, SUMMARY_VALUE, c(COUNT, MEDIAN, QUINTILE, GROUP)) %>%
  tidyr::unite(TYPE_SUMMARY, c("TYPE","SUMMARY_STAT", "TAX_YEAR")) %>%
  tidyr::spread(TYPE_SUMMARY, SUMMARY_VALUE) %>%
  dplyr::mutate_at(dplyr::vars(matches("MEDIAN")),as.double) %>%
  dplyr::mutate_at(dplyr::vars(matches("COUNT|QUINTILE")),as.integer)

# Check neighbors
# probably want to write a function, create a nested tibble, and use pmap

tracts_sf <- census_tracts_2016 %>%
  dplyr::filter(! GEOID %in% excluded_tract_geoids) %>% # this removes the Puget Sound census tract (no census data)
  dplyr::select(GEOID)

tract_summary_stats_sf <-  tract_summary_stats %>%
  dplyr::right_join(tracts_sf, by = "GEOID") %>%
  sf::st_sf()


is_spillover <- function(data){

  "HIGH" %in% data$VALUE_TOTAL_GROUP_2018_NEIGHBOR || "HIGH" %in% data$APPRECIATION_GROUP_YEARS_10_18_NEIGHBOR

}

tracts_spillover <-  tract_summary_stats_sf %>%
  sf::st_join(tract_summary_stats_sf, sf::st_touches, suffix = c("","_NEIGHBOR")) %>%
  sf::st_set_geometry(NULL) %>%
  tibble::as_tibble() %>%
  dplyr::group_by(GEOID) %>%
  tidyr::nest() %>%
  dplyr::mutate(SPILLOVER_LGL = purrr::map_lgl(data, is_spillover)) %>%
  dplyr::select(-data)


check_spillover <- function(){

  tmp <-
    tracts_spillover %>%
    dplyr::left_join(tracts_sf, by = "GEOID") %>%
    sf::st_sf()

  mapview(tmp, zcol = "SPILLOVER_LGL")

  # there seems to be some missing tracts -- U District, Downtown Seattle, etc.


}


housing_market_indicators_sf <- list(tract_summary_stats_sf, tracts_spillover) %>%
  purrr::reduce(dplyr::left_join, by = "GEOID") %>%
  dplyr::mutate(HOUSING_MARKET_TYPE = dplyr::case_when(
    VALUE_TOTAL_GROUP_2005 %in% "LOW/MED" & VALUE_TOTAL_GROUP_2018 %in% "HIGH" & APPRECIATION_GROUP_YEARS_05_18 %in% "HIGH" ~ "appreciated",
    VALUE_TOTAL_GROUP_2018 %in% "LOW/MED" & APPRECIATION_GROUP_YEARS_10_18 %in% "HIGH" ~ "accelerating",
    VALUE_TOTAL_GROUP_2018 %in% "LOW/MED" & APPRECIATION_GROUP_YEARS_10_18 %in% "LOW/MED" & SPILLOVER_LGL ~ "adjacent",
    TRUE ~ "none"
  ))

check_housing_market_inds <- function(){
  tmp <- housing_market_indicators_sf %>%
  mutate(HOUSING_MARKET_TYPE = factor(HOUSING_MARKET_TYPE,
                                      levels = c(NA_character_,"adjacent", "accelerating", "appreciated"),
                                      ordered = TRUE))

  mapview(tmp, zcol = "HOUSING_MARKET_TYPE")
}

housing_market_indicators <- housing_market_indicators_sf %>%
  sf::st_set_geometry(NULL) %>%
  tibble::as_tibble()

  return(housing_market_indicators)

}
