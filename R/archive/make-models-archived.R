
#' @title Make The Models
#' @description Temporary description.
#' @param model_table desc
#' @param indicators_comparison Tibble, Temporary description.
#' @param indicators_comparison_of_change desc
#' @param indicators_change_in_comparison desc
#' @return a `tibble`

make_model_vulnerability <- function(model_table, indicators_comparison){


  # FILTER INDICATORS -------------------------------------------------------

  model_table_dates <- model_table %>%
    dplyr::mutate(DATE_BEGIN = as.character(get_date_begin(DATE_BEGIN)),
                  DATE_END = as.character(get_date_end(DATE_END)))

  inds_vuln <- indicators_comparison %>%
    dplyr::filter(TOPIC %in% "VULNERABILITY") %>%
    dplyr::inner_join(model_table_dates,  # filtering join
                      by = c("SOURCE",
                             "DATE_BEGIN",
                             "DATE_END",
                             "TOPIC",
                             "INDICATOR",
                             "MEASURE_TYPE"))


  # CREATE MODEL INDICATORS -------------------------------------------------

  get_greater_inds <- function(data){
    # browser()

    # For each tract, returns each INDICATOR name (e.g., RACE, TENURE, etc.)
    # where the proportion of the tract is greater than the county

    if(is.na(data$N)){return(NA_character_)}

    if(sum(data$N) ==0){return("none")}

    data %>% dplyr::filter(N>0) %>% purrr::pluck("INDICATOR")
  }

  inds_vuln_long <- inds_vuln %>%
    dplyr::group_by(MODEL, TOPIC, GEOGRAPHY_ID, INDICATOR) %>%
    dplyr::summarise(N = sum(as.numeric(stringr::str_detect(INDICATOR_TYPE_MODEL, "GREATER"), na.rm = TRUE))) %>%
    tidyr::nest() %>%
    dplyr::mutate(N = purrr::map_dbl(data, ~ purrr::pluck(.x,"N") %>% sum),
                  LGL = N > 2,
                  DESC = purrr::map_chr(data, ~ get_greater_inds(.x) %>% stringr::str_c(collapse = ", "))
    ) %>%
    dplyr::select(-data,-N)

  inds_vuln_wide <- inds_vuln_long %>%
    tidyr::gather(VALUETYPE, VALUE, LGL, DESC) %>%
    tidyr::unite("TOPIC_VALUETYPE", c(TOPIC, VALUETYPE)) %>%
    spread(TOPIC_VALUETYPE, VALUE)

  # RETURN ------------------------------------------------------------------

  return(inds_vuln_wide)


}


make_model_demo_change <- function(model_table, indicators_comparison_of_change){


  # FILTER INDICATORS -------------------------------------------------------

  model_table_dates <- model_table %>%
    dplyr::mutate(DATE_GROUP_ID = as.character(DATE_END)) %>%
    dplyr::select(-SOURCE,-DATE_BEGIN,-DATE_END)

  inds_demo <- indicators_comparison_of_change %>%
    dplyr::filter(TOPIC %in% "DEMOGRAPHIC CHANGE") %>%
    dplyr::inner_join(model_table_dates,  # filtering join
                      by = c("DATE_GROUP_ID",
                             "TOPIC",
                             "INDICATOR",
                             "MEASURE_TYPE"))


  # CREATE MODEL INDICATORS -------------------------------------------------

  get_greater_inds <- function(data){
    # browser()

    # For each tract, returns each INDICATOR name (e.g., RACE, TENURE, etc.)
    # where the proportion of the tract is greater than the county

    if(is.na(data$N)){return(NA_character_)}

    if(sum(data$N) ==0){return("none")}

    data %>% dplyr::filter(N>0) %>% purrr::pluck("INDICATOR")
  }

  inds_demo_long <- inds_demo %>%
    dplyr::group_by(MODEL, TOPIC, GEOGRAPHY_ID, INDICATOR) %>%
    dplyr::summarise(N = sum(as.numeric(stringr::str_detect(INDICATOR_TYPE_MODEL, "GREATER"), na.rm = TRUE))) %>%
    tidyr::nest() %>%
    dplyr::mutate(N = purrr::map_dbl(data, ~ purrr::pluck(.x,"N") %>% sum),
                  DESC = purrr::map_chr(data, ~ get_greater_inds(.x) %>% stringr::str_c(collapse = ", ")),
                  LGL = dplyr::case_when(
                    N > 2 ~ TRUE,
                    N == 2 & stringr::str_detect(DESC, "RACE") & stringr::str_detect(DESC, "EDUCATION") ~ TRUE,
                    TRUE ~ FALSE
                  )
    ) %>%
    dplyr::select(-data, -N)

  inds_demo_wide <- inds_demo_long %>%
    dplyr::mutate(TOPIC = str_replace_all(TOPIC,"\\s","_")) %>% # remove the space in DEMOGRAPHIC CHANGE
    tidyr::gather(VALUETYPE, VALUE, LGL, DESC) %>%
    tidyr::unite("TOPIC_VALUETYPE", c(TOPIC, VALUETYPE)) %>%
    spread(TOPIC_VALUETYPE, VALUE)

  # RETURN ------------------------------------------------------------------

  return(inds_demo_wide)


}

make_model_housing_market <- function(model_table, indicators_comparison, indicators_change_in_comparison){



  # REVISE MODEL TABLE ------------------------------------------------------

  # add the 3-year spans to the model table (this should be migrated to make_model_table at some point)


  model_table_rev <- tibble::tribble(
    ~MODEL,               ~TOPIC, ~MEASURE_TYPE,       ~INDICATOR,    ~SOURCE, ~DATE_BEGIN, ~DATE_END,
    "PORTLAND",      "VULNERABILITY",     "PERCENT",           "RACE",      "ACS",       2013L,     2017L,
    "PORTLAND",      "VULNERABILITY",     "PERCENT",      "EDUCATION",      "ACS",       2013L,     2017L,
    "PORTLAND",      "VULNERABILITY",     "PERCENT",         "TENURE",      "ACS",       2013L,     2017L,
    "PORTLAND",      "VULNERABILITY",     "PERCENT",         "INCOME",     "CHAS",       2011L,     2015L,
    "PORTLAND", "DEMOGRAPHIC CHANGE",     "PERCENT",           "RACE",      "ACS",       2007L,     2011L,
    "PORTLAND", "DEMOGRAPHIC CHANGE",     "PERCENT",      "EDUCATION",      "ACS",       2007L,     2011L,
    "PORTLAND", "DEMOGRAPHIC CHANGE",     "PERCENT",         "TENURE",      "ACS",       2007L,     2011L,
    "PORTLAND", "DEMOGRAPHIC CHANGE",     "PERCENT",         "INCOME",     "CHAS",       2006L,     2010L,
    "PORTLAND", "DEMOGRAPHIC CHANGE",     "PERCENT",           "RACE",      "ACS",       2013L,     2017L,
    "PORTLAND", "DEMOGRAPHIC CHANGE",     "PERCENT",      "EDUCATION",      "ACS",       2013L,     2017L,
    "PORTLAND", "DEMOGRAPHIC CHANGE",     "PERCENT",         "TENURE",      "ACS",       2013L,     2017L,
    "PORTLAND", "DEMOGRAPHIC CHANGE",     "PERCENT",         "INCOME",      "ACS",       2013L,     2017L,
    "PORTLAND",     "HOUSING MARKET",      "MEDIAN",          "VALUE",     "LTDB",       2000L,     2000L,
    "PORTLAND",     "HOUSING MARKET",      "MEDIAN",          "VALUE",      "ACS",       2006L,     2010L,
    "PORTLAND",     "HOUSING MARKET",      "MEDIAN",          "VALUE",      "ACS",       2013L,     2017L,
    "COO_ORIGINAL",      "VULNERABILITY",     "PERCENT",           "RACE",      "ACS",       2011L,     2015L,
    "COO_ORIGINAL",      "VULNERABILITY",     "PERCENT",      "EDUCATION",      "ACS",       2011L,     2015L,
    "COO_ORIGINAL",      "VULNERABILITY",     "PERCENT",         "TENURE",      "ACS",       2011L,     2015L,
    "COO_ORIGINAL",      "VULNERABILITY",     "PERCENT",         "INCOME",      "ACS",       2011L,     2015L,
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",     "PERCENT",           "RACE",      "ACS",       2006L,     2010L,
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",     "PERCENT",      "EDUCATION",      "ACS",       2006L,     2010L,
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",     "PERCENT",         "TENURE",      "ACS",       2006L,     2010L,
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",     "PERCENT",         "INCOME",      "ACS",       2006L,     2010L,
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",     "PERCENT",           "RACE",      "ACS",       2011L,     2015L,
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",     "PERCENT",      "EDUCATION",      "ACS",       2011L,     2015L,
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",     "PERCENT",         "TENURE",      "ACS",       2011L,     2015L,
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",     "PERCENT",         "INCOME",      "ACS",       2011L,     2015L,
    "COO_ORIGINAL",     "HOUSING MARKET",      "MEDIAN", "ASSESSED VALUE", "ASSESSOR",       2005L,     2005L,
    "COO_ORIGINAL",     "HOUSING MARKET",      "MEDIAN", "ASSESSED VALUE", "ASSESSOR",       2010L,     2010L,
    "COO_ORIGINAL",     "HOUSING MARKET",      "MEDIAN", "ASSESSED VALUE", "ASSESSOR",       2015L,     2015L,
    "COO_ORIGINAL_UPDATED",      "VULNERABILITY",     "PERCENT",           "RACE",      "ACS",       2013L,     2017L,
    "COO_ORIGINAL_UPDATED",      "VULNERABILITY",     "PERCENT",      "EDUCATION",      "ACS",       2013L,     2017L,
    "COO_ORIGINAL_UPDATED",      "VULNERABILITY",     "PERCENT",         "TENURE",      "ACS",       2013L,     2017L,
    "COO_ORIGINAL_UPDATED",      "VULNERABILITY",     "PERCENT",         "INCOME",      "ACS",       2013L,     2017L,
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",     "PERCENT",           "RACE",      "ACS",       2007L,     2011L,
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",     "PERCENT",      "EDUCATION",      "ACS",       2007L,     2011L,
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",     "PERCENT",         "TENURE",      "ACS",       2007L,     2011L,
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",     "PERCENT",         "INCOME",      "ACS",       2007L,     2011L,
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",     "PERCENT",           "RACE",      "ACS",       2013L,     2017L,
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",     "PERCENT",      "EDUCATION",      "ACS",       2013L,     2017L,
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",     "PERCENT",         "TENURE",      "ACS",       2013L,     2017L,
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",     "PERCENT",         "INCOME",      "ACS",       2013L,     2017L,
    "COO_ORIGINAL_UPDATED",     "HOUSING MARKET",      "MEDIAN", "ASSESSED VALUE", "ASSESSOR",       2005L,     2005L,
    "COO_ORIGINAL_UPDATED",     "HOUSING MARKET",      "MEDIAN", "ASSESSED VALUE", "ASSESSOR",       2010L,     2010L,
    "COO_ORIGINAL_UPDATED",     "HOUSING MARKET",      "MEDIAN", "ASSESSED VALUE", "ASSESSOR",       2018L,     2018L,
    "COO_REVISED",      "VULNERABILITY",     "PERCENT",           "RACE",      "ACS",       2013L,     2017L,
    "COO_REVISED",      "VULNERABILITY",     "PERCENT",      "EDUCATION",      "ACS",       2013L,     2017L,
    "COO_REVISED",      "VULNERABILITY",     "PERCENT",         "TENURE",      "ACS",       2013L,     2017L,
    "COO_REVISED",      "VULNERABILITY",     "PERCENT",         "INCOME",     "CHAS",       2011L,     2015L,
    "COO_REVISED", "DEMOGRAPHIC CHANGE",     "PERCENT",           "RACE",      "ACS",       2007L,     2011L,
    "COO_REVISED", "DEMOGRAPHIC CHANGE",     "PERCENT",      "EDUCATION",      "ACS",       2007L,     2011L,
    "COO_REVISED", "DEMOGRAPHIC CHANGE",     "PERCENT",         "TENURE",      "ACS",       2007L,     2011L,
    "COO_REVISED", "DEMOGRAPHIC CHANGE",     "PERCENT",         "INCOME",     "CHAS",       2006L,     2010L,
    "COO_REVISED", "DEMOGRAPHIC CHANGE",     "PERCENT",           "RACE",      "ACS",       2013L,     2017L,
    "COO_REVISED", "DEMOGRAPHIC CHANGE",     "PERCENT",      "EDUCATION",      "ACS",       2013L,     2017L,
    "COO_REVISED", "DEMOGRAPHIC CHANGE",     "PERCENT",         "TENURE",      "ACS",       2013L,     2017L,
    "COO_REVISED", "DEMOGRAPHIC CHANGE",     "PERCENT",         "INCOME",     "CHAS",       2011L,     2015L,
    # "COO_REVISED",     "HOUSING MARKET",      "MEDIAN",           "RENT",      "ACS",       2006L,     2010L,
    # "COO_REVISED",     "HOUSING MARKET",      "MEDIAN",           "RENT",      "ACS",       2013L,     2017L,
    "COO_REVISED",     "HOUSING MARKET",      "MEDIAN", "ASSESSED VALUE", "ASSESSOR",       2005L,     2005L, # changed
    "COO_REVISED",     "HOUSING MARKET",      "MEDIAN", "ASSESSED VALUE", "ASSESSOR",       2010L,     2010L,# changed
    "COO_REVISED",     "HOUSING MARKET",      "MEDIAN", "ASSESSED VALUE", "ASSESSOR",       2018L,     2018L,# changed
    "COO_REVISED",     "HOUSING MARKET",      "MEDIAN",     "SALE PRICE", "ASSESSOR",       2013L,     2015L, # changed
    "COO_REVISED",     "HOUSING MARKET",      "MEDIAN",     "SALE PRICE", "ASSESSOR",       2016L,     2018L, # changed
    "COO_REVISED",     "HOUSING MARKET",     "PERCENT",      "SALE RATE", "ASSESSOR",       2013L,     2015L, # changed
    "COO_REVISED",     "HOUSING MARKET",     "PERCENT",      "SALE RATE", "ASSESSOR",       2016L,     2018L, # changed
    "OTHER",     "VULENERABILITY",     "PERCENT",    "COST BURDEN",      "ACS",       2013L,     2017L,
    "OTHER",     "VULENERABILITY",     "PERCENT",    "COST BURDEN",      "ACS",       2007L,     2011L,
    "OTHER",      "MISCELLANEOUS",       "COUNT",     "POPULATION",      "ACS",       2006L,     2010L,
    "OTHER",      "MISCELLANEOUS",       "COUNT",     "POPULATION",      "ACS",       2013L,     2017L
  )

  # FILTER INDICATORS -------------------------------------------------------


  housing_ind_var <- list(indicators_comparison,indicators_change_in_comparison) %>%
    purrr::map_dfr(c) %>%
    dplyr::filter(TOPIC %in% "HOUSING MARKET") %>%
    select(INDICATOR, VARIABLE) %>%
    dplyr::distinct()


  model_table_dates_relative <- model_table_rev %>%
    dplyr::mutate(INDICATOR = stringr::str_replace_all(INDICATOR,"\\s","_"),
                  DATE_GROUP_ID = str_c(DATE_BEGIN,DATE_END,sep = "_")) %>%
    dplyr::select(-SOURCE,-DATE_BEGIN,-DATE_END) %>%
    dplyr::inner_join(housing_ind_var, by = "INDICATOR")

model_table_dates_join <- model_table_dates_relative %>%
  dplyr::mutate(DATE_JOIN = stringr::str_extract(DATE_GROUP_ID,".{4}$"))

change_endyears_trim <- change_endyears %>% dplyr::select(BEGIN,END)

model_table_dates_change <- model_table_dates_join %>%
  dplyr::left_join(change_endyears_trim, by = c(DATE_JOIN = "BEGIN")) %>%
  dplyr::transmute(MODEL,
                   TOPIC,
                   MEASURE_TYPE,
                   INDICATOR,
                   VARIABLE,
                   DATE_GROUP_ID = stringr::str_c(DATE_JOIN,END,sep = "_"))

  model_table_dates_all <- list(model_table_dates_relative,
                                model_table_dates_change) %>%
    purrr::map_dfr(c) %>%
    dplyr::distinct()


  get_daterange <- function(begin, end){ stringr::str_c(lubridate::year(begin),lubridate::year(end), sep = "_")}

  model_portland_housing <-


    housing_variable_list <- c("")

inds_housing_model <- list(indicators_comparison,indicators_change_in_comparison) %>%
    purrr::map_dfr(c)  %>%
    dplyr::filter(TOPIC %in% "HOUSING MARKET") %>%
    dplyr::filter(! DATE_RANGE_TYPE %in% "one quarter") %>%
    dplyr::mutate(DATE_GROUP_ID = get_daterange(DATE_BEGIN,DATE_END)) %>%
    dplyr::inner_join(model_table_dates_all,  # filtering join
                    by = c("DATE_GROUP_ID",
                           "TOPIC",
                           "INDICATOR",
                           "VARIABLE",
                           "MEASURE_TYPE")) %>%
    dplyr::filter(VARIABLE %in% c("B25077",  # this is an important filter
                                  "SR_SF",
                                  "SP_SQFT_SF",
                                  "ATV_SF")) %>%
  dplyr::distinct()

  check_inds_housing_model <- function(){
  inds_housing_model %>%
    count(MODEL,
          TOPIC,
          INDICATOR,
          VARIABLE,
          MEASURE_TYPE,
          DATE_GROUP_ID) %>% View()
    }


  # CREATE MODEL INDICATORS -------------------------------------------------

  test_fun <- function(data, indicator, indtype){
    # browser()

    # For each tract, returns each INDICATOR name (e.g., RACE, TENURE, etc.)
    # where the proportion of the tract is greater than the county

    if(data$GEOGRAPHY_TYPE %in% c("county","community")){return(NA_character_)}

    if(nrow(data) > 1){stop("Too many rows")}

    stringr::str_c(data$INDICATOR_TYPE_MODEL, indicator, sep = " ")
    # if(indicator %in% "SALE_RATE" & indtype %in% "RELATIVE"){data %>% purrr::pluck("")}
  }

  inds_housing_long <- inds_housing_model %>%
    nest(-MODEL, -TOPIC, -GEOGRAPHY_ID, -INDICATOR, -INDICATOR_TYPE,-VARIABLE,-DATE_GROUP_ID) %>%
    mutate(NROW = map_int(data,nrow)) %>%
    filter(NROW == 1) %>%
    dplyr::mutate(DESC = purrr::pmap_chr(list(data, INDICATOR, INDICATOR_TYPE), test_fun)) %>%
    dplyr::select(-data,-NROW)


  inds_housing_wide <- inds_housing_long %>%
    dplyr::mutate(TOPIC = str_replace_all(TOPIC,"\\s","_")) %>% # remove the space in DEMOGRAPHIC CHANGE
    tidyr::gather(VALUETYPE, VALUE, DESC) %>%
    tidyr::unite("TOPIC_VALUETYPE", c(TOPIC, VALUETYPE)) %>%
    spread(TOPIC_VALUETYPE, VALUE)



# SPILLOVER ---------------------------------------------------------------

tracts_sf <- census_tracts_2016 %>%
  dplyr::filter(! GEOID %in% excluded_tract_geoids) %>% # this removes the Puget Sound census tract (no census data)
  dplyr::select(GEOGRAPHY_ID = GEOID)

tract_summary_stats_sf <-  inds_housing_wide %>%
  dplyr::right_join(tracts_sf, by = "GEOGRAPHY_ID") %>%
  sf::st_sf()


is_spillover <- function(data){

  "HIGH ASSESSED_VALUE" %in% data$HOUSING_MARKET_DESC || "HIGH -> HIGH VALUE" %in% data$HOUSING_MARKET_DESC

}

tracts_spillover <-  tract_summary_stats_sf %>%
  sf::st_join(tract_summary_stats_sf, sf::st_touches, suffix = c("","_NEIGHBOR")) %>%
  sf::st_set_geometry(NULL) %>%
  tibble::as_tibble() %>%
  dplyr::group_by(GEOID) %>%
  tidyr::nest() %>%
  dplyr::mutate(SPILLOVER_LGL = purrr::map_lgl(data, is_spillover)) %>%
  dplyr::select(-data)



  # RETURN ------------------------------------------------------------------

  return(inds_demo_wide)


}

