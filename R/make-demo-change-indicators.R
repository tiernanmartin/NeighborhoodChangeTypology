
#' @title Make The Demographic Change Indicators
#' @description Temporary description
#' @param acs_indicators Tibble, Temporary description.
#' @return a `tibble`
#' @export
make_demo_change_indicators <- function(acs_indicators){

  get_greater_vars <- function(data){

    # For each tract, returns each TOPIC name (e.g., RACE, TENURE, etc.)
    # where the proportion of the tract is greater than the county

    if(sum(data$N) ==0){return("none")}

    data %>% dplyr::filter(N>0) %>% purrr::pluck("TOPIC")
  }

  demo_change_comparison <- acs_indicators %>%
    dplyr::select(-GREATER_THAN_COUNTY) %>%
    dplyr::mutate(COUNTY_PROPORTION = 1 - COUNTY_PROPORTION,
           TRACT_PROPORTION = 1 - TRACT_PROPORTION) %>%
    tidyr::gather(VAR, VAL, tidyselect::matches("COUNTY|TRACT")) %>%
    tidyr::unite(VAR_YEAR, VAR, ENDYEAR) %>%
    tidyr::spread(VAR_YEAR,VAL) %>%
    dplyr::mutate(COUNTY_CHANGE = COUNTY_PROPORTION_2016 - COUNTY_PROPORTION_2010,
           TRACT_CHANGE = TRACT_PROPORTION_2016 - TRACT_PROPORTION_2010,
           GREATER_THAN_COUNTY = TRACT_CHANGE >= COUNTY_CHANGE
           )

  demo_change_indicators <- demo_change_comparison %>%
    dplyr::group_by(GEOID, TOPIC) %>%
    dplyr::summarise(N = sum(as.numeric(GREATER_THAN_COUNTY), na.rm = TRUE)) %>%
    tidyr::nest() %>%
   dplyr::mutate(N = purrr::map_dbl(data, ~ purrr::pluck(.x,"N") %>% sum),
           DEMO_CHANGE_INDS = purrr::map_chr(data, ~ get_greater_vars(.x) %>% stringr::str_c(collapse = ", ")),
           DEMO_CHANGE_LGL = dplyr::case_when(
             N > 2 ~ TRUE,
             N == 2 & any(stringr::str_detect(DEMO_CHANGE_INDS, "RACE|EDUCATION")) ~ TRUE,
             TRUE ~ FALSE
           )
           ) %>%
    dplyr::transmute(GEOID, DEMO_CHANGE_TR_CNT = N, DEMO_CHANGE_LGL, DEMO_CHANGE_INDS)

return(demo_change_indicators)


}
