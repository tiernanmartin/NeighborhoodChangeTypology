
#' @title Make The Portland Model
#' @description Temporary description.
#' @param model_table desc
#' @param indicators_comparison Tibble, Temporary description.
#' @return a `tibble`

#' @rdname portland-model
#' @export
make_portland_model_vulnerability <- function(model_table, indicators_comparison){


  # FILTER INDICATORS -------------------------------------------------------

  model_table_portland <- model_table %>%
    dplyr::filter(MODEL %in% "PORTLAND") %>%
    dplyr::mutate(DATE_BEGIN = as.character(get_date_begin(DATE_BEGIN)),
                  DATE_END = as.character(get_date_end(DATE_END)))

  inds <- indicators_comparison %>%
    dplyr::filter(TOPIC %in% "VULNERABILITY") %>%
    dplyr::inner_join(model_table_portland,  # filtering join
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

  inds %>%
    dplyr::group_by(MODEL, TOPIC, GEOGRAPHY_ID, DATE_GROUP_ID, INDICATOR) %>%
    dplyr::summarise(N = sum(as.numeric(stringr::str_detect(INDICATOR_TYPE_MODEL, "GREATER"), na.rm = TRUE))) %>%
    tidyr::nest() %>%
    dplyr::mutate(N = purrr::map_dbl(data, ~ purrr::pluck(.x,"N") %>% sum),
                  LGL = N > 2,
                  VULNERABLE_INDS = purrr::map_chr(data, ~ get_greater_inds(.x) %>% stringr::str_c(collapse = ", "))
                  ) %>%
    dplyr::select(-data,-N)


  tidyr::nest(-GEOGRAPHY_ID, -DATE_GROUP_ID, -INDICATOR) %>%
    dplyr::mutate(N = map(data, test_fun))

  # OLD STUFF ---------------------------------------------------------------



  get_greater_vars <- function(data){

    # For each tract, returns each INDICATOR name (e.g., RACE, TENURE, etc.)
    # where the proportion of the tract is greater than the county

    if(sum(data$N) ==0){return("none")}

    data %>% dplyr::filter(N>0) %>% purrr::pluck("INDICATOR")
  }

  vuln_inds <- acs_indicators %>%
    dplyr::group_by(GEOID, ENDYEAR, INDICATOR) %>%
    dplyr::summarise(N = sum(as.numeric(GREATER_THAN_COUNTY), na.rm = TRUE)) %>%
    tidyr::nest() %>%
    dplyr::mutate(N = purrr::map_dbl(data, ~ purrr::pluck(.x,"N") %>% sum),
                  VULNERABLE_LGL = N > 2,
                  VULNERABLE_INDS = purrr::map_chr(data, ~ get_greater_vars(.x) %>% stringr::str_c(collapse = ", "))) %>%
    dplyr::filter(ENDYEAR %in% 2016) %>%
    dplyr::transmute(GEOID, VULNERABLE_TR_CNT = N, VULNERABLE_LGL, VULNERABLE_INDS)

  vulnerability_indicators <- vuln_inds


  # RETURN ------------------------------------------------------------------

  return(portland_model_vulnerability)


}



