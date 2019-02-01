#' @title Make The Indicator Sample Size Metadata
#' @description Description
#' @param indicators_cnt_pct desc
#' @param indicators_median desc
#' @return a `tibble`
#' @export
make_sample_size_metadata <- function(indicators_cnt_pct,
                                 indicators_median){

  stop("This command is temporarily disabled")

  # SOMETHING ---------------------------------------------------------------


  sample_size_min <- tibble::tribble(
    ~SAMPLE_VARIABLE_DESC, ~SAMPLE_SIZE_MIN,
    "TOTAL_POPULATION", 1250L,
    "TOTAL_SALE_RATE_ALL", 50L,
    "TOTAL_SALE_RATE_CONDO_ONLY", 50L,
    "TOTAL_SALE_RATE_SF_ONLY", 50L,
    "COUNT_SALE_RATE_ALL", 10L,
    "COUNT_SALE_RATE_CONDO_ONLY", 10L,
    "COUNT_SALE_RATE_SF_ONLY", 10L
  )

  indicator_cols <-
    list(indicators_cnt_pct,
         indicators_median) %>%
    purrr::map_dfr(c) %>%
    dplyr::select(MEASURE_TYPE, SOURCE, INDICATOR, VARIABLE, VARIABLE_DESC) %>%
    dplyr::distinct()

  sample_variable <- indicator_cols %>%
    dplyr::mutate(SAMPLE_VARIABLE_DESC = dplyr::case_when(
      MEASURE_TYPE %in% c("COUNT", "TOTAL") ~ NA_character_,
      MEASURE_TYPE %in% "PERCENT" & SOURCE %in% c("ACS", "CHAS") ~ "TOTAL_POPULATION_ACS",
      MEASURE_TYPE %in% "PERCENT" & SOURCE %in% "ASSESSOR" ~ NA_character_,
      MEASURE_TYPE %in% "MEDIAN" & SOURCE %in% c("ACS", "LTDB","FACTFINDER") ~ "TOTAL_POPULATION_ACS",
      MEASURE_TYPE %in% "MEDIAN" & INDICATOR %in% "ASSESSED_VALUE" & stringr::str_detect(VARIABLE,"ALL") ~ "TOTAL_SALE_RATE_ALL",
      MEASURE_TYPE %in% "MEDIAN" & INDICATOR %in% "ASSESSED_VALUE" & stringr::str_detect(VARIABLE,"CONDO") ~ "TOTAL_SALE_RATE_CONDO",
      MEASURE_TYPE %in% "MEDIAN"  & INDICATOR %in% "ASSESSED_VALUE" & stringr::str_detect(VARIABLE,"SF") ~ "TOTAL_SALE_RATE_SF",
      MEASURE_TYPE %in% "MEDIAN" & INDICATOR %in% "SALE_PRICE" & stringr::str_detect(VARIABLE,"ALL") ~ "COUNT_SALE_RATE_ALL",
      MEASURE_TYPE %in% "MEDIAN" & INDICATOR %in% "SALE_PRICE" & stringr::str_detect(VARIABLE,"CONDO") ~ "COUNT_SALE_RATE_CONDO",
      MEASURE_TYPE %in% "MEDIAN"  & INDICATOR %in% "SALE_PRICE" & stringr::str_detect(VARIABLE,"SF") ~ "COUNT_SALE_RATE_SF",
      TRUE ~ NA_character_
    ))

  sample_size_metadata_ready <- sample_variable %>%
    dplyr::left_join(sample_size_min, by = "SAMPLE_VARIABLE_DESC")


 sample_size_metadata <- sample_size_metadata_ready

  # RETURN ------------------------------------------------------------------

  return(sample_size_metadata)

}
