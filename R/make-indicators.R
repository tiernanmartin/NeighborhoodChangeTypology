#' @title Make All Indicators
#' @description Desc
#' @param indicators_cnt_pct desc
#' @param indicators_median desc
#' @param sample_size_metadata desc
#' @return a `tibble`
#' @export
make_indicators <- function(indicators_cnt_pct,
                                 indicators_median,
                                 sample_size_metadata){

  stop("This command is temporarily disabled")

  inds_all <- list(indicators_cnt_pct,
                                 indicators_median) %>%
    purrr::map_dfr(c) %>%
    dplyr::left_join(sample_size_metadata, by = c("SOURCE", "INDICATOR", "VARIABLE", "VARIABLE_DESC", "MEASURE_TYPE"))

  indicators <- inds_all

  return(indicators)

}
