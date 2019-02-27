#' @title Make The Change Date Group IDs (Long)
#' @description Description
#' @param model_table_production desc
#' @return a `tibble`
#' @export
make_change_dategroupid_long <- function(model_table_production){


# PREPARE DATA ------------------------------------------------------------

  change_dategroupid_wide <- model_table_production %>%
    dplyr::filter(str_detect(MEASURE_TYPE, "^CHANGE")) %>%
    dplyr::select(-dplyr::starts_with("MODEL"),
                  -dplyr::starts_with("MEASURE_TYPE"),
                  -dplyr::ends_with("SHORT")) %>%  # drop columns that will impair the join
    dplyr::mutate(DATE_GROUP_ID_SEPARATE = DATE_GROUP_ID) %>%
    tidyr::separate(DATE_GROUP_ID_SEPARATE, into = c("BEGIN_DATE_GROUP_ID", "END_DATE_GROUP_ID"),sep = "_TO_") %>%  # separate BEGIN and END into fields
    dplyr::distinct() %>%
    dplyr::arrange(DIMENSION, INDICATOR, VARIABLE, BEGIN_DATE_GROUP_ID)

  change_dategroupid_long <- change_dategroupid_wide %>%
      tidyr::gather(DATE_TYPE, DATE_GROUP_ID_JOIN, c(BEGIN_DATE_GROUP_ID, END_DATE_GROUP_ID))


# RETURN ------------------------------------------------------------------

  return(change_dategroupid_long)
}
