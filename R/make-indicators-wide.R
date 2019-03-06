#' @title Make The Project Inidicators (Wide)
#' @description Description
#' @param model_table_column_type desc
#' @param model_table_production desc
#' @param community_metadata desc
#' @param indicators_comparison desc
#' @param indicators_comparison_of_change desc
#' @param indicators_change_in_comparison desc
#' @param indicators_proximity desc
#' @return a `tibble`
#' @export

make_indicators_wide <- function(model_table_column_type,
                                 model_table_production,
                                 community_metadata,
                                 indicators_comparison,
                                 indicators_comparison_of_change,
                                 indicators_change_in_comparison,
                                 indicators_proximity){
  # GET THE COLNAMES FOR EACH MODEL_ROLE TYPE -------------------------------

  cols_ref <- model_table_column_type %>%
    dplyr::filter(MODEL_ROLE %in% "REFERENCE") %>%
    dplyr::pull(COLNAME)

  cols_value <- model_table_column_type %>%
    dplyr::filter(! MODEL_ROLE %in% "REFERENCE") %>%
    dplyr::pull(COLNAME)

  cols_relative <- model_table_column_type %>%
    dplyr::filter(MODEL_ROLE %in% c("REFERENCE","RELATIVE")) %>%
    dplyr::pull(COLNAME)

  cols_change <- model_table_column_type %>%
    dplyr::filter(MODEL_ROLE %in% c("REFERENCE","CHANGE")) %>%
    dplyr::pull(COLNAME)

  cols_relative_change <- model_table_column_type %>%
    dplyr::filter(MODEL_ROLE %in% c("REFERENCE","RELATIVE_CHANGE")) %>%
    dplyr::pull(COLNAME)

  # this one is different because it doesn't have metadata
  # columns other than GEOGRAPHY_*
  cols_proximity <- names(indicators_proximity)



  # SELECT THE APPROPRIATE COLUMS AND ADD MODEL_ROLE ------------------------

  # create a tibble to faciliate pmap()
  inds_cols_tbl <- tibble::tibble(
    data = list(
      indicators_comparison,
      indicators_comparison_of_change,
      indicators_change_in_comparison,
      indicators_proximity
    ),
    col_names = list(cols_relative,
                     cols_change,
                     cols_relative_change,
                     cols_proximity),
    model_roles = c("RELATIVE",
                    "CHANGE",
                    "RELATIVE_CHANGE",
                    "PROXIMITY"))

  select_cols_and_mutate <- function(data, col_names, model_roles){
    data %>%
      dplyr::select(tidyselect::vars_select(names(.), col_names)) %>%
      dplyr::mutate(MODEL_ROLE = model_roles )
  }

  # map the function, rowbind the results, and gather()
  all_inds_long <- purrr::pmap_dfr(inds_cols_tbl, select_cols_and_mutate) %>%
    tidyr::gather(VALUE_TYPE, VALUE, tidyselect::vars_select(names(.), cols_value))



  # JOIN MODEL_TABLE_PRODUCTION (USE FILTERS), UNITE, SPREAD() --------------------------

  model_table_value_short <- model_table_column_type %>%
    dplyr::filter(! MODEL_ROLE %in% "REFERENCE") %>%
    # these cols will be joined to all_inds_long, so they are renamed accordingly
    dplyr::transmute(VALUE = COLNAME,
                     VALUE_SHORT = COLNAME_SHORT)

  all_inds_wide_full <- all_inds_long %>%
    dplyr::left_join(model_table_value_short, by = c(VALUE_TYPE = "VALUE")) %>%
    dplyr::select(-VALUE_TYPE) %>%
    # only include the combinations of fields that are found in `model_table_production`
    dplyr::semi_join(model_table_production,
                     by = c("DATE_GROUP_ID", "DIMENSION", "INDICATOR", "VARIABLE", "MEASURE_TYPE")) %>%
    dplyr::left_join(model_table_production,
                     by = c("DATE_GROUP_ID", "DIMENSION", "INDICATOR", "VARIABLE", "MEASURE_TYPE")) %>%
    # only include the combinations of fields where the MODEL_ROLE matches the VALUE_SHORT (see model_table_column_type)
    dplyr::semi_join(model_table_column_type,
                     by = c(MODEL_ROLE = "MODEL_ROLE", VALUE_SHORT = "COLNAME_SHORT")) %>%
    tidyr::unite("WIDE_FIELD",
                 c(MODEL_SHORT, DIMENSION_SHORT,DESC_SHORT, VALUE_SHORT, MEASURE_TYPE_SHORT, DATE_GROUP_ID)) %>%
    dplyr::select(dplyr::starts_with("GEOGRAPHY"),
                  WIDE_FIELD,
                  VALUE) %>%
    tidyr::spread(WIDE_FIELD, VALUE)

  # result: tbl with 402 rows (tracts, communities and county) and 1,381 columns



  # DROP COLUMNS THAT ONLY CONTAIN NA ---------------------------------------

  # Note: only drop columns that contain all NA's (columns with some missing values are retained)
  all_inds_wide_no_na <- drop_na_cols(all_inds_wide_full)

  # result: tbl with 402 rows (tracts, communities and county) and 1,060 columns


  # RE-ESTABLISH CORRECT CLASSES --------------------------------------------

  cols_char <- names(all_inds_wide_no_na) %>%
    purrr::keep(stringr::str_detect,"GEOG|DESC_")

  cols_lgl <- names(all_inds_wide_no_na) %>%
    purrr::keep(stringr::str_detect,"LGL_")

  cols_num <- names(all_inds_wide_no_na) %>%
    purrr::discard(stringr::str_detect,"GEOG|DESC_|LGL_")

  all_inds_wide_class <-  all_inds_wide_no_na %>%
    dplyr::mutate_at(tidyselect::vars_select(names(.), cols_char), as.character) %>%
    dplyr::mutate_at(tidyselect::vars_select(names(.), cols_lgl), as.logical) %>%
    dplyr::mutate_at(tidyselect::vars_select(names(.), cols_num), as.numeric)

# JOIN GEOGRAPHY_COMMUNITY_* COLUMNS --------------------------------------


  geography_community_cols <- community_metadata %>%
    dplyr::distinct() %>%
    dplyr::select(GEOGRAPHY_ID, GEOGRAPHY_COMMUNITY_NAME, GEOGRAPHY_COMMUNITY_ID)

  all_inds_wide_comm <- all_inds_wide_class %>%
    dplyr::left_join(geography_community_cols, by = "GEOGRAPHY_ID")

    indicators_wide <- all_inds_wide_comm


  # RETURN ------------------------------------------------------------------

  return(indicators_wide)


}

