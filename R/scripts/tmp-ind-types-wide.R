
# SETUP -------------------------------------------------------------------

devtools::load_all(".")
library(tidyverse)

loadd(model_table_column_type,
      model_table_production,
      indicators_comparison,
      indicators_comparison_of_change,
      indicators_change_in_comparison,
      indicators_proximity)



# GET THE COLNAMES FOR EACH MODEL_ROLE TYPE -------------------------------

cols_value <- model_table_column_type %>%
  dplyr::filter(! MODEL_ROLE %in% "REFERENCE") %>%
  pull(COLNAME)

cols_relative <- model_table_column_type %>%
  dplyr::filter(MODEL_ROLE %in% c("REFERENCE","RELATIVE")) %>%
  pull(COLNAME)

cols_change <- model_table_column_type %>%
  dplyr::filter(MODEL_ROLE %in% c("REFERENCE","CHANGE")) %>%
  pull(COLNAME)

cols_relative_change <- model_table_column_type %>%
  dplyr::filter(MODEL_ROLE %in% c("REFERENCE","RELATIVE_CHANGE")) %>%
  pull(COLNAME)

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
all_inds_long <- pmap_dfr(inds_cols_tbl, select_cols_and_mutate) %>%
  tidyr::gather(VALUE_TYPE, VALUE, tidyselect::vars_select(names(.), cols_value))



# JOIN MODEL_TABLE_PRODUCTION (USE FILTERS), UNITE, SPREAD() --------------------------

model_table_value_short <- model_table_column_type %>%
  dplyr::filter(! MODEL_ROLE %in% "REFERENCE") %>%
  # these cols will be joined to all_inds_long, so they are renamed accordingly
  dplyr::transmute(VALUE = COLNAME,
                   VALUE_SHORT = COLNAME_SHORT)

all_inds_wide <- all_inds_long %>%
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

# result: tbl with 402 rows (tracts, communities and county) and 1114 columns

