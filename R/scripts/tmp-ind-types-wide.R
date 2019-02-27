
# SETUP -------------------------------------------------------------------

devtools::load_all(".")
library(tidyverse)

loadd(model_table_short,
      model_table_production,
      indicators_by_dimension,
      indicators_comparison,
      indicators_comparison_of_change,
      indicators_change_in_comparison,
      indicators_proximity)


# COMBINE INDS_COMP/CHANGE ------------------------------------------------


list(indicators_comparison,
     indicators_comparison_of_change,
     indicators_change_in_comparison,
     indicators_proximity) %>%
  map(slice, 1:10) %>%
  map(drop_na_cols) %>%
  walk(g)

list(indicators_comparison,
     indicators_comparison_of_change,
     indicators_change_in_comparison,
     indicators_proximity) %>%
  map(slice, 1:10) %>%
  map(drop_na_cols) %>%
  reduce(full_join) %>%
  dplyr::select(dplyr::starts_with("SOURCE"),
                dplyr::starts_with("GEOGRAPHY"),
                dplyr::starts_with("DATE"),
                dplyr::starts_with("DIMENSION"),
                INDICATOR,
                dplyr::starts_with("VARIABLE"),
                dplyr::starts_with("MEASURE"),
                dplyr::starts_with("ESTIMATE"),
                dplyr::starts_with("MOE"),
                dplyr::starts_with("INDICATOR_")) %>%
  dplyr::select(dplyr::everything(),
                # make INDICATOR_TYPE_MODEL the final field
                -INDICATOR_TYPE_MODEL,
                INDICATOR_TYPE_MODEL) %>% View()
g()




# SOMETHING ---------------------------------------------------------------

# this creates a "wide" df with 473 fields and 402 rows

indicators_comparison %>%
  gather(VALUE_TYPE, VALUE, matches("ESTIMATE|MOE|RELATIVE")) %>%
  left_join(model_table_production) %>%
  unite("COMBO", c(MODEL_SHORT, DIMENSION_SHORT, DESC_SHORT,MEASURE_TYPE_SHORT,  VALUE_TYPE,DATE_GROUP_ID)) %>%
  select(starts_with("GEOGRAPHY"),
         COMBO,
         VALUE) %>%
  spread(COMBO,VALUE) %>%
  drop_na_cols() %>% g()
