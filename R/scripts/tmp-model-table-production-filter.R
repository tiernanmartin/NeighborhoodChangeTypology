loadd(indicators_by_topic,
      model_table_production)

indicators_by_topic %>%
  count(TOPIC, INDICATOR, VARIABLE, MEASURE_TYPE, DATE_GROUP_ID) %>% print(n=50)

indicators_by_topic %>%
  filter(TOPIC %in% "HOUSING_MARKET") %>%
  filter(MEASURE_TYPE %in% c("MEDIAN", "PERCENT")) %>%
  filter(! str_detect(DATE_GROUP_ID,"Q")) %>%
  count(INDICATOR, VARIABLE, MEASURE_TYPE, DATE_RANGE_TYPE, DATE_GROUP_ID) %>% print(n=Inf)

model_table_production %>%
  filter(TOPIC %in% "HOUSING_MARKET") %>%
  filter(MEASURE_TYPE %in% c("MEDIAN", "PERCENT")) %>%
  count(INDICATOR, VARIABLE, MEASURE_TYPE, DATE_GROUP_ID) %>% print(n=Inf)

indicators_by_topic %>%
  filter(TOPIC %in% "HOUSING_MARKET") %>%
  anti_join(model_table_production,
             by = c("TOPIC", "INDICATOR", "VARIABLE", "MEASURE_TYPE","DATE_GROUP_ID")) %>%
  count(TOPIC, INDICATOR, VARIABLE, MEASURE_TYPE, DATE_GROUP_ID) %>% print(n=Inf)


indicators_by_topic %>%
  inner_join(model_table_production,
             by = c("TOPIC", "INDICATOR", "VARIABLE", "MEASURE_TYPE","DATE_GROUP_ID")) %>%
  count(MODEL,TOPIC, INDICATOR, VARIABLE, MEASURE_TYPE, DATE_GROUP_ID) %>% print(n=Inf)

# this returns only the data with MEASURE_TYPE %in% c("CHANGE_ABSOLUTE", "CHANGE_RATIO")
model_table_production %>%
  anti_join(indicators_by_topic,
            by = c("TOPIC", "INDICATOR", "VARIABLE", "MEASURE_TYPE", "DATE_GROUP_ID") )%>% print(n=Inf)



indicators_by_topic %>%
  anti_join(model_table_production,
             by = c("TOPIC", "INDICATOR", "VARIABLE", "MEASURE_TYPE","DATE_GROUP_ID")) %>%
  filter(! MEASURE_TYPE %in% c("COUNT", "TOTAL")) %>%
  filter(! str_detect(DATE_GROUP_ID,"Q")) %>%
  count(TOPIC, INDICATOR, VARIABLE, MEASURE_TYPE, DATE_GROUP_ID) %>% print(n=Inf)

indicators_by_topic %>%
  anti_join(model_table_production,
             by = c("TOPIC", "INDICATOR", "VARIABLE", "MEASURE_TYPE","DATE_GROUP_ID")) %>%
   filter(str_detect(DATE_GROUP_ID,"Q")) %>%
   count(INDICATOR, VARIABLE, is.na(MEASURE_TYPE)) %>%
  arrange(`is.na(MEASURE_TYPE)`)
