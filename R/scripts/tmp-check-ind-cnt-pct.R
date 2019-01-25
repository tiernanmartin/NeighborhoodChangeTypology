tmp <- indicators_cnt_pct %>%
    dplyr::filter(MEASURE_TYPE %in% "PERCENT") %>%
    dplyr::filter(DATE_RANGE_TYPE %in% c("one quarter")) %>%
  filter(DATE_END_YEAR %in% "2015_Q1") %>% g


tmp2 <- all_cnt_vars %>%
    dplyr::mutate(VARIABLE_ROLE = toupper(VARIABLE_ROLE)) %>%
    dplyr::group_by_at(dplyr::vars(-VARIABLE_SUBTOTAL, -MEASURE_TYPE, -VARIABLE_SUBTOTAL_DESC, -ESTIMATE, -MOE)) %>%
    dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                     MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!VARIABLE_ROLE %in% "OMIT") %>%
    dplyr::filter(!is.na(GEOGRAPHY_ID)) %>% # there are missing GEOIDS in the assessor data
    tidyr::gather(TYPE, VALUE, ESTIMATE, MOE) %>%
    tidyr::unite(PROP_TYPE, VARIABLE_ROLE, TYPE) %>%
    tidyr::spread(PROP_TYPE, VALUE)

tmp2 %>%
    dplyr::filter(DATE_RANGE_TYPE %in% c("one quarter")) %>%
  filter(DATE_END_YEAR %in% "2015_Q1") %>%
    vis_miss()

# this shows that the DATE_BEGIN and DATE_END do not match
tmp2 %>%
    dplyr::filter(DATE_RANGE_TYPE %in% c("one quarter")) %>%
  filter(DATE_END_YEAR %in% "2015_Q1") %>% slice(c(1,4)) %>%
    g

