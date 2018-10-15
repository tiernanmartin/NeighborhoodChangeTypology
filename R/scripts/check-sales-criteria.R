
# NOTE --------------------------------------------------------------------

# This script was used to explore the criteria that defined a "sale"
# of a property.
#
# It is not intended to be used in the project plan.


# SCRIPT ------------------------------------------------------------------

\dontrun{
  library(tidyverse)
library(janitor)
loadd(parcel_sales, sales_lut_key_list)

glimpse(parcel_sales)
glimpse(sales_lut_key_list)

p_sales <- parcel_sales %>%
  mutate_all(as.character) %>%
  left_join(sales_lut_key_list$PRINCIPAL_USE, by = "PRINCIPAL_USE") %>%
  left_join(sales_lut_key_list$PROPERTY_CLASS, by = "PROPERTY_CLASS") %>%
  left_join(sales_lut_key_list$PROPERTY_TYPE, by = "PROPERTY_TYPE") %>%
  left_join(sales_lut_key_list$SALE_INSTRUMENT, by = "SALE_INSTRUMENT") %>%
  left_join(sales_lut_key_list$SALE_REASON, by = "SALE_REASON")

check_na_pct <- function(){

  sales_cols <- p_sales %>%
    mutate(SALE_PRICE = as.numeric(SALE_PRICE)) %>%
    select(SALE_PRICE,
           matches("DESC|SALE_PRICE"))

  p_check_na %>%
    gather(VAR, VALUE) %>%
    group_by(VAR) %>%
    summarize(PCT_NA = sum(is.na(VALUE))/nrow(p_check_na)) %>%
    arrange(PCT_NA)

}

# It seems that `PRINCIPAL_USE_DESC` has the most missing data

p_sales %>%
  tabyl(PROPERTY_TYPE_DESC) %>%
    adorn_percentages("col") %>%
    adorn_pct_formatting() %>%
    arrange(desc(n))

# Use: 'LAND WITH PREV USED BLDG' OR 'LAND WITH NEW BUILDING' OR 'Household, single family units' OR 'Residential condominiums'

p_sales %>%
  tabyl(SALE_REASON_DESC) %>%
  adorn_percentages("col") %>%
    adorn_pct_formatting() %>%
    arrange(desc(n))

# Use 'None' OR 'Other' OR 'Trust' OR 'Property Settlement' OR 'Estate Settlement'

p_sales %>%
  tabyl(PROPERTY_CLASS_DESC) %>%
  adorn_percentages("col") %>%
    adorn_pct_formatting() %>%
    arrange(desc(n))

p_sales %>%
  tabyl(PRINCIPAL_USE_DESC) %>%
  adorn_percentages("col") %>%
    adorn_pct_formatting() %>%
    arrange(desc(n))



p_sales %>%
  filter(PROPERTY_CLASS_DESC %in% "Res-Improved property") %>%
  count(PROPERTY_TYPE_DESC, sort = TRUE) %>%
  mutate(PCT = n/nrow(p_sales))

p_sales %>%
  filter(PROPERTY_CLASS_DESC %in% "Res-Improved property") %>%
  count(PRINCIPAL_USE_DESC, sort = TRUE) %>%
  mutate(PCT = n/nrow(p_sales))

p_sales %>%
  filter(PROPERTY_CLASS_DESC %in% "Res-Improved property") %>%
  select(SALE_PRICE,
           matches("DESC|SALE_PRICE")) %>%
  gather(VAR, VALUE) %>%
    group_by(VAR) %>%
    summarize(PCT_NA = sum(is.na(VALUE))/nrow(p_check_na)) %>%
    arrange(PCT_NA)


sales_criteria <- list(
  "principal_use" = c("RESIDENTIAL", "CONDOMINIUM"),
  "property_class" = c("Res-Improved property", "C/I-Condominium"),
  "sale_reason" = c("None","Other", "Trust", "Property Settlement", "Estate Settlement"),
  "date" = c("2005", "2010", "2017")
)

tmp <-
p_sales %>%
  mutate(DATE = str_extract(DOCUMENT_DATE,".{4}$")) %>%
  filter(PRINCIPAL_USE_DESC %in% sales_criteria$principal_use,
         PROPERTY_CLASS_DESC %in% sales_criteria$property_class,
         SALE_REASON_DESC %in% sales_criteria$sale_reason,
         DATE %in% sales_criteria$date)

tmp %>%
  transmute(SALE_PRICE = as.numeric(SALE_PRICE),
            DATE = factor(DATE)
            ) %>%
  ggplot(aes(x = SALE_PRICE)) +
  geom_histogram() +
  scale_x_log10(labels = scales::log_breaks()) +
  facet_wrap(~ DATE,ncol = 1)
}



