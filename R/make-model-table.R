#' @title Make A Tibble of the Project Models
#' @description Return a `tibble` containing metadata for each of the
#'   models used to describe neighborhood change.
#' @return a `tibble`
#' @export
make_model_table <- function(){

  model_table <- tibble::tribble(
   ~MODEL,               ~TOPIC, ~MEASURE_TYPE,       ~INDICATOR,    ~SOURCE, ~ENDYEAR,
    "OLD",      "VULNERABILITY",     "PERCENT",           "RACE",      "ACS",    2015L,
    "OLD",      "VULNERABILITY",     "PERCENT",      "EDUCATION",      "ACS",    2015L,
    "OLD",      "VULNERABILITY",     "PERCENT",         "TENURE",      "ACS",    2015L,
    "OLD",      "VULNERABILITY",     "PERCENT",         "INCOME",      "ACS",    2015L,
    "OLD", "DEMOGRAPHIC CHANGE",     "PERCENT",           "RACE",      "ACS",    2010L,
    "OLD", "DEMOGRAPHIC CHANGE",     "PERCENT",      "EDUCATION",      "ACS",    2010L,
    "OLD", "DEMOGRAPHIC CHANGE",     "PERCENT",         "TENURE",      "ACS",    2010L,
    "OLD", "DEMOGRAPHIC CHANGE",     "PERCENT",         "INCOME",      "ACS",    2010L,
    "OLD", "DEMOGRAPHIC CHANGE",     "PERCENT",           "RACE",      "ACS",    2015L,
    "OLD", "DEMOGRAPHIC CHANGE",     "PERCENT",      "EDUCATION",      "ACS",    2015L,
    "OLD", "DEMOGRAPHIC CHANGE",     "PERCENT",         "TENURE",      "ACS",    2015L,
    "OLD", "DEMOGRAPHIC CHANGE",     "PERCENT",         "INCOME",      "ACS",    2015L,
    "OLD",     "HOUSING MARKET",       "VALUE", "ASSESSED VALUE", "ASSESSOR",    2005L,
    "OLD",     "HOUSING MARKET",       "VALUE", "ASSESSED VALUE", "ASSESSOR",    2010L,
    "OLD",     "HOUSING MARKET",       "VALUE", "ASSESSED VALUE", "ASSESSOR",    2015L,
    "NEW",      "VULNERABILITY",     "PERCENT",           "RACE",      "ACS",    2016L,
    "NEW",      "VULNERABILITY",     "PERCENT",      "EDUCATION",      "ACS",    2016L,
    "NEW",      "VULNERABILITY",     "PERCENT",         "TENURE",      "ACS",    2016L,
    "NEW",      "VULNERABILITY",     "PERCENT",         "INCOME",      "ACS",    2016L,
    "NEW", "DEMOGRAPHIC CHANGE",     "PERCENT",           "RACE",      "ACS",    2011L,
    "NEW", "DEMOGRAPHIC CHANGE",     "PERCENT",      "EDUCATION",      "ACS",    2011L,
    "NEW", "DEMOGRAPHIC CHANGE",     "PERCENT",         "TENURE",      "ACS",    2011L,
    "NEW", "DEMOGRAPHIC CHANGE",     "PERCENT",         "INCOME",      "ACS",    2011L,
    "NEW", "DEMOGRAPHIC CHANGE",     "PERCENT",           "RACE",      "ACS",    2016L,
    "NEW", "DEMOGRAPHIC CHANGE",     "PERCENT",      "EDUCATION",      "ACS",    2015L,
    "NEW", "DEMOGRAPHIC CHANGE",     "PERCENT",         "TENURE",      "ACS",    2015L,
    "NEW", "DEMOGRAPHIC CHANGE",     "PERCENT",         "INCOME",      "ACS",    2015L,
    "NEW",     "HOUSING MARKET",       "VALUE", "ASSESSED VALUE", "ASSESSOR",    2005L,
    "NEW",     "HOUSING MARKET",       "VALUE", "ASSESSED VALUE", "ASSESSOR",    2010L,
    "NEW",     "HOUSING MARKET",       "VALUE", "ASSESSED VALUE", "ASSESSOR",    2018L,
    "ALT",      "VULNERABILITY",     "PERCENT",           "RACE",      "ACS",    2016L,
    "ALT",      "VULNERABILITY",     "PERCENT",      "EDUCATION",      "ACS",    2016L,
    "ALT",      "VULNERABILITY",     "PERCENT",         "TENURE",      "ACS",    2016L,
    "ALT",      "VULNERABILITY",     "PERCENT",         "INCOME",      "ACS",    2016L,
    "ALT", "DEMOGRAPHIC CHANGE",     "PERCENT",           "RACE",      "ACS",    2011L,
    "ALT", "DEMOGRAPHIC CHANGE",     "PERCENT",      "EDUCATION",      "ACS",    2011L,
    "ALT", "DEMOGRAPHIC CHANGE",     "PERCENT",         "TENURE",      "ACS",    2011L,
    "ALT", "DEMOGRAPHIC CHANGE",     "PERCENT",         "INCOME",      "ACS",    2011L,
    "ALT", "DEMOGRAPHIC CHANGE",     "PERCENT",           "RACE",      "ACS",    2016L,
    "ALT", "DEMOGRAPHIC CHANGE",     "PERCENT",      "EDUCATION",      "ACS",    2015L,
    "ALT", "DEMOGRAPHIC CHANGE",     "PERCENT",         "TENURE",      "ACS",    2015L,
    "ALT", "DEMOGRAPHIC CHANGE",     "PERCENT",         "INCOME",      "ACS",    2015L,
    "ALT",     "HOUSING MARKET",       "VALUE",           "RENT",      "ACS",    2010L,
    "ALT",     "HOUSING MARKET",       "VALUE",           "RENT",      "ACS",    2016L,
    "ALT",     "HOUSING MARKET",       "VALUE",     "SALE PRICE", "ASSESSOR",    2005L,
    "ALT",     "HOUSING MARKET",       "VALUE",     "SALE PRICE", "ASSESSOR",    2010L,
    "ALT",     "HOUSING MARKET",       "VALUE",     "SALE PRICE", "ASSESSOR",    2018L,
    "ALT",     "HOUSING MARKET",     "PERCENT",      "SALE RATE", "ASSESSOR",    2005L,
    "ALT",     "HOUSING MARKET",     "PERCENT",      "SALE RATE", "ASSESSOR",    2010L,
    "ALT",     "HOUSING MARKET",     "PERCENT",      "SALE RATE", "ASSESSOR",    2018L,
  "OTHER",     "VULENERABILITY",     "PERCENT",    "COST BURDEN",      "ACS",    2016L,
  "OTHER",     "VULENERABILITY",     "PERCENT",    "COST BURDEN",      "ACS",    2011L
  )






return(model_table)

}
