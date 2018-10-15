#' @title Make A Tibble of the Project Models
#' @description Return a `tibble` containing metadata for each of the
#'   models used to describe neighborhood change.
#' @return a `tibble`
#' @export
make_model_table <- function(){

  model_table <- tibble::tribble(
  ~MODEL,               ~TOPIC,       ~INDICATOR,    ~SOURCE, ~ENDYEAR,
   "OLD",      "VULNERABILITY",           "RACE",      "ACS",    2015L,
   "OLD",      "VULNERABILITY",      "EDUCATION",      "ACS",    2015L,
   "OLD",      "VULNERABILITY",         "TENURE",      "ACS",    2015L,
   "OLD",      "VULNERABILITY",         "INCOME",      "ACS",    2015L,
   "OLD", "DEMOGRAPHIC CHANGE",           "RACE",      "ACS",    2010L,
   "OLD", "DEMOGRAPHIC CHANGE",      "EDUCATION",      "ACS",    2010L,
   "OLD", "DEMOGRAPHIC CHANGE",         "TENURE",      "ACS",    2010L,
   "OLD", "DEMOGRAPHIC CHANGE",         "INCOME",      "ACS",    2010L,
   "OLD", "DEMOGRAPHIC CHANGE",           "RACE",      "ACS",    2015L,
   "OLD", "DEMOGRAPHIC CHANGE",      "EDUCATION",      "ACS",    2015L,
   "OLD", "DEMOGRAPHIC CHANGE",         "TENURE",      "ACS",    2015L,
   "OLD", "DEMOGRAPHIC CHANGE",         "INCOME",      "ACS",    2015L,
   "OLD",     "HOUSING MARKET", "ASSESSED VALUE", "ASSESSOR",    2005L,
   "OLD",     "HOUSING MARKET", "ASSESSED VALUE", "ASSESSOR",    2010L,
   "OLD",     "HOUSING MARKET", "ASSESSED VALUE", "ASSESSOR",    2015L,
   "NEW",      "VULNERABILITY",           "RACE",      "ACS",    2016L,
   "NEW",      "VULNERABILITY",      "EDUCATION",      "ACS",    2016L,
   "NEW",      "VULNERABILITY",         "TENURE",      "ACS",    2016L,
   "NEW",      "VULNERABILITY",         "INCOME",      "ACS",    2016L,
   "NEW", "DEMOGRAPHIC CHANGE",           "RACE",      "ACS",    2011L,
   "NEW", "DEMOGRAPHIC CHANGE",      "EDUCATION",      "ACS",    2011L,
   "NEW", "DEMOGRAPHIC CHANGE",         "TENURE",      "ACS",    2011L,
   "NEW", "DEMOGRAPHIC CHANGE",         "INCOME",      "ACS",    2011L,
   "NEW", "DEMOGRAPHIC CHANGE",           "RACE",      "ACS",    2016L,
   "NEW", "DEMOGRAPHIC CHANGE",      "EDUCATION",      "ACS",    2015L,
   "NEW", "DEMOGRAPHIC CHANGE",         "TENURE",      "ACS",    2015L,
   "NEW", "DEMOGRAPHIC CHANGE",         "INCOME",      "ACS",    2015L,
   "NEW",     "HOUSING MARKET", "ASSESSED VALUE", "ASSESSOR",    2005L,
   "NEW",     "HOUSING MARKET", "ASSESSED VALUE", "ASSESSOR",    2010L,
   "NEW",     "HOUSING MARKET", "ASSESSED VALUE", "ASSESSOR",    2018L,
   "ALT",      "VULNERABILITY",           "RACE",      "ACS",    2016L,
   "ALT",      "VULNERABILITY",      "EDUCATION",      "ACS",    2016L,
   "ALT",      "VULNERABILITY",         "TENURE",      "ACS",    2016L,
   "ALT",      "VULNERABILITY",         "INCOME",      "ACS",    2016L,
   "ALT", "DEMOGRAPHIC CHANGE",           "RACE",      "ACS",    2011L,
   "ALT", "DEMOGRAPHIC CHANGE",      "EDUCATION",      "ACS",    2011L,
   "ALT", "DEMOGRAPHIC CHANGE",         "TENURE",      "ACS",    2011L,
   "ALT", "DEMOGRAPHIC CHANGE",         "INCOME",      "ACS",    2011L,
   "ALT", "DEMOGRAPHIC CHANGE",           "RACE",      "ACS",    2016L,
   "ALT", "DEMOGRAPHIC CHANGE",      "EDUCATION",      "ACS",    2015L,
   "ALT", "DEMOGRAPHIC CHANGE",         "TENURE",      "ACS",    2015L,
   "ALT", "DEMOGRAPHIC CHANGE",         "INCOME",      "ACS",    2015L,
   "ALT",     "HOUSING MARKET",           "RENT",      "ACS",    2010L,
   "ALT",     "HOUSING MARKET",           "RENT",      "ACS",    2016L,
   "ALT",     "HOUSING MARKET",     "SALE PRICE", "ASSESSOR",    2005L,
   "ALT",     "HOUSING MARKET",     "SALE PRICE", "ASSESSOR",    2010L,
   "ALT",     "HOUSING MARKET",     "SALE PRICE", "ASSESSOR",    2018L,
   "ALT",     "HOUSING MARKET",      "SALE RATE", "ASSESSOR",    2005L,
   "ALT",     "HOUSING MARKET",      "SALE RATE", "ASSESSOR",    2010L,
   "ALT",     "HOUSING MARKET",      "SALE RATE", "ASSESSOR",    2018L
  )

return(model_table)

}
