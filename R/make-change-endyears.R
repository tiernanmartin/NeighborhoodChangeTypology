#' @title Make The Change Endyears
#' @description Description
#' @return a `tibble`
#' @export
make_change_endyears <- function(){


  # CREATE CHANGE_ENDYEARS_GUIDE --------------------------------------------

  change_endyears <- tibble::tribble(
    ~INDICATOR,       ~VARIABLE,      ~BEGIN,        ~END,
    "COST_BURDEN_OWN",    "B25106_OWN",      "2010",      "2015",
    "COST_BURDEN_OWN",    "B25106_OWN",      "2011",      "2017",
    "COST_BURDEN_RENT",   "B25106_RENT",      "2010",      "2015",
    "COST_BURDEN_RENT",   "B25106_RENT",      "2011",      "2017",
    "EDUCATION",        "B15002",      "2010",      "2015",
    "EDUCATION",        "B15002",      "2011",      "2017",
    "INCOME",            "T7",      "2010",      "2015",
    "INCOME",        "B19001",      "2010",      "2015",
    "INCOME",        "B19001",      "2011",      "2017",
    "RACE",        "B03002",      "2010",      "2015",
    "RACE",        "B03002",      "2011",      "2017",
    "TENURE",        "B25033",      "2010",      "2015",
    "TENURE",        "B25033",      "2011",      "2017",
    "VALUE",        "B25077",      "2000",      "2010",
    "VALUE",        "B25077",      "2000",      "2017",
    "VALUE",        "B25077",      "2010",      "2017",
    "RENT",        "B25058",      "2010",      "2015",
    "RENT",        "B25058",      "2011",      "2017",
    "ASSESSED_VALUE",       "ATV_ALL",      "2005",      "2010",
    "ASSESSED_VALUE",       "ATV_ALL",      "2005",      "2018",
    "ASSESSED_VALUE",       "ATV_ALL",      "2010",      "2018",
    "ASSESSED_VALUE",     "ATV_CONDO",      "2005",      "2010",
    "ASSESSED_VALUE",     "ATV_CONDO",      "2005",      "2018",
    "ASSESSED_VALUE",     "ATV_CONDO",      "2010",      "2018",
    "ASSESSED_VALUE",        "ATV_SF",      "2005",      "2010",
    "ASSESSED_VALUE",        "ATV_SF",      "2005",      "2018",
    "ASSESSED_VALUE",        "ATV_SF",      "2010",      "2018",
    "SALE_PRICE",        "SP_ALL",      "2005",      "2010",
    "SALE_PRICE",        "SP_ALL",      "2005",      "2018",
    "SALE_PRICE",        "SP_ALL",      "2010",      "2018",
    "SALE_PRICE",        "SP_ALL", "2013_2015", "2016_2018",
    "SALE_PRICE",      "SP_CONDO",      "2005",      "2010",
    "SALE_PRICE",      "SP_CONDO",      "2005",      "2018",
    "SALE_PRICE",      "SP_CONDO",      "2010",      "2018",
    "SALE_PRICE",      "SP_CONDO", "2013_2015", "2016_2018",
    "SALE_PRICE",         "SP_SF",      "2005",      "2010",
    "SALE_PRICE",         "SP_SF",      "2005",      "2018",
    "SALE_PRICE",         "SP_SF",      "2010",      "2018",
    "SALE_PRICE",         "SP_SF", "2013_2015", "2016_2018",
    "SALE_PRICE",   "SP_SQFT_ALL",      "2005",      "2010",
    "SALE_PRICE",   "SP_SQFT_ALL",      "2005",      "2018",
    "SALE_PRICE",   "SP_SQFT_ALL",      "2010",      "2018",
    "SALE_PRICE",   "SP_SQFT_ALL", "2013_2015", "2016_2018",
    "SALE_PRICE", "SP_SQFT_CONDO",      "2005",      "2010",
    "SALE_PRICE", "SP_SQFT_CONDO",      "2005",      "2018",
    "SALE_PRICE", "SP_SQFT_CONDO",      "2010",      "2018",
    "SALE_PRICE", "SP_SQFT_CONDO", "2013_2015", "2016_2018",
    "SALE_PRICE",    "SP_SQFT_SF",      "2005",      "2010",
    "SALE_PRICE",    "SP_SQFT_SF",      "2005",      "2018",
    "SALE_PRICE",    "SP_SQFT_SF",      "2010",      "2018",
    "SALE_PRICE",    "SP_SQFT_SF", "2013_2015", "2016_2018",
    "SALE_RATE",        "SR_ALL",      "2005",      "2010",
    "SALE_RATE",        "SR_ALL",      "2005",      "2018",
    "SALE_RATE",        "SR_ALL",      "2010",      "2018",
    "SALE_RATE",        "SR_ALL", "2013_2015", "2016_2018",
    "SALE_RATE",      "SR_CONDO",      "2005",      "2010",
    "SALE_RATE",      "SR_CONDO",      "2005",      "2018",
    "SALE_RATE",      "SR_CONDO",      "2010",      "2018",
    "SALE_RATE",      "SR_CONDO", "2013_2015", "2016_2018",
    "SALE_RATE",         "SR_SF",      "2005",      "2010",
    "SALE_RATE",         "SR_SF",      "2005",      "2018",
    "SALE_RATE",         "SR_SF",      "2010",      "2018",
    "SALE_RATE",         "SR_SF", "2013_2015", "2016_2018"
  )


  # RETURN ------------------------------------------------------------------

  return(change_endyears)

}
