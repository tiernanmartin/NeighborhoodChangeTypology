
# SETUP -------------------------------------------------------------------

devtools::load_all(".")
library(tidyverse)
library(skimr)

g <- glimpse


# LOAD DATA ---------------------------------------------------------------

res_2016_gdb <- sf::st_read(dsn = "extdata/source/Year2016.gdb", layer = "RESBLDG_EXTR") %>% as_tibble()

res_2016_zip <- "none"

res_2017_gdb <- sf::st_read(dsn = "extdata/source/Year2017.gdb", layer = "RESBLDG_EXTR") %>% as_tibble()

res_2017_zip <- read_csv("extdata/source/Year-2017/EXTR_ResBldg.csv") %>% as_tibble()

condo_2016_gdb <- sf::st_read(dsn = "extdata/source/Year2016.gdb", layer = "CONDOUNIT_EXTR") %>% as_tibble()

condo_2016_zip <- "none"

condo_2017_gdb <- sf::st_read(dsn = "extdata/source/Year2017.gdb", layer = "CONDOUNIT_EXTR") %>% as_tibble()

condo_2017_zip <- read_csv("extdata/source/Year-2017/EXTR_CondoUnit2.csv") %>% as_tibble()


# ANALYSIS: NROW ----------------------------------------------------------


# Check the number of records included in each

list(res_2016_gdb,
     res_2017_gdb,
     res_2017_zip) %>% map(nrow)

list(condo_2016_gdb,
     condo_2017_gdb,
     condo_2017_zip) %>% map(nrow)



# ANALYSIS: RES -----------------------------------------------------------


# Compare the 2017 condo data

list(res_2016_gdb,
     res_2017_gdb,
     res_2017_zip) %>% walk(g)

res_2017_gdb_comp <- res_2017_gdb %>%
  janitor::clean_names(case = "screaming_snake") %>%
  transmute(PIN = make_pin(MAJOR, MINOR),
            BLDG_NBR = BLDGNBR,
            NBR_LIVING_UNITS = NBRLIVINGUNITS,
            SQ_FT_TOT_LIVING = SQFTTOTLIVING,
            YR_BUILT = YRBUILT,
            YR_RENOVATED = YRRENOVATED) %>%
  mutate_if(is.numeric, as.double)

res_2017_zip_comp <- res_2017_zip %>%
  janitor::clean_names(case = "screaming_snake") %>%
  transmute(PIN = make_pin(MAJOR, MINOR),
            BLDG_NBR,
            NBR_LIVING_UNITS,
            SQ_FT_TOT_LIVING,
            YR_BUILT,
            YR_RENOVATED)

list(res_2017_gdb_comp,
     res_2017_zip_comp) %>% walk(g)


res_2017_gdb_comp %>% count(YR_BUILT == 2017) # zero records

res_2017_zip_comp %>% count(YR_BUILT == 2017) #3,868 records


# ANALYSIS: CONDO ---------------------------------------------------------


# Compare the 2017 condo data


list(condo_2016_gdb,
     condo_2017_gdb,
     condo_2017_zip) %>% walk(g)

condo_2017_gdb_comp <- condo_2017_gdb %>%
  janitor::clean_names(case = "screaming_snake") %>%
  transmute(PIN = make_pin(MAJOR, MINOR),
            UNIT_TYPE = UNITTYPE,
            BLDG_NBR = BLDGNUM,
            UNIT_NBR = UNITNUM,
            FOOTAGE = FT)

condo_2017_zip_comp <- condo_2017_zip %>%
  janitor::clean_names(case = "screaming_snake") %>%
  transmute(PIN = make_pin(MAJOR, MINOR),
            UNIT_TYPE,
            BLDG_NBR,
            UNIT_NBR,
            FOOTAGE)

list(condo_2017_gdb_comp,
     condo_2017_zip_comp) %>% walk(g)

condo_2017_gdb_comp %>% anti_join(condo_2017_zip_comp, by = c("PIN","FOOTAGE")) %>% g

condo_2017_zip_comp  %>% anti_join(condo_2017_gdb_comp, by = c("PIN","FOOTAGE")) %>% g





# Aside from the number of records, where do the versions differ from each other?

# some sort of anti_join? semi_join?


# CONCLUSION --------------------------------------------------------------

# The absence of any properties built in 2017 in the `res_2017_gdb` suggests
# that this data is mislabeled and should be replaced by `res_2017_gdb`
# (which contains 3,868 records of properties built in 2017)

