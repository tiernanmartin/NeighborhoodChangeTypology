#' @title Prepare Variable Metadata Objects
#' @description Return a `tibble` object containing the metadata used for all o.
#' @param acs_data desc
#' @param present_use_key desc
#' @param condo_unit_type_key desc
#' @param parcel_tract_overlay desc
#' @param parcel_info_2005 desc
#' @param parcel_info_2010 desc
#' @param parcel_info_2015 desc
#' @param parcel_info_2016 desc
#' @param parcel_info_2017 desc
#' @param parcel_info_2018 desc
#' @param condo_info_2005 desc
#' @param condo_info_2010 desc
#' @param condo_info_2015 desc
#' @param condo_info_2017 desc
#' @param condo_info_2018 desc
#' @param res_bldg_2005 desc
#' @param res_bldg_2010 desc
#' @param res_bldg_2015 desc
#' @param res_bldg_2017 desc
#' @param res_bldg_2018 desc
#' @param parcel_sales  desc
#' @param variable_template desc
#' @return a `tibble`

#' @rdname metadata-variables
#' @export
make_parcel_all_metadata <- function(present_use_key,
                                     condo_unit_type_key,
                                     parcel_tract_overlay,
                                     parcel_info_2005,
                                     parcel_info_2010,
                                     parcel_info_2015,
                                     parcel_info_2016,
                                     parcel_info_2017,
                                     parcel_info_2018,
                                     condo_info_2005,
                                     condo_info_2010,
                                     condo_info_2015,
                                     condo_info_2017,
                                     condo_info_2018,
                                     res_bldg_2005,
                                     res_bldg_2010,
                                     res_bldg_2015,
                                     res_bldg_2017,
                                     res_bldg_2018,
                                     parcel_sales,
                                     variable_template){


  # PREP PARCEL_INFO --------------------------------------------------------

  parcel_list <- list(parcel_info_2005,
                      parcel_info_2010,
                      parcel_info_2015,
                      parcel_info_2016,
                      parcel_info_2017,
                      parcel_info_2018)

  prep_parcel <- function(x){
    x %>%
      dplyr::rename(META_PRESENT_USE = META_PRESENTUSE) %>%
      dplyr::left_join(present_use_key, by = "META_PRESENT_USE") %>%
      dplyr::transmute(SOURCE,
                       GEOGRAPHY_ID,
                       GEOGRAPHY_ID_TYPE,
                       GEOGRAPHY_NAME,
                       GEOGRAPHY_TYPE,
                       DATE_BEGIN,
                       DATE_END,
                       DATE_END_YEAR,
                       DATE_RANGE,
                       DATE_RANGE_TYPE,
                       META_PRESENT_USE = META_PRESENT_USE_DESC,
                       META_LOT_SQ_FT = META_SQFTLOT
      )

  }


  p_all <- purrr::map_dfr(parcel_list, prep_parcel)

  # PREP: RES_BLDG ----------------------------------------------------------


  prep_res_bldg <- function(x){

    x %>% dplyr::transmute(SOURCE,
                           GEOGRAPHY_ID,
                           GEOGRAPHY_ID_TYPE,
                           GEOGRAPHY_NAME,
                           GEOGRAPHY_TYPE,
                           DATE_BEGIN,
                           DATE_END,
                           DATE_END_YEAR,
                           DATE_RANGE,
                           DATE_RANGE_TYPE,
                           META_PROPERTY_CATEGORY = "res",
                           META_BLDG_NBR,
                           META_LIVING_SQ_FT = units::set_units(META_SQ_FT_TOT_LIVING,"ft^2")) %>%
      dplyr::group_by(GEOGRAPHY_ID, DATE_END) %>%
      dplyr::mutate(META_NBR_BUILDINGS = dplyr::n()) %>%
      dplyr::ungroup()


  }

  res_bldg_list <- list(res_bldg_2005,
                        res_bldg_2010,
                        res_bldg_2015,
                        res_bldg_2017, # res_bldg_2016 is unnavailable but it will be imputed in the next step
                        res_bldg_2018)

  res_bldg_no_2016 <- purrr::map_dfr(res_bldg_list, prep_res_bldg)


  sale_2016 <- parcel_sales %>%
    dplyr::filter(DATE_END_YEAR %in% "2016") %>%
    dplyr::transmute(GEOGRAPHY_ID,
                     SOLD_2016 = TRUE)

  res_2015_2017_distinct <- res_bldg_no_2016 %>%
    dplyr::filter(DATE_END_YEAR %in% c("2015", "2017")) %>%
    dplyr::left_join(sale_2016, by = "GEOGRAPHY_ID") %>%
    dplyr::mutate(SOLD_2016 = dplyr::case_when(
      is.na(SOLD_2016) ~ FALSE,
      TRUE ~ TRUE
    )) %>%
    dplyr::distinct()

  res_2015_2017_wide <- res_2015_2017_distinct %>%
    tidyr::unite("GEOGRAPHY_ID_BLDG", c(GEOGRAPHY_ID, META_BLDG_NBR)) %>%
    tidyr::gather(META_VAR, VAL, dplyr::matches("META")) %>%
    dplyr::mutate(YEAR = stringr::str_c("YEAR_", DATE_END_YEAR)) %>%
    dplyr::select(-dplyr::starts_with("DATE")) %>%
    tidyr::spread(YEAR, VAL)

  res_2016_raw <- res_2015_2017_wide  %>%
    dplyr::mutate(YEAR_2016 = dplyr::case_when(
      YEAR_2015 %in% YEAR_2017 ~ YEAR_2017, # if they're the same, return the 2017 value
      SOLD_2016 ~ YEAR_2017, # if not the same and the prop sold in 2016, return 2017 value
      is.na(YEAR_2015) ~ YEAR_2017,
      is.na(YEAR_2017) ~ YEAR_2015,
      TRUE ~ NA_character_
    )) %>%
    tidyr::gather(VAR, VAL, dplyr::matches("YEAR_")) %>%
    dplyr::mutate(VAR = stringr::str_remove(VAR,"YEAR_")) %>%
    dplyr::rename(DATE_END_YEAR = VAR) %>%
    tidyr::spread(META_VAR, VAL) %>%
    dplyr::filter(DATE_END_YEAR %in% "2016") %>%
    dplyr::select(-SOLD_2016)

  res_2016_ready <- res_2016_raw %>%
    tidyr::separate(GEOGRAPHY_ID_BLDG, into = c("GEOGRAPHY_ID", "META_BLDG_NBR"), sep = "_") %>%
    dplyr::transmute(SOURCE,
                     GEOGRAPHY_ID,
                     GEOGRAPHY_ID_TYPE,
                     GEOGRAPHY_NAME,
                     GEOGRAPHY_TYPE,
                     DATE_BEGIN = "2016-12-31",
                     DATE_END = "2016-12-31",
                     DATE_END_YEAR = "2016",
                     DATE_RANGE = "20161231_20161231",
                     DATE_RANGE_TYPE = "one day",
                     META_PROPERTY_CATEGORY,
                     META_BLDG_NBR = as.double(META_BLDG_NBR), # make it match the class in res_bldg_no_2016
                     META_LIVING_SQ_FT = as.double(META_LIVING_SQ_FT), # make it match the class in res_bldg_no_2016
                     META_NBR_BUILDINGS = as.integer(META_NBR_BUILDINGS) # make it match the class in res_bldg_no_2016
                     )

  res_bldg_all <- list(res_bldg_no_2016,
                       res_2016_ready) %>%
    purrr::map_dfr(c) %>%
    dplyr::left_join(p_all, by = c("SOURCE",
                                   "GEOGRAPHY_ID",
                                   "GEOGRAPHY_ID_TYPE",
                                   "GEOGRAPHY_NAME",
                                   "GEOGRAPHY_TYPE",
                                   "DATE_BEGIN",
                                   "DATE_END",
                                   "DATE_END_YEAR",
                                   "DATE_RANGE",
                                   "DATE_RANGE_TYPE"))


  # PREP: CONDO_UNIT  --------------------------------------------------------

  prep_condo_unit <- function(x){

    x %>%
      dplyr::left_join(condo_unit_type_key, by = c(META_UNIT_TYPE = "META_CONDO_UNIT_TYPE")) %>%
      dplyr::transmute(SOURCE,
                       GEOGRAPHY_ID,
                       GEOGRAPHY_ID_TYPE,
                       GEOGRAPHY_NAME,
                       GEOGRAPHY_TYPE,
                       DATE_BEGIN,
                       DATE_END,
                       DATE_END_YEAR,
                       DATE_RANGE,
                       DATE_RANGE_TYPE,
                       META_PROPERTY_CATEGORY = "condo",
                       META_CONDO_UNIT_TYPE = META_CONDO_UNIT_TYPE_DESC,
                       META_NBR_BUILDINGS = 1L,
                       META_LIVING_SQ_FT = units::set_units(META_FOOTAGE,"ft^2"))
  }

  condo_list <- list(condo_info_2005,
                        condo_info_2010,
                        condo_info_2015,
                        condo_info_2017, # condo_info_2016 is unnavailable but it will be imputed in the next step
                        condo_info_2018)

  condo_no_2016 <- purrr::map_dfr(condo_list, prep_condo_unit)

  condo_2015_2017_distinct <- condo_no_2016 %>%
    dplyr::filter(DATE_END_YEAR %in% c("2015", "2017")) %>%
    dplyr::left_join(sale_2016, by = "GEOGRAPHY_ID") %>%
    dplyr::mutate(SOLD_2016 = dplyr::case_when(
      is.na(SOLD_2016) ~ FALSE,
      TRUE ~ TRUE
    )) %>%
    dplyr::distinct()

  condo_2015_2017_wide <- condo_2015_2017_distinct %>%
    tidyr::gather(META_VAR, VAL, dplyr::matches("META")) %>%
    dplyr::mutate(YEAR = stringr::str_c("YEAR_", DATE_END_YEAR)) %>%
    dplyr::select(-dplyr::starts_with("DATE")) %>%
    tidyr::spread(YEAR, VAL)

  condo_2016_raw <- condo_2015_2017_wide  %>%
    dplyr::mutate(YEAR_2016 = dplyr::case_when(
      YEAR_2015 %in% YEAR_2017 ~ YEAR_2017, # if they're the same, return the 2017 value
      SOLD_2016 ~ YEAR_2017, # if not the same and the prop sold in 2016, return 2017 value
      is.na(YEAR_2015) ~ YEAR_2017,
      is.na(YEAR_2017) ~ YEAR_2015,
      TRUE ~ NA_character_
    )) %>%
    tidyr::gather(VAR, VAL, dplyr::matches("YEAR_")) %>%
    dplyr::mutate(VAR = stringr::str_remove(VAR,"YEAR_")) %>%
    dplyr::rename(DATE_END_YEAR = VAR) %>%
    tidyr::spread(META_VAR, VAL) %>%
    dplyr::filter(DATE_END_YEAR %in% "2016") %>%
    dplyr::select(-SOLD_2016)

  condo_2016_ready <- condo_2016_raw %>%
    dplyr::transmute(SOURCE,
                     GEOGRAPHY_ID,
                     GEOGRAPHY_ID_TYPE,
                     GEOGRAPHY_NAME,
                     GEOGRAPHY_TYPE,
                     DATE_BEGIN = "2016-12-31",
                     DATE_END = "2016-12-31",
                     DATE_END_YEAR = "2016",
                     DATE_RANGE = "20161231_20161231",
                     DATE_RANGE_TYPE = "one day",
                     META_PROPERTY_CATEGORY,
                     META_CONDO_UNIT_TYPE,
                     META_LIVING_SQ_FT = as.double(META_LIVING_SQ_FT), # make it match the class in res_bldg_no_2016
                     META_NBR_BUILDINGS = as.integer(META_NBR_BUILDINGS) # make it match the class in res_bldg_no_2016
                     )

  condo_all <- list(condo_no_2016,
                       condo_2016_ready) %>%
    purrr::map_dfr(c)

  # PREP: PROPERTY (RES + CONDO) ----------------------------------------------------------


  # Note: this makes it possible to filter out PRESENT_USE or CONDO_UNIT_TYPE cases
  #       that need to be excluded from the analysis (e.g., commerical condos)

  prop_all_type <- dplyr::bind_rows(res_bldg_all,
                                    condo_all) %>%
    dplyr::mutate(META_PRESENT_USE = dplyr::case_when(
      META_PROPERTY_CATEGORY %in% "condo" ~ "not res",
      TRUE ~ META_PRESENT_USE
    ),
    META_CONDO_UNIT_TYPE = dplyr::case_when(
      META_PROPERTY_CATEGORY %in% "res" ~ "not condo",
      TRUE ~ META_CONDO_UNIT_TYPE
    )
    )

  # Note: join the tract GEOID to each record

  prop_all_geoid <- prop_all_type %>%
    dplyr::left_join(parcel_tract_overlay, by = c(GEOGRAPHY_ID = "PIN")) %>%
    dplyr::rename(META_TRACT_GEOID = GEOID)

  prop_all_ready <- prop_all_geoid

  # ARRANGE COLUMNS WITH TEMPLATE -------------------------------------------


  parcel_all_metadata_ready <- variable_template %>% # drop the columns related to the VARIABLE, ESTIMATE, or MOE
    dplyr::select(SOURCE,
                  GEOGRAPHY_ID,
                  GEOGRAPHY_ID_TYPE,
                  GEOGRAPHY_NAME,
                  GEOGRAPHY_TYPE,
                  DATE_BEGIN,
                  DATE_END,
                  DATE_END_YEAR,
                  DATE_RANGE,
                  DATE_RANGE_TYPE) %>%
    dplyr::full_join(prop_all_ready,
                     by = c("SOURCE",
                            "GEOGRAPHY_ID",
                            "GEOGRAPHY_ID_TYPE",
                            "GEOGRAPHY_NAME",
                            "GEOGRAPHY_TYPE",
                            "DATE_BEGIN",
                            "DATE_END",
                            "DATE_END_YEAR",
                            "DATE_RANGE",
                            "DATE_RANGE_TYPE"))

    parcel_all_metadata <- parcel_all_metadata_ready

# CHECK THE COUNTS (PROP_TYPE by YEAR) ------------------------------------

  check_prop_type_by_year <- function(){
     parcel_all_metadata_ready %>% dplyr::count(META_PROPERTY_CATEGORY, DATE_END_YEAR)
  }


  # RETURN ------------------------------------------------------------------

  return(parcel_all_metadata)


}
#' @rdname metadata-variables
#' @export
make_community_metadata <- function(){


  # Note: this object is intended to be joined to the census tract data (by = GEOGRAPHY_ID)
  # and then summarized by GEOGRAPHY_COMMUNITY_NAME

  # COO Communities

  community_geography_geoids <- tibble::tribble(
    ~GEOGRAPHY_ID, ~GEOGRAPHY_COMMUNITY_NAME,
    "53033010001",          "Rainier Valley",
    "53033010300",          "Rainier Valley",
    "53033010401",          "Rainier Valley",
    "53033011001",          "Rainier Valley",
    "53033011002",          "Rainier Valley",
    "53033011101",          "Rainier Valley",
    "53033011102",          "Rainier Valley",
    "53033011700",          "Rainier Valley",
    "53033011800",          "Rainier Valley",
    "53033011900",          "Rainier Valley",
    "53033026600",            "White Center",
    "53033026700",            "White Center",
    "53033026500",            "White Center",
    "53033026801",            "White Center",
    "53033026802",            "White Center",
    "53033027000",            "White Center",
    "53033026200",          "Seatac/Tukwila",
    "53033027300",          "Seatac/Tukwila",
    "53033028000",          "Seatac/Tukwila",
    "53033028100",          "Seatac/Tukwila",
    "53033028300",          "Seatac/Tukwila",
    "53033028402",          "Seatac/Tukwila",
    "53033028403",          "Seatac/Tukwila",
    "53033028500",          "Seatac/Tukwila",
    "53033028700",          "Seatac/Tukwila",
    "53033028801",          "Seatac/Tukwila",
    "53033028802",          "Seatac/Tukwila",
    "53033029101",          "Seatac/Tukwila",
    "53033026100",          "Seatac/Tukwila",
    "53033026200",          "Seatac/Tukwila",
    "53033026300",          "Seatac/Tukwila",
    "53033026400",          "Seatac/Tukwila",
    "53033027100",          "Seatac/Tukwila",
    "53033027200",          "Seatac/Tukwila",
    "53033027300",          "Seatac/Tukwila",
    "53033028100",          "Seatac/Tukwila",
    "53033028200",          "Seatac/Tukwila",
    "53033028300",          "Seatac/Tukwila",
    "53033028802",          "Seatac/Tukwila"
  )

  community_metadata <- community_geography_geoids %>%
    dplyr::mutate(GEOGRAPHY_COMMUNITY_ID = dplyr::case_when(
      GEOGRAPHY_COMMUNITY_NAME %in% "Rainier Valley" ~ "RV",
      GEOGRAPHY_COMMUNITY_NAME %in% "White Center" ~ "WC",
      GEOGRAPHY_COMMUNITY_NAME %in% "Seatac/Tukwila" ~ "STC_TUK"
    ),
    GEOGRAPHY_COMMUNITY_ID_TYPE = "abbr",
    GEOGRAPHY_COMMUNITY_TYPE = "community")

  return(community_metadata)

}

#' @rdname metadata-variables
#' @export
make_county_community_tract_all_metadata <- function(acs_data, community_metadata){


  # PREPARE DATA ------------------------------------------------------------

  comm_ready <- community_metadata %>%
    dplyr::select(-GEOGRAPHY_ID) %>% # remove the tract GEOID
    dplyr::rename_all(dplyr::funs(stringr::str_remove(.,"_COMMUNITY"))) %>%  # rename columns to match `variable_template`
    dplyr::distinct()

  county_community_tract_all_metadata <- list(acs_data,
                                              comm_ready) %>%
    purrr::map_dfr(c) %>%
    dplyr::select(GEOGRAPHY_ID,
                  GEOGRAPHY_ID_TYPE,
                  GEOGRAPHY_NAME,
                  GEOGRAPHY_TYPE) %>%
    dplyr::distinct()

  # RETURN ------------------------------------------------------------------

  return(county_community_tract_all_metadata)

}
