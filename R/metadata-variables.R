#' @title Prepare Variable Metadata Objects
#' @description Return a `tibble` object containing the metadata used for all o.
#' @param acs_data desc
#' @param present_use_key desc
#' @param condo_unit_type_key desc
#' @param parcel_tract_overlay desc
#' @param parcel_info_2005 desc
#' @param parcel_info_2010 desc
#' @param parcel_info_2018 desc
#' @param condo_info_2005 desc
#' @param condo_info_2010 desc
#' @param condo_info_2018 desc
#' @param res_bldg_2005 desc
#' @param res_bldg_2010 desc
#' @param res_bldg_2018 desc
#' @param variable_template desc
#' @param res_bldg_2018  desc
#' @return a `tibble`

#' @rdname metadata-variables
#' @export
make_parcel_all_metadata <- function(present_use_key,
                                     condo_unit_type_key,
                                     parcel_tract_overlay,
                                     parcel_info_2005,
                                     parcel_info_2010,
                                     parcel_info_2018,
                                     condo_info_2005,
                                     condo_info_2010,
                                     condo_info_2018,
                                     res_bldg_2005,
                                     res_bldg_2010,
                                     res_bldg_2018,
                                     variable_template){

  # PREP: RES_BLDG ----------------------------------------------------------


  parcel_list <- list(parcel_info_2005, parcel_info_2010, parcel_info_2018)

  prep_parcel <- function(x){
    x %>%
      dplyr::rename(META_PRESENT_USE = META_PRESENTUSE) %>%
      dplyr::left_join(present_use_key, by = "META_PRESENT_USE") %>%
      dplyr::transmute(SOURCE,
                       GEOGRAPHY_ID,
                       GEOGRAPHY_ID_TYPE,
                       GEOGRAPHY_NAME,
                       GEOGRAPHY_TYPE,
                       ENDYEAR,
                       META_PRESENT_USE = META_PRESENT_USE_DESC,
                       META_LOT_SQ_FT = META_SQFTLOT
      )

  }


  p_all <- purrr::map_dfr(parcel_list, prep_parcel)

  prep_res_bldg <- function(x){

    x %>% dplyr::transmute(SOURCE,
                           GEOGRAPHY_ID,
                           GEOGRAPHY_ID_TYPE,
                           GEOGRAPHY_NAME,
                           GEOGRAPHY_TYPE,
                           ENDYEAR,
                           META_PROPERTY_CATEGORY = "res",
                           META_BLDG_NBR,
                           META_LIVING_SQ_FT = units::set_units(META_SQ_FT_TOT_LIVING,"ft^2")) %>%
      dplyr::group_by(GEOGRAPHY_ID, ENDYEAR) %>%
      dplyr::mutate(META_NBR_BUILDINGS = dplyr::n()) %>%
      dplyr::ungroup()


  }

  res_bldg_list <- list(res_bldg_2005, res_bldg_2010, res_bldg_2018)

  res_bldg_all <- purrr::map_dfr(res_bldg_list, prep_res_bldg)  %>%
    dplyr::left_join(p_all, by = c("SOURCE",
                                   "GEOGRAPHY_ID",
                                   "GEOGRAPHY_ID_TYPE",
                                   "GEOGRAPHY_NAME",
                                   "GEOGRAPHY_TYPE",
                                   "ENDYEAR"))



  # PREP: CONDO_UNIT --------------------------------------------------------

  prep_condo_unit <- function(x){

    x %>%
      dplyr::left_join(condo_unit_type_key, by = c(META_UNIT_TYPE = "META_CONDO_UNIT_TYPE")) %>%
      dplyr::transmute(SOURCE,
                       GEOGRAPHY_ID,
                       GEOGRAPHY_ID_TYPE,
                       GEOGRAPHY_NAME,
                       GEOGRAPHY_TYPE,
                       ENDYEAR,
                       META_PROPERTY_CATEGORY = "condo",
                       META_CONDO_UNIT_TYPE = META_CONDO_UNIT_TYPE_DESC,
                       META_NBR_BUILDINGS = 1L,
                       META_LIVING_SQ_FT = units::set_units(META_FOOTAGE,"ft^2"))
  }

  condo_list <- list(condo_info_2005, condo_info_2010, condo_info_2018)

  condo_unit_all <- purrr::map_dfr(condo_list, prep_condo_unit)



  # PREP: PROPERTY ----------------------------------------------------------


  # Note: this makes it possible to filter out PRESENT_USE or CONDO_UNIT_TYPE cases
  #       that need to be excluded from the analysis (e.g., commerical condos)

  prop_all_type <- dplyr::bind_rows(res_bldg_all,
                                    condo_unit_all) %>%
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
                  ENDYEAR) %>%
    dplyr::full_join(prop_all_ready,
                     by = c("SOURCE",
                            "GEOGRAPHY_ID",
                            "GEOGRAPHY_ID_TYPE",
                            "GEOGRAPHY_NAME",
                            "GEOGRAPHY_TYPE",
                            "ENDYEAR"))


  parcel_all_metadata <- parcel_all_metadata_ready

  # RETURN ------------------------------------------------------------------

  return(parcel_all_metadata)


}

#' @rdname metadata-variables
#' @export
make_county_tract_all_metadata <- function(acs_data){


# PREPARE DATA ------------------------------------------------------------

  county_tract_all_metadata <-  acs_data %>%
    dplyr::select(GEOGRAPHY_ID,
                  GEOGRAPHY_ID_TYPE,
                  GEOGRAPHY_NAME,
                  GEOGRAPHY_TYPE) %>%
    dplyr::distinct()

  # RETURN ------------------------------------------------------------------

  return(county_tract_all_metadata)

}
