
#' @title Make A Tibble of The Project's Tax Assessor Variables
#' @description Return a `tibble` of all of the
#' @param model_table Tibble, the `model_table` object
#' @param acs_tables Tibble, the `acs_table` object
#' @param path Character, the path or connection to write to.
#' @param zip_path Character, the file path of the archive file
#' @param file_path Character, the file path of the file that is to be extracted
#' @param data_template Tibble, desc
#' @param metadata_template Tibble, desc
#' @return a `tibble`

#' @rdname parcel-data
#' @export
prepare_parcel_data <- function(model_table, acs_tables, zip_path){


  # GET DATA ----------------------------------------------------------------

  # DATA SOURCES:
  #   * 2018 data: https://info.kingcounty.gov/assessor/DataDownload/default.aspx
  #   * pre-2018 data: direct request fulfilled by the King County Assessor Office

  sales <- readr::read_csv("extdata/source/Year2018/EXTR_RPSale.csv") %>%
    janitor::clean_names(case = "screaming_snake")

  val <- readr::read_csv("extdata/source/Year2018/EXTR_ValueHistory_V.csv") %>%
    janitor::clean_names(case = "screaming_snake")

  p_2018 <- readr::read_csv("extdata/source/Year2018/EXTR_Parcel.csv") %>%
    janitor::clean_names(case = "screaming_snake")

  condo_2018 <- readr::read_csv("extdata/source/Year2018/EXTR_CondoUnit2.csv") %>%
    janitor::clean_names(case = "screaming_snake")

  lut_2018 <- readr::read_csv("extdata/source/Year2018/EXTR_LookUp.csv") %>%
    janitor::clean_names(case = "screaming_snake")

  res_bldg_2018 <- readr::read_csv("extdata/source/Year2018/EXTR_ResBldg.csv") %>%
    janitor::clean_names(case = "screaming_snake")

  present_use_lut <- lut_2018 %>%
    dplyr::filter(LU_TYPE %in% 102) %>%
    dplyr::transmute(PRESENT_USE = LU_ITEM,
                     PRESENT_USE_DESC = LU_DESCRIPTION)

  check_present_use <- function(x, n_min){
    x %>%
      dplyr::transmute(PIN = NeighborhoodChangeTypology::make_pin(MAJOR, MINOR),
                       PRESENT_USE) %>%
      dplyr::left_join(present_use_lut, by = "PRESENT_USE") %>%
      tidyr::drop_na() %>%
      dplyr::count(PRESENT_USE_DESC, sort = TRUE) %>%
      dplyr::filter(n > n_min) %>%
      print(n=Inf)
  }

  # check_present_use(p, 100)

  sales_reason_lut <- lut_2018 %>%
    dplyr::filter(LU_TYPE %in% 5) %>%
    dplyr::transmute(SALE_REASON = LU_ITEM,
                     SALE_REASON_DESC = LU_DESCRIPTION)

  check_sale_reason <- function(x, n_min){
    x %>%
      dplyr::transmute(PIN = NeighborhoodChangeTypology::make_pin(MAJOR, MINOR),
                       SALE_REASON) %>%
      dplyr::left_join(sales_reason_lut, by = "SALE_REASON") %>%
      tidyr::drop_na() %>%
      dplyr::count(SALE_REASON_DESC, sort = TRUE) %>%
      dplyr::filter(n > n_min) %>%
      print(n=Inf)
  }

  # check_sale_reason(sales, 100)


  rgdal::ogrListLayers("extdata/source/Year2005.gdb")

  rgdal::ogrListLayers("extdata/source/Year2010.gdb")

  p_2005 <- sf::st_read(dsn = "extdata/source/Year2005.gdb", layer = "parcel_extr")

  condo_2005 <- sf::st_read(dsn = "extdata/source/Year2005.gdb", layer = "condounit_extr")

  lut_2005 <- sf::st_read(dsn = "extdata/source/Year2005.gdb", layer = "lookup_extr")

  res_bldg_2005 <- sf::st_read(dsn = "extdata/source/Year2005.gdb", layer = "resbldg_extr")

  p_2010 <- sf::st_read(dsn = "extdata/source/Year2010.gdb", layer = "parcel_extr")

  condo_2010 <- sf::st_read(dsn = "extdata/source/Year2010.gdb", layer = "condounit_extr")

  res_bldg_2010 <- sf::st_read(dsn = "extdata/source/Year2005.gdb", layer = "resbldg_extr")



  # WRITE DATA --------------------------------------------------------------

  target_dir <- tools::file_path_sans_ext(zip_path)

  suppressWarnings(suppressMessages(dir.create(target_dir)))

  files <- list(sales,
                val,
                p_2018,
                condo_2018,
                lut_2018,
                res_bldg_2018,
                p_2005,
                condo_2005,
                lut_2005,
                res_bldg_2005,
                p_2010,
                res_bldg_2010,
                condo_2010
  )

  file_paths <- list("EXTR_RPSale.csv",
                     "EXTR_ValueHistory_V.csv",
                     "EXTR_Parcel_2018.csv",
                     "EXTR_CondoUnit2_2018.csv",
                     "EXTR_LookUp_2018.csv",
                     "EXTR_ResBldg_2018.csv",
                     "EXTR_Parcel_2005.csv",
                     "EXTR_Condo_Unit_2005.csv",
                     "EXTR_LookUp_2005.csv",
                     "EXTR_ResBldg_2005.csv",
                     "EXTR_Parcel_2010.csv",
                     "EXTR_ResBldg_2010.csv",
                     "EXTR_Condo_Unit_2010.csv"
  ) %>% purrr::map_chr(~ file.path(target_dir,.x))

  purrr::walk2(files, file_paths, readr::write_csv)

  zip_subdirectory(zip_path = zip_path, dir_path = target_dir)


  # RETURN ------------------------------------------------------------------

  parcel_data_prep_status <- NeighborhoodChangeTypology::get_modified_time(zip_path)

  return(parcel_data_prep_status)

}

#' @rdname parcel-data
#' @export
make_parcel_value <- function(data_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_value_filter <- suppressWarnings(suppressMessages(readr::read_csv("extdata/osf/kc-assessor-parcels-2005-2010-2018/EXTR_ValueHistory_V.csv"))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::filter(TAX_STATUS %in% "T") %>% # The object takes up too much memory! Need to filter it down.
    dplyr::filter(TAX_YR %in% c(2005, 2010, 2018))


  parcel_value_long <- parcel_value_filter %>%
    dplyr::rename(VALUE_LAND = LAND_VAL,
                  VALUE_IMPROVEMENT = IMPS_VAL,
                  APPRAISED_VALUE_LAND = APPR_LAND_VAL,
                  APPRAISED_VALUE_IMPROVEMENT = APPR_IMPS_VAL,
                  APPRAISED_VALUE_IMPROVEMENT_INCR = APPR_IMP_INCR
    ) %>%
    tidyr::gather(VARIABLE, ESTIMATE, VALUE_LAND, VALUE_IMPROVEMENT, APPRAISED_VALUE_LAND, APPRAISED_VALUE_IMPROVEMENT, APPRAISED_VALUE_IMPROVEMENT_INCR) %>%
    dplyr::rename_at(dplyr::vars(-dplyr::matches("VARIABLE|ESTIMATE")), dplyr::funs(stringr::str_c("META_",.)))

  parcel_value_ready <- data_template %>%
    dplyr::full_join(parcel_value_long, by = c("VARIABLE",
                                               "ESTIMATE",
                                               ENDYEAR = "META_TAX_YR")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID = make_pin(META_MAJOR, META_MINOR),
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  MEASURE_TYPE = "VALUE",
                  MOE = 0)


  parcel_value <- parcel_value_ready

  return(parcel_value)

}

#' @rdname parcel-data
#' @export
make_parcel_sales <- function(data_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_sales_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake")

  parcel_sales_long <- parcel_sales_raw %>%
    dplyr::mutate(ENDYEAR = as.integer(stringr::str_extract(DOCUMENT_DATE, "\\d{4}$"))) %>%
    tidyr::gather(VARIABLE, ESTIMATE, SALE_PRICE) %>%
    dplyr::rename_at(dplyr::vars(-dplyr::matches("VARIABLE|ESTIMATE")), dplyr::funs(stringr::str_c("META_",.)))


  parcel_sales_ready <-  data_template %>%
    dplyr::full_join(parcel_sales_long, by = c("VARIABLE",
                                               "ESTIMATE",
                                               ENDYEAR = "META_ENDYEAR")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID = make_pin(META_MAJOR, META_MINOR),
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  MEASURE_TYPE = "VALUE",
                  MOE = 0)

  parcel_sales <- parcel_sales_ready

  return(parcel_sales)

}

#' @rdname parcel-data
#' @export
make_parcel_info_2005 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_info_2005_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.)))


  parcel_info_2005 <- metadata_template %>%
    dplyr::full_join(parcel_info_2005_raw, by = c(GEOGRAPHY_ID = "META_PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  ENDYEAR = 2005L)

  return(parcel_info_2005)
}

#' @rdname parcel-data
#' @export
make_parcel_info_2010 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_info_2010_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.)))


  parcel_info_2010 <- metadata_template %>%
    dplyr::full_join(parcel_info_2010_raw, by = c(GEOGRAPHY_ID = "META_PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  ENDYEAR = 2010L)

  return(parcel_info_2010)

}

#' @rdname parcel-data
#' @export
make_parcel_info_2018 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_info_2018_raw <- suppressWarnings(suppressMessages(readr::read_csv("extdata/osf/kc-assessor-parcels-2005-2010-2018/EXTR_Parcel_2018.csv"))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename(PRESENTUSE = PRESENT_USE,
                  SQFTLOT = SQ_FT_LOT) %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.))) %>%
    dplyr::mutate(META_PIN = make_pin(META_MAJOR, META_MINOR))


  parcel_info_2018 <- metadata_template %>%
    dplyr::full_join(parcel_info_2018_raw, by = c(GEOGRAPHY_ID = "META_PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  ENDYEAR = 2018L)

  return(parcel_info_2018)
}

#' @rdname parcel-data
#' @export
make_parcel_lut_2005 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_lut_2005 <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename(LU_TYPE = LUTYPE,
                  LU_ITEM = LUITEM,
                  LU_DESCRIPTION = LUDESCRIPT) %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.)))

  return(parcel_lut_2005)

}

#' @rdname parcel-data
#' @export
make_parcel_lut_2018 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_lut_2018 <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.)))

  return(parcel_lut_2018)
}

#' @rdname parcel-data
#' @export
make_condo_info_2005 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  condo_info_2005_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename(FOOTAGE = FT,
                  UNIT_TYPE = UNITTYPE) %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.)))


  condo_info_2005 <- metadata_template %>%
    dplyr::full_join(condo_info_2005_raw, by = c(GEOGRAPHY_ID = "META_PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  ENDYEAR = 2005L)

  return(condo_info_2005)
}

#' @rdname parcel-data
#' @export
make_condo_info_2010 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  condo_info_2010_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename(FOOTAGE = FT,
                  UNIT_TYPE = UNITTYPE) %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.)))


  condo_info_2010 <- metadata_template %>%
    dplyr::full_join(condo_info_2010_raw, by = c(GEOGRAPHY_ID = "META_PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  ENDYEAR = 2010L)

  return(condo_info_2010)
}

#' @rdname parcel-data
#' @export
make_condo_info_2018 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  condo_info_2018_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.))) %>%
    dplyr::mutate(META_PIN = make_pin(META_MAJOR, META_MINOR))


  condo_info_2018 <- metadata_template %>%
    dplyr::full_join(condo_info_2018_raw, by = c(GEOGRAPHY_ID = "META_PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  ENDYEAR = 2018L)

  return(condo_info_2018)
}

#' @rdname parcel-data
#' @export
make_res_bldg_2005 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  res_bldg_2005_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename(SQ_FT_TOT_LIVING = SQFTTOTLIV,
                  BLDG_NBR = BLDGNBR,
                  YR_BUILT = YRBUILT) %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.)))

  res_bldg_2005 <- metadata_template %>%
    dplyr::full_join(res_bldg_2005_raw, by = c(GEOGRAPHY_ID = "META_PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  ENDYEAR = 2005L)

  return(res_bldg_2005)
}

#' @rdname parcel-data
#' @export
make_res_bldg_2010 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  res_bldg_2010_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename(SQ_FT_TOT_LIVING = SQFTTOTLIV,
                  BLDG_NBR = BLDGNBR,
                  YR_BUILT = YRBUILT) %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.)))

  res_bldg_2010 <- metadata_template %>%
    dplyr::full_join(res_bldg_2010_raw, by = c(GEOGRAPHY_ID = "META_PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  ENDYEAR = 2010L)

  return(res_bldg_2010)
}

#' @rdname parcel-data
#' @export
make_res_bldg_2018 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  res_bldg_2018_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.))) %>%
    dplyr::mutate(META_PIN = make_pin(META_MAJOR, META_MINOR))

  res_bldg_2018 <- metadata_template %>%
    dplyr::full_join(res_bldg_2018_raw, by = c(GEOGRAPHY_ID = "META_PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  ENDYEAR = 2018L)

  return(res_bldg_2018)
}







