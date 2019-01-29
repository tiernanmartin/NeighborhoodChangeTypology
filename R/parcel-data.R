
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

  rgdal::ogrListLayers("extdata/source/Year2015.gdb")

  rgdal::ogrListLayers("extdata/source/Year2016.gdb")


  # 2005

  p_2005 <- sf::st_read(dsn = "extdata/source/Year2005.gdb", layer = "parcel_extr")

  condo_2005 <- sf::st_read(dsn = "extdata/source/Year2005.gdb", layer = "condounit_extr")

  lut_2005 <- sf::st_read(dsn = "extdata/source/Year2005.gdb", layer = "lookup_extr")

  res_bldg_2005 <- sf::st_read(dsn = "extdata/source/Year2005.gdb", layer = "resbldg_extr")


  # 2010

  p_2010 <- sf::st_read(dsn = "extdata/source/Year2010.gdb", layer = "parcel_extr")

  condo_2010 <- sf::st_read(dsn = "extdata/source/Year2010.gdb", layer = "condounit_extr")

  res_bldg_2010 <- sf::st_read(dsn = "extdata/source/Year2010.gdb", layer = "resbldg_extr")


  # 2015

  p_2015 <- sf::st_read(dsn = "extdata/source/Year2015.gdb", layer = "PARCEL_EXTR")

  condo_2015 <- sf::st_read(dsn = "extdata/source/Year2015.gdb", layer = "CONDOUNIT_EXTR")

  res_bldg_2015 <- sf::st_read(dsn = "extdata/source/Year2015.gdb", layer = "RESBLDG_EXTR")


  # 2016

  p_2016 <- sf::st_read(dsn = "extdata/source/Year2016.gdb", layer = "PARCEL_EXTR")


  # 2017

  p_2017 <- readr::read_csv("extdata/source/Year2017/EXTR_Parcel.csv") %>%
    janitor::clean_names(case = "screaming_snake")

  condo_2017 <- readr::read_csv("extdata/source/Year2017/EXTR_CondoUnit2.csv") %>%
    janitor::clean_names(case = "screaming_snake")

  res_bldg_2017 <- readr::read_csv("extdata/source/Year2017/EXTR_ResBldg.csv") %>%
    janitor::clean_names(case = "screaming_snake")


  # WRITE DATA --------------------------------------------------------------

  target_dir <- tools::file_path_sans_ext(zip_path)

  suppressWarnings(suppressMessages(dir.create(target_dir)))

  files_df <- tibble::tribble(
    ~x,                      ~path,
    sales,          "EXTR_RPSale.csv",
    val,  "EXTR_ValueHistory_V.csv",
    p_2018,     "EXTR_Parcel_2018.csv",
    condo_2018, "EXTR_CondoUnit2_2018.csv",
    lut_2018,     "EXTR_LookUp_2018.csv",
    res_bldg_2018,    "EXTR_ResBldg_2018.csv",
    p_2005,     "EXTR_Parcel_2005.csv",
    condo_2005, "EXTR_Condo_Unit_2005.csv",
    lut_2005,     "EXTR_LookUp_2005.csv",
    res_bldg_2005,    "EXTR_ResBldg_2005.csv",
    p_2010,     "EXTR_Parcel_2010.csv",
    res_bldg_2010,    "EXTR_ResBldg_2010.csv",
    condo_2010, "EXTR_Condo_Unit_2010.csv",
    p_2015,     "EXTR_Parcel_2015.csv",
    condo_2015, "EXTR_Condo_Unit_2015.csv",
    res_bldg_2015,    "EXTR_ResBldg_2015.csv",
    p_2016,     "EXTR_Parcel_2016.csv",
    p_2017,     "EXTR_Parcel_2017.csv",
    condo_2017, "EXTR_CondoUnit2_2017.csv",
    res_bldg_2017,    "EXTR_ResBldg_2017.csv"
  ) %>%
    dplyr::mutate(path = stringr::str_c(target_dir, path, sep = "/"))


  purrr::pwalk(files_df, readr::write_csv)

  zip_subdirectory(zip_path = zip_path, dir_path = target_dir)


  # RETURN ------------------------------------------------------------------

  parcel_data_prep_status <- NeighborhoodChangeTypology::get_modified_time(zip_path)

  return(parcel_data_prep_status)

}

#' @rdname parcel-data
#' @export
make_parcel_value <- function(data_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_value_filter <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::filter(TAX_STATUS %in% "T") %>% # The object takes up too much memory! Need to filter it down.
    dplyr::filter(TAX_YR %in% c(2005, 2010, 2013, 2014, 2015, 2016, 2017, 2018))


  parcel_value_long <- parcel_value_filter %>%
    dplyr::mutate(DATE_BEGIN = get_date_end(TAX_YR), # date of the last day of the calendar year
                  DATE_END = get_date_end(TAX_YR), # also the date of the last day of the calendar year
                  DATE_GROUP_ID = as.character(lubridate::year(DATE_END)),
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "one day",
                  VALUE_IMPROVEMENT = IMPS_VAL,
                     VALUE_TOTAL = LAND_VAL) %>%
    dplyr::mutate_if(lubridate::is.Date, as.character) %>%  # convert Date cols to character
    dplyr::select(-dplyr::matches("VAL$")) %>% # remove the original value-related fields
    tidyr::gather(VARIABLE, ESTIMATE, VALUE_IMPROVEMENT, VALUE_TOTAL) %>%
    dplyr::rename_at(dplyr::vars(-dplyr::matches("VARIABLE|ESTIMATE|^DATE")), dplyr::funs(stringr::str_c("META_",.)))


  parcel_value_transformed <- parcel_value_long %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID = make_pin(META_MAJOR, META_MINOR),
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  DATE_GROUP_ID,
                  DATE_BEGIN, # date of the last day of the calendar year
                  DATE_END, # also the date of the last day of the calendar year
                  DATE_RANGE,
                  DATE_RANGE_TYPE,
                  VARIABLE,
                  VARIABLE_SUBTOTAL = NA_character_,
                  VARIABLE_SUBTOTAL_DESC = NA_character_,
                  MEASURE_TYPE = "VALUE",
                  ESTIMATE,
                  MOE = 0
    )

  parcel_value_formatted <- data_template %>%
    dplyr::full_join(parcel_value_transformed,
                     by = c("SOURCE",
                            "GEOGRAPHY_ID",
                            "GEOGRAPHY_ID_TYPE",
                            "GEOGRAPHY_NAME",
                            "GEOGRAPHY_TYPE",
                            "DATE_GROUP_ID",
                            "DATE_BEGIN",
                            "DATE_END",
                            "DATE_RANGE",
                            "DATE_RANGE_TYPE",
                            "VARIABLE",
                            "VARIABLE_SUBTOTAL",
                            "VARIABLE_SUBTOTAL_DESC",
                            "MEASURE_TYPE",
                            "ESTIMATE",
                            "MOE"))

  parcel_value <- parcel_value_formatted

  return(parcel_value)

}

#' @rdname parcel-data
#' @export
make_parcel_sales <- function(data_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_sales_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake")

  parcel_sales_long <- parcel_sales_raw %>%
    # dplyr::mutate(ENDYEAR = as.integer(stringr::str_extract(DOCUMENT_DATE, "\\d{4}$"))) %>%
    tidyr::gather(VARIABLE, ESTIMATE, SALE_PRICE) %>%
    dplyr::rename_at(dplyr::vars(-dplyr::matches("VARIABLE|ESTIMATE")), dplyr::funs(stringr::str_c("META_",.)))

  parcel_sales_transformed <- parcel_sales_long %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID = make_pin(META_MAJOR, META_MINOR),
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  DATE_BEGIN = as.Date(META_DOCUMENT_DATE, format = "%m/%d/%Y"), # date of the sale document
                  DATE_END = as.Date(META_DOCUMENT_DATE, format = "%m/%d/%Y"), # also the date of the sale document
                  DATE_GROUP_ID = as.character(lubridate::year(DATE_END)),
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "one day",
                  VARIABLE,
                  VARIABLE_SUBTOTAL = NA_character_,
                  VARIABLE_SUBTOTAL_DESC = NA_character_,
                  MEASURE_TYPE = "VALUE",
                  ESTIMATE,
                  MOE = 0
    ) %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)

  parcel_sales_formatted <- data_template %>%
    dplyr::full_join(parcel_sales_transformed,
                     by = c("SOURCE",
                            "GEOGRAPHY_ID",
                            "GEOGRAPHY_ID_TYPE",
                            "GEOGRAPHY_NAME",
                            "GEOGRAPHY_TYPE",
                            "DATE_GROUP_ID",
                            "DATE_BEGIN",
                            "DATE_END",
                            "DATE_RANGE",
                            "DATE_RANGE_TYPE",
                            "VARIABLE",
                            "VARIABLE_SUBTOTAL",
                            "VARIABLE_SUBTOTAL_DESC",
                            "MEASURE_TYPE",
                            "ESTIMATE",
                            "MOE"))

  parcel_sales <- parcel_sales_formatted

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
                  DATE_BEGIN = get_date_end(2005L), # date of the last day of the calendar year
                  DATE_END = get_date_end(2005L), # also the date of the last day of the calendar year
                  DATE_GROUP_ID = as.character(lubridate::year(DATE_END)),
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "one day") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)

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
                  DATE_BEGIN = get_date_end(2010L), # date of the last day of the calendar year
                  DATE_END = get_date_end(2010L), # also the date of the last day of the calendar year
                  DATE_GROUP_ID = as.character(lubridate::year(DATE_END)),
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "one day") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)

  return(parcel_info_2010)

}

#' @rdname parcel-data
#' @export
make_parcel_info_2013 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_info_2013_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.)))


  parcel_info_2013 <- metadata_template %>%
    dplyr::full_join(parcel_info_2013_raw, by = c(GEOGRAPHY_ID = "META_PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  DATE_BEGIN = get_date_end(2013L), # date of the last day of the calendar year
                  DATE_END = get_date_end(2013L), # also the date of the last day of the calendar year
                  DATE_GROUP_ID = as.character(lubridate::year(DATE_END)),
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "one day") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)

  return(parcel_info_2013)

}

#' @rdname parcel-data
#' @export
make_parcel_info_2014 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_info_2014_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.)))


  parcel_info_2014 <- metadata_template %>%
    dplyr::full_join(parcel_info_2014_raw, by = c(GEOGRAPHY_ID = "META_PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  DATE_BEGIN = get_date_end(2014L), # date of the last day of the calendar year
                  DATE_END = get_date_end(2014L), # also the date of the last day of the calendar year
                  DATE_GROUP_ID = as.character(lubridate::year(DATE_END)),
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "one day") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)

  return(parcel_info_2014)

}

#' @rdname parcel-data
#' @export
make_parcel_info_2015 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_info_2015_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.)))


  parcel_info_2015 <- metadata_template %>%
    dplyr::full_join(parcel_info_2015_raw, by = c(GEOGRAPHY_ID = "META_PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  DATE_BEGIN = get_date_end(2015L), # date of the last day of the calendar year
                  DATE_END = get_date_end(2015L), # also the date of the last day of the calendar year
                  DATE_GROUP_ID = as.character(lubridate::year(DATE_END)),
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "one day") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)

  return(parcel_info_2015)

}

#' @rdname parcel-data
#' @export
make_parcel_info_2016 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_info_2016_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.)))


  parcel_info_2016 <- metadata_template %>%
    dplyr::full_join(parcel_info_2016_raw, by = c(GEOGRAPHY_ID = "META_PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  DATE_BEGIN = get_date_end(2016L), # date of the last day of the calendar year
                  DATE_END = get_date_end(2016L), # also the date of the last day of the calendar year
                  DATE_GROUP_ID = as.character(lubridate::year(DATE_END)),
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "one day") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)

  return(parcel_info_2016)

}

#' @rdname parcel-data
#' @export
make_parcel_info_2017 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_info_2017_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename(PRESENTUSE = PRESENT_USE,
                  SQFTLOT = SQ_FT_LOT) %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.))) %>%
    dplyr::mutate(META_PIN = make_pin(META_MAJOR, META_MINOR))


  parcel_info_2017 <- metadata_template %>%
    dplyr::full_join(parcel_info_2017_raw, by = c(GEOGRAPHY_ID = "META_PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  DATE_BEGIN = get_date_end(2017L), # date of the last day of the calendar year
                  DATE_END = get_date_end(2017L), # also the date of the last day of the calendar year
                  DATE_GROUP_ID = as.character(lubridate::year(DATE_END)),
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "one day") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)

  return(parcel_info_2017)

}

#' @rdname parcel-data
#' @export
make_parcel_info_2018 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_info_2018_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
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
                  DATE_BEGIN = get_date_end(2018L), # date of the last day of the calendar year
                  DATE_END = get_date_end(2018L), # also the date of the last day of the calendar year
                  DATE_GROUP_ID = as.character(lubridate::year(DATE_END)),
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "one day") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)

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
                  DATE_BEGIN = get_date_end(2005L), # date of the last day of the calendar year
                  DATE_END = get_date_end(2005L), # also the date of the last day of the calendar year
                  DATE_GROUP_ID = as.character(lubridate::year(DATE_END)),
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "one day") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)

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
                  DATE_BEGIN = get_date_end(2010L), # date of the last day of the calendar year
                  DATE_END = get_date_end(2010L), # also the date of the last day of the calendar year
                  DATE_GROUP_ID = as.character(lubridate::year(DATE_END)),
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "one day") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)

  return(condo_info_2010)
}

#' @rdname parcel-data
#' @export
make_condo_info_2013 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  condo_info_2013_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename(FOOTAGE = FT,
                  UNIT_TYPE = UNITTYPE) %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.)))


  condo_info_2013 <- metadata_template %>%
    dplyr::full_join(condo_info_2013_raw, by = c(GEOGRAPHY_ID = "META_PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  DATE_BEGIN = get_date_end(2013L), # date of the last day of the calendar year
                  DATE_END = get_date_end(2013L), # also the date of the last day of the calendar year
                  DATE_GROUP_ID = as.character(lubridate::year(DATE_END)),
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "one day") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)

  return(condo_info_2013)
}

#' @rdname parcel-data
#' @export
make_condo_info_2014 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  condo_info_2014_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename(FOOTAGE = FT,
                  UNIT_TYPE = UNITTYPE) %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.)))


  condo_info_2014 <- metadata_template %>%
    dplyr::full_join(condo_info_2014_raw, by = c(GEOGRAPHY_ID = "META_PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  DATE_BEGIN = get_date_end(2014L), # date of the last day of the calendar year
                  DATE_END = get_date_end(2014L), # also the date of the last day of the calendar year
                  DATE_GROUP_ID = as.character(lubridate::year(DATE_END)),
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "one day") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)

  return(condo_info_2014)
}

#' @rdname parcel-data
#' @export
make_condo_info_2015 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  condo_info_2015_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename(FOOTAGE = FT,
                  UNIT_TYPE = UNITTYPE) %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.)))


  condo_info_2015 <- metadata_template %>%
    dplyr::full_join(condo_info_2015_raw, by = c(GEOGRAPHY_ID = "META_PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  DATE_BEGIN = get_date_end(2015L), # date of the last day of the calendar year
                  DATE_END = get_date_end(2015L), # also the date of the last day of the calendar year
                  DATE_GROUP_ID = as.character(lubridate::year(DATE_END)),
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "one day") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)

  return(condo_info_2015)
}

#' @rdname parcel-data
#' @export
make_condo_info_2017 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  condo_info_2017_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.))) %>%
    dplyr::mutate(META_PIN = make_pin(META_MAJOR, META_MINOR))


  condo_info_2017 <- metadata_template %>%
    dplyr::full_join(condo_info_2017_raw, by = c(GEOGRAPHY_ID = "META_PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  DATE_BEGIN = get_date_end(2017L), # date of the last day of the calendar year
                  DATE_END = get_date_end(2017L), # also the date of the last day of the calendar year
                  DATE_GROUP_ID = as.character(lubridate::year(DATE_END)),
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "one day") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)

  return(condo_info_2017)
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
                  DATE_BEGIN = get_date_end(2018L), # date of the last day of the calendar year
                  DATE_END = get_date_end(2018L), # also the date of the last day of the calendar year
                  DATE_GROUP_ID = as.character(lubridate::year(DATE_END)),
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "one day") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)

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
                  DATE_BEGIN = get_date_end(2005L), # date of the last day of the calendar year
                  DATE_END = get_date_end(2005L), # also the date of the last day of the calendar year
                  DATE_GROUP_ID = as.character(lubridate::year(DATE_END)),
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "one day") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)

  return(res_bldg_2005)
}

#' @rdname parcel-data
#' @export
make_res_bldg_2010 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  res_bldg_2010_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename(SQ_FT_TOT_LIVING = SQFTTOTLIVING,
                  BLDG_NBR = BLDGNBR,
                  YR_BUILT = YRBUILT) %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.)))

  res_bldg_2010 <- metadata_template %>%
    dplyr::full_join(res_bldg_2010_raw, by = c(GEOGRAPHY_ID = "META_PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  DATE_BEGIN = get_date_end(2010L), # date of the last day of the calendar year
                  DATE_END = get_date_end(2010L), # also the date of the last day of the calendar year
                  DATE_GROUP_ID = as.character(lubridate::year(DATE_END)),
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "one day") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)

  return(res_bldg_2010)
}

#' @rdname parcel-data
#' @export
make_res_bldg_2013 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  res_bldg_2013_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename(SQ_FT_TOT_LIVING = SQFTTOTLIVING,
                  BLDG_NBR = BLDGNBR,
                  YR_BUILT = YRBUILT) %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.)))

  res_bldg_2013 <- metadata_template %>%
    dplyr::full_join(res_bldg_2013_raw, by = c(GEOGRAPHY_ID = "META_PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  DATE_BEGIN = get_date_end(2013L), # date of the last day of the calendar year
                  DATE_END = get_date_end(2013L), # also the date of the last day of the calendar year
                  DATE_GROUP_ID = as.character(lubridate::year(DATE_END)),
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "one day") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)

  return(res_bldg_2013)
}

#' @rdname parcel-data
#' @export
make_res_bldg_2014 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  res_bldg_2014_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename(SQ_FT_TOT_LIVING = SQFTTOTLIVING,
                  BLDG_NBR = BLDGNBR,
                  YR_BUILT = YRBUILT) %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.)))

  res_bldg_2014 <- metadata_template %>%
    dplyr::full_join(res_bldg_2014_raw, by = c(GEOGRAPHY_ID = "META_PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  DATE_BEGIN = get_date_end(2014L), # date of the last day of the calendar year
                  DATE_END = get_date_end(2014L), # also the date of the last day of the calendar year
                  DATE_GROUP_ID = as.character(lubridate::year(DATE_END)),
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "one day") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)

  return(res_bldg_2014)
}

#' @rdname parcel-data
#' @export
make_res_bldg_2015 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  res_bldg_2015_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename(SQ_FT_TOT_LIVING = SQFTTOTLIVING,
                  BLDG_NBR = BLDGNBR,
                  YR_BUILT = YRBUILT) %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.)))

  res_bldg_2015 <- metadata_template %>%
    dplyr::full_join(res_bldg_2015_raw, by = c(GEOGRAPHY_ID = "META_PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  DATE_BEGIN = get_date_end(2015L), # date of the last day of the calendar year
                  DATE_END = get_date_end(2015L), # also the date of the last day of the calendar year
                  DATE_GROUP_ID = as.character(lubridate::year(DATE_END)),
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "one day") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)

  return(res_bldg_2015)
}

#' @rdname parcel-data
#' @export
make_res_bldg_2017 <- function(metadata_template, zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  res_bldg_2017_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename_all(dplyr::funs(stringr::str_c("META_",.))) %>%
    dplyr::mutate(META_PIN = make_pin(META_MAJOR, META_MINOR))

  res_bldg_2017 <- metadata_template %>%
    dplyr::full_join(res_bldg_2017_raw, by = c(GEOGRAPHY_ID = "META_PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID_TYPE = "PIN",
                  GEOGRAPHY_NAME = NA_character_,
                  GEOGRAPHY_TYPE = "parcel",
                  DATE_BEGIN = get_date_end(2017L), # date of the last day of the calendar year
                  DATE_END = get_date_end(2017L), # also the date of the last day of the calendar year
                  DATE_GROUP_ID = as.character(lubridate::year(DATE_END)),
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "one day") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)

  return(res_bldg_2017)
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
                  DATE_BEGIN = get_date_end(2018L), # date of the last day of the calendar year
                  DATE_END = get_date_end(2018L), # also the date of the last day of the calendar year
                  DATE_GROUP_ID = as.character(lubridate::year(DATE_END)),
                  DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                  DATE_RANGE_TYPE = "one day") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)

  return(res_bldg_2018)
}







