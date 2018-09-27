
#' @title Make A Tibble of The Project's Tax Assessor Variables
#' @description Return a `tibble` of all of the
#' @param model_table Tibble, the `model_table` object
#' @param acs_tables Tibble, the `acs_table` object
#' @param path Character, the path or connection to write to.
#' @param zip_path Character, the file path of the archive file
#' @param file_path Character, the file path of the file that is to be extracted
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

  p_2010 <- sf::st_read(dsn = "extdata/source/Year2010.gdb", layer = "parcel_extr")

  condo_2010 <- sf::st_read(dsn = "extdata/source/Year2010.gdb", layer = "condounit_extr")



  # WRITE DATA --------------------------------------------------------------

  target_dir <- tools::file_path_sans_ext(zip_path)

  suppressWarnings(suppressMessages(dir.create(target_dir)))

  files <- list(sales,
                val,
                p_2018,
                condo_2018,
                lut_2018,
                p_2005,
                condo_2005,
                lut_2005,
                p_2010,
                condo_2010
  )

  file_paths <- list("EXTR_RPSale.csv",
                    "EXTR_ValueHistory_V.csv",
                    "EXTR_Parcel.csv",
                    "EXTR_CondoUnit2_2018.csv",
                    "EXTR_LookUp_2018.csv",
                    "EXTR_Parcel_2005.csv",
                    "EXTR_Condo_Unit_2005.csv",
                    "EXTR_LookUp_2005.csv",
                    "EXTR_Parcel_2010.csv",
                    "EXTR_Condo_Unit_2010.csv"
  ) %>% purrr::map_chr(~ file.path(target_dir,.x))

  purrr::walk2(files, file_paths, readr::write_csv)

  zip_subdirectory(zip_path = zip_path, dir_path = target_dir)


  # RETURN ------------------------------------------------------------------

  parcel_data_prep_status <- NeighborhoodChangeTypology::get_modified_time(zip_path)

  return(parcel_data_prep_status)

}

#' @rdname acs-data
#' @export
make_parcel_value <- function(zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_value_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake")

  parcel_value_ready <- parcel_value_raw %>%
    dplyr::filter(TAX_STATUS %in% "T") %>%
    dplyr::filter(TAX_YR %in% c(2005, 2010, 2018)) %>%
    dplyr::transmute(PIN = make_pin(MAJOR, MINOR),
                     VALUE_LAND = LAND_VAL,
                     VALUE_IMPROVEMENT = IMPS_VAL,
                     VALUE_TOTAL = VALUE_LAND + VALUE_IMPROVEMENT,
                     TAX_YEAR = TAX_YR)

  parcel_value <- parcel_value_ready

  return(parcel_value)

}

#' @rdname acs-data
#' @export
make_parcel_sales <- function(zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_sales_raw <- suppressWarnings(suppressMessages(readr::read_csv(file_path)))

  parcel_sales_ready <- parcel_sales_raw # more can be added here if necessary

  parcel_sales <- parcel_sales_ready

  return(parcel_sales)

}

#' @rdname acs-data
#' @export
make_condo_info_2005 <- function(zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  condo_info_2005 <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake")

  return(condo_info_2005)
}

#' @rdname acs-data
#' @export
make_condo_info_2010 <- function(zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  condo_info_2010 <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake")

  return(condo_info_2010)
}

#' @rdname acs-data
#' @export
make_condo_info_2018 <- function(zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  condo_info_2018 <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename(UNITTYPE = UNIT_TYPE) # this facilitates a downstream join with the 2005 and 2010 condo data

  return(condo_info_2018)
}

#' @rdname acs-data
#' @export
make_parcel_info_2005 <- function(zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_info_2005 <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake")

  return(parcel_info_2005)
}

#' @rdname acs-data
#' @export
make_parcel_info_2010 <- function(zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_info_2010 <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake")

  return(parcel_info_2010)

}

#' @rdname acs-data
#' @export
make_parcel_info_2018 <- function(zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_info_2018 <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename(PRESENTUSE = PRESENT_USE,
                  SQFTLOT = SQ_FT_LOT)

  return(parcel_info_2018)
}

#' @rdname acs-data
#' @export
make_parcel_lut_2005 <- function(zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_lut_2005 <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake") %>%
    dplyr::rename(LU_TYPE = LUTYPE,
                  LU_ITEM = LUITEM,
                  LU_DESCRIPTION = LUDESCRIPT)

}

#' @rdname acs-data
#' @export
make_parcel_lut_2018 <- function(zip_path, file_path){

  NeighborhoodChangeTypology::extract_file(zip_path, file_path)

  parcel_lut_2018 <- suppressWarnings(suppressMessages(readr::read_csv(file_path))) %>%
    janitor::clean_names(case = "screaming_snake")

  return(parcel_lut_2018)
}
