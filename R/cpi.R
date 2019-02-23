#' @title Make the CPI Key for Inflation-Adjusted Dollar Values
#' @description Make the Consumer Price Index key for calculating inflation-adjusted dollar values.
#' @return a named vector (`class = double`)

#' @rdname cpi
#' @export
prepare_cpi_all <- function(path){

  # GET DATA ----------------------------------------------------------------

  # NOTE: series id codes here: <https://download.bls.gov/pub/time.series/cu/cu.series>

  seriesid_cpi_all <- "CUSR0000SA0"

  payload <- list(
    'seriesid'= seriesid_cpi_all,
    'startyear'= 2000,
    'endyear'= 2018,
    'annualaverage' = TRUE)

  bls_results <- get_bls_data(payload = payload,
                              api_version = 2,
                              return_data_frame = TRUE)


  # WRITE -------------------------------------------------------------------

  readr::write_csv(bls_results, path)


  # RETURN ------------------------------------------------------------------

  cpi_all_prep_status <- NeighborhoodChangeTypology::get_modified_time(path)

  return(cpi_all_prep_status)

}

#' @rdname cpi
#' @export
make_cpi_all <- function(path){

  bls_results <- suppressWarnings(suppressMessages(readr::read_csv(path)))

  cpi_all_2000_2018 <- bls_results %>%
    tibble::as_tibble() %>%
    dplyr::rename_all(toupper) %>%
    dplyr::group_by(YEAR) %>%
    dplyr::summarise(VALUE = mean(as.double(VALUE), na.rm = TRUE))

  cpi_all_2000_2018_vec <- purrr::pmap(cpi_all_2000_2018, ~ purrr::set_names(.y,.x)) %>% purrr::flatten_dbl()

  return(cpi_all_2000_2018_vec)
}



#' @rdname cpi
#' @export
prepare_cpi_less_shelter <- function(path){

  # GET DATA ----------------------------------------------------------------

  # NOTE 1: series id codes here: <https://download.bls.gov/pub/time.series/cu/cu.series>

  # NOTE 2: when adjusting housing prices or rent for inflation, use CPI less shetler
  # source: <https://twitter.com/DanImmergluck/status/1055105151621574657?s=19>


  seriesid_cpi_less_shelter <- "CUSR0000SA0L2"

  payload <- list(
    'seriesid'= seriesid_cpi_less_shelter,
    'startyear'= 2000,
    'endyear'= 2018,
    'annualaverage' = TRUE)

  bls_results <- get_bls_data(payload = payload,
                              api_version = 2,
                              return_data_frame = TRUE)


  # WRITE -------------------------------------------------------------------

  readr::write_csv(bls_results, path)


  # RETURN ------------------------------------------------------------------

  cpi_less_shelter_prep_status <- NeighborhoodChangeTypology::get_modified_time(path)

  return(cpi_less_shelter_prep_status)

}

#' @rdname cpi
#' @export
make_cpi_less_shelter <- function(path){

  bls_results <- suppressWarnings(suppressMessages(readr::read_csv(path)))

  cpi_less_shelter_2000_2018 <- bls_results %>%
    tibble::as_tibble() %>%
    dplyr::rename_all(toupper) %>%
    dplyr::group_by(YEAR) %>%
    dplyr::summarise(VALUE = mean(as.double(VALUE), na.rm = TRUE))

  cpi_less_shelter_2000_2018_vec <- purrr::pmap(cpi_less_shelter_2000_2018, ~ purrr::set_names(.y,.x)) %>% purrr::flatten_dbl()

  return(cpi_less_shelter_2000_2018_vec)
}
