#' @title Make the CPI Key for Inflation-Adjusted Dollar Values
#' @description Make the Consumer Price Index key for calculating inflation-adjusted dollar values.
#' @return a named vector (`class = double`)

#' @rdname cpi
#' @export
prepare_cpi <- function(path){

# NOTE: as of 10/3/2018, the blsAPI() function is not working.
#       An issue has been filed with the package developer
#      <https://github.com/mikeasilva/blsAPI/issues/19#issue-366444481>
#      Until this gets resolved, this function should not be used.


  # GET DATA ----------------------------------------------------------------

  uscpi_seriesid <- "CUSR0000SA0"

  payload <- list(
    'seriesid'= uscpi_seriesid,
    'startyear'= 2000,
    'endyear'= 2018,
    'annualaverage' = TRUE)

  bls_results <- get_bls_data(payload = payload,
                              api_version = 2,
                              return_data_frame = TRUE)


  # WRITE -------------------------------------------------------------------

  readr::write_csv(bls_results, path)


  # RETURN ------------------------------------------------------------------

  cpi_prep_status <- NeighborhoodChangeTypology::get_modified_time(path)

  return(cpi_prep_status)

}

#' @rdname cpi
#' @export
make_cpi <- function(path){

  bls_results <- suppressWarnings(suppressMessages(readr::read_csv(path)))

  cpi_2000_2018 <- bls_results %>%
    tibble::as_tibble() %>%
    dplyr::rename_all(toupper) %>%
    dplyr::group_by(YEAR) %>%
    dplyr::summarise(VALUE = mean(as.double(VALUE), na.rm = TRUE))

  cpi_2000_2018_vec <- purrr::pmap(cpi_2000_2018, ~ purrr::set_names(.y,.x)) %>% purrr::flatten_dbl()

  return(cpi_2000_2018_vec)
}
