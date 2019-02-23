#' @title Make the CPI Key for Inflation-Adjusted Dollar Values
#' @description Make the Consumer Price Index key for calculating inflation-adjusted dollar values.
#' @return a named vector (`class = double`)

#' @rdname cpi
#' @export
prepare_cpi <- function(path){

  # GET DATA ----------------------------------------------------------------

  # NOTE 1: series id codes here: <https://download.bls.gov/pub/time.series/cu/cu.series>

  # NOTE 2: when adjusting housing prices or rent for inflation, use CPI less shetler
  # source: <https://twitter.com/DanImmergluck/status/1055105151621574657?s=19>


  series_tbl <- tibble::tibble("seriesid" = c("CUSR0000SA0", "CUSR0000SA0L2"),
                        "series_title" = c("all","less_shelter"))

  get_multiple_bls <- function(seriesid, series_title){

    bls_data <- get_bls_data(payload = list('seriesid'= seriesid,
                                'startyear'= 2000,
                                'endyear'= 2018,
                                'annualaverage' = TRUE),
                 api_version = 2,
                 return_data_frame = TRUE)

    bls_data["series_title"] <- series_title

    return(bls_data)
  }

  bls_results <- purrr::pmap_dfr(series_tbl, get_multiple_bls)



  # WRITE -------------------------------------------------------------------

  readr::write_csv(bls_results, path)


  # RETURN ------------------------------------------------------------------

  cpi_all_prep_status <- NeighborhoodChangeTypology::get_modified_time(path)

  return(cpi_all_prep_status)

}

#' @rdname cpi
#' @export
make_cpi <- function(path){

  bls_results <- suppressWarnings(suppressMessages(readr::read_csv(path)))

  cpi_all_2000_2018 <- bls_results %>%
    tibble::as_tibble() %>%
    dplyr::rename_all(snakecase::to_screaming_snake_case) %>%
    dplyr::group_by(YEAR, SERIES_TITLE) %>%
    dplyr::summarise(VALUE = mean(as.double(VALUE), na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::ungroup()

  # this is a bit crazy, but it works
  # it creates a named list (length = 2) and each list item contains a named vector

  cpi_all_2000_2018_list <- cpi_all_2000_2018 %>%
    split(factor(.$SERIES_TITLE)) %>%
    purrr::map(~ dplyr::select(.x, YEAR, VALUE)) %>%
    purrr::map(~ purrr::pmap(.x, ~ purrr::set_names(.y,.x)) %>% purrr::flatten_dbl())


  cpi <- cpi_all_2000_2018_list

  return(cpi)
}
