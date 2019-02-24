#' @title Convert to 2018-Dollars
#' @description Convert a dollar value to its inflation-adjusted 2018 dollar value.
#'   Two time series from the US Bureau of Labor Statistics (BLS) are provided: all goods (CUSR0000SA0)
#'   and all good less shelter (CUSR0000SA0L2).
#' @param value numeric
#' @param date_string character, a date-like character string in the following pattern: "YYYY-MM-DD"
#' @param cpi list, the \code{cpi} list object
#' @param series_title character, either \code{"less_shelter"} (default) or \code{"all"}
#' @note A list of all the BLS series id codes is available here: \url{https://download.bls.gov/pub/time.series/cu/cu.series}
#' @return a named list (length == 2) contained named vectors of the inflation adjustment rate for each year.
#' @export
convert_to_2018_dollars <- function(value, date_string, cpi, series_title = "less_shelter"){

  # Note: this function can only convery sales after the year 1999 -- earlier years will return NA

  # Check that `year` is a date-like character string in the following pattern: "YYYY-MM-DD"

  date_pattern <- "^\\d{4}-\\d{2}-\\d{2}$"

  if(any(is.na(value), is.na(date_string))){NA_integer_}

  if(! stringr::str_detect(date_string, date_pattern)){stop("The `date_string` argument must be a date-like character vector in the following format: 'YYYY-MM-DD'.")}

  year <- as.character(lubridate::year(date_string))

  cpi_series <- cpi[[series_title]]

  adj_rate <- cpi_series[as.character(2018)]/cpi_series[year]

  as.integer(round(as.double(value) * adj_rate ,digits = -2) )
}
