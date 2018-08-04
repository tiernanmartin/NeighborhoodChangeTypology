
#' @title Make a Parcel Identification Number
#' @description Make a complete ten-digit Parcel Identification Number.
#' @return a `tibble`
#' @param major Character, the first six (6) digits of King County's
#'   ten-digit Parcel Identification number
#' @param minor Character, the last four (4) digits of King County's
#'   ten-digit Parcel Identification number
#' @export
make_pin <- function( major, minor){
  stringr::str_c(stringr::str_pad(string = major, width = 6,side = "left",pad = "0"),
               stringr::str_pad(string = minor, width = 4,side = "left",pad = "0"))
}
