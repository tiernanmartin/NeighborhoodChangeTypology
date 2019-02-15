#' @title A set of generic helpful functions
#' @description General purpose functions to make working in R easier.
#' @name utils
#' @import purrr
NULL

#' @export
convert_to_complex_pin <- function(x){

  # converts a condo unit PIN (parcel identification number) in the condo complex PIN

  stringr::str_replace(x,".{4}$","0000")
  }

#' @export
rm_gc <- function(x){rm(x)
  gc(verbose = FALSE)
  invisible(NULL)
}

#' @export
scale_pct_points <- function(x){
  label <- stringr::str_c(scales::percent(x)," pts")
  label <- ifelse(x>0,
                  stringr::str_c("+",label),
                  label)
  label <- ifelse(x==0,
                  "0",
                  label)
  return(label)
}

#' @export
convert_to_2018_dollars <- function(value, date_string, cpi){

  # Note: this function can only convery sales after the year 1999 -- earlier years will return NA

  # Check that `year` is a date-like character string in the following pattern: "YYYY-MM-DD"

  date_pattern <- "^\\d{4}-\\d{2}-\\d{2}$"

  if(! stringr::str_detect(date_string, date_pattern)){stop("The `date_string` argument must be a date-like character vector in the following format: 'YYYY-MM-DD'.")}

  year <- as.character(lubridate::year(date_string))

  adj_rate <- cpi[as.character(2018)]/cpi[year]

  as.integer(round(as.double(value) * adj_rate ,digits = -2) )
}


#' @export
get_date_begin <- function(x){
  as.Date(stringr::str_c(x,"-01-01")) %>% lubridate::floor_date(unit = "year") %>% lubridate::ymd()
}

#' @export
get_date_end <- function(x){
  as.Date(stringr::str_c(x,"-01-01")) %>%
    lubridate::ceiling_date(unit = "year") %>%
    magrittr::subtract(1) %>%
    lubridate::ymd()
}

#' @export
get_year_quarter <- function(date_x){
    stringr::str_c(lubridate::year(date_x),"Q",lubridate::quarter(date_x))
  }


#' @export
create_range_quarter <- function(date_x, date_y){
  stringr::str_c(get_year_quarter(date_x), get_year_quarter(date_y), sep = "_")

}


#' @export
create_range_year <- function(year_x, year_y){
  stringr::str_c(lubridate::year(year_x), lubridate::year(year_y), sep = "_")

}

#' @export
create_range_date <- function(x,y){

  x_trim <- stringr::str_remove_all(x,"-")
  y_trim <- stringr::str_remove_all(y,"-")


  range_date <- stringr::str_c(x_trim, y_trim, sep = "_")

  return(range_date)
}


#' @export
not_sfc <- function(x) !any(class(x) %in% 'sfc')

first_not_na <- function(x){
  if(all(sapply(x,is.na))){
    methods::as(NA,class(x))
  }else{
    x[!sapply(x,is.na)][1]
  }


}

st_intersect_filter <- function(x,y){
  ids <- lengths(sf::st_intersects(x, y))>0

  x[ids,]
}

#' @export
get_modified_time <- function(path){glue::glue("'{path}' modified at: {file.mtime(path)}.")}


#' @export
extract_file <- function(zip_path, file_path){

  split_file_path <- file_path %>%
    stringr::str_split(pattern = "(?<=osf)/") %>%  # split at the '/' sign preceeded by 'osf'
    purrr::flatten_chr() %>%
    purrr::set_names(c("exdir","files"))

  unzip(zipfile = zip_path,
        files = split_file_path["files"],
        exdir = split_file_path["exdir"])

  invisible(NULL)
}

#' @export
zip_subdirectory <- function(zip_path, dir_path){

  proj_wd <- getwd()

  zip_wd <- dirname(dir_path)

  setwd(zip_wd)

  zip(zipfile = basename(zip_path), files = basename(dir_path))

  setwd(proj_wd)

}
