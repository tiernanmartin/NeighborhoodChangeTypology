#' @title A set of generic helpful functions
#' @description General purpose functions to make working in R easier.
#' @name utils
#' @import purrr
NULL


#' @export
word2num <- function(word){

  # source: https://stackoverflow.com/a/18333845/5170276

    wsplit <- strsplit(tolower(word)," ")[[1]]
    one_digits <- list(zero=0, one=1, two=2, three=3, four=4, five=5,
                       six=6, seven=7, eight=8, nine=9)
    teens <- list(eleven=11, twelve=12, thirteen=13, fourteen=14, fifteen=15,
                  sixteen=16, seventeen=17, eighteen=18, nineteen=19)
    ten_digits <- list(ten=10, twenty=20, thirty=30, forty=40, fifty=50,
                       sixty=60, seventy=70, eighty=80, ninety=90)
    doubles <- c(teens,ten_digits)
    out <- 0
    i <- 1
    while(i <= length(wsplit)){
        j <- 1
        if(i==1 && wsplit[i]=="hundred")
            temp <- 100
        else if(i==1 && wsplit[i]=="thousand")
            temp <- 1000
        else if(wsplit[i] %in% names(one_digits))
            temp <- as.numeric(one_digits[wsplit[i]])
        else if(wsplit[i] %in% names(teens))
            temp <- as.numeric(teens[wsplit[i]])
        else if(wsplit[i] %in% names(ten_digits))
            temp <- (as.numeric(ten_digits[wsplit[i]]))
        if(i < length(wsplit) && wsplit[i+1]=="hundred"){
            if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
                out <- out + 100*temp
            else
                out <- 100*(out + temp)
            j <- 2
        }
        else if(i < length(wsplit) && wsplit[i+1]=="thousand"){
            if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
                out <- out + 1000*temp
            else
                out <- 1000*(out + temp)
            j <- 2
        }
        else if(i < length(wsplit) && wsplit[i+1] %in% names(doubles)){
            temp <- temp*100
            out <- out + temp
        }
        else{
            out <- out + temp
        }
        i <- i + j
    }
    return(out)
}


#' @export
convert_to_2018_dollars <- function(value, year){

    # Note: this function can only convery sales after the year 1999 -- earlier years will return NA

    # Check that `year` is a 4-digit character

  if(!(is.character(year) & nchar(year) == 4L)){stop("The `year` argument must be a character vector with nchar == 4.\nFor example: '2012'")}

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
create_daterange <- function(x,y){
  x_trim <- stringr::str_remove_all(x,"-")
  y_trim <- stringr::str_remove_all(y,"-")
  stringr::str_c(x_trim, y_trim, sep = "_")
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
