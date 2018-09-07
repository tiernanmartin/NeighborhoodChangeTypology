#' @title A set of generic helpful functions
#' @description General purpose functions to make working in R easier.
#' @name utils
#' @import purrr
NULL

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
