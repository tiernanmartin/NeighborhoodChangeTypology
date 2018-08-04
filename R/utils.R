#' @title A set of generic helpful functions
#' @description General purpose functions to make working in R easier.
#' @name utils
#' @import purrr
NULL

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
