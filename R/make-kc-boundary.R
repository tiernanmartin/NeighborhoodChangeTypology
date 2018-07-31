#' @title Make the Boundary of King County, WA
#' @description Get the polygon of King County, WA using the \code{\link[tigris]{tigris}} package.
#' @return a MULTIPOLYGON simple feature (class =  `sf`)
#' @importFrom tigris counties
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom sf st_transform
#' @export
make_kc_boundary <- function(){

  options(tigris_class = "sf")

  counties <- tigris::counties(state = "53")

  kc <- counties %>%
    select(NAME) %>%
    filter(NAME %in% "King") %>%
    sf::st_transform(2926)

  kc_boundary <- kc

  return(kc_boundary)

}
