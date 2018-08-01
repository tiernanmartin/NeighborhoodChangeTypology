
#' @title Make King County's Census Tract Boundaries (2016)
#' @description Download the polygons for the census tracts in King County, WA
#'   sing the \code{\link[tigris]{tigris}} package.
#' @return a MULTIPOLYGON simple feature (class =  `sf`)
#' @importFrom tigris tracts
#' @importFrom sf st_transform
#' @export
make_census_tracts_2016 <- function(){

  options(tigris_class = "sf")

  kc_tracts_2016_ready <- tigris::tracts(state = "53", county = "033", year = 2016) %>%
    sf::st_transform(2926)

  census_tracts_2016 <- kc_tracts_2016_ready

}
