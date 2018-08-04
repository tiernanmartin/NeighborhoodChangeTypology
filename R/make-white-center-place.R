#' @title Make White Center Place
#' @description This is a temporary function.
#' @return a MULTIPOLYGON simple feature (class =  `sf`)
#' @export
make_white_center_place <- function(){

  options(tigris_class = "sf")

  white_center_place <- tigris::places(state = "WA") %>%
    filter(NAME %in% "White Center") %>%
    sf::st_transform(4326)
}
