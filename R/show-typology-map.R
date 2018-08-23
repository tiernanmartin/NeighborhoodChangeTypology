#' @title SHow Typology Map
#' @description This is a temporary function.
#' @param typology typology
#' @param white_center_place white center boundary
#' @return a MULTIPOLYGON simple feature (class =  `sf`)
#' @export
show_typology_map <- function(typology, white_center_place){

  my_pal <- c(
        RColorBrewer::brewer.pal(n = 9,'YlOrRd')[c(3,5,6)],
        RColorBrewer::brewer.pal(n = 9,'RdPu')[c(6)],
        RColorBrewer::brewer.pal(n = 9,'YlGnBu')[c(6,7)]
)


 wc_tr <- c("53033026600",
           "53033026700",
           "53033026500",
           "53033026801",
           "53033026802",
           "53033027000")

 wc <- typology %>% dplyr::filter(GEOID %in% wc_tr) %>% sf::st_transform(4326)

pal <- leaflet::colorFactor(my_pal,levels = levels(typology$TYPO_FCT), ordered = TRUE,na.color = 'transparent')

vals <- levels(ordered(typology$TYPO_FCT))

typology %>%
  sf::st_transform(4326) %>%
  dplyr::mutate(POPUP= paste0(GEOID,": ",TYPO_CHR)) %>%
  leaflet::leaflet() %>%
  leaflet::addProviderTiles(provider = leaflet::providers$OpenStreetMap) %>%
  leaflet::addPolygons(fillColor = ~pal(TYPO_FCT), fillOpacity = .75, smoothFactor = 0,
              color = 'white', opacity = .85, weight = .5,
              popup = ~POPUP) %>%
  leaflet::addLegend(title = '',position = 'topright',pal = pal, values = ~TYPO_FCT, opacity = .75) %>%
  leaflet::addPolygons(data = wc,
              fillOpacity = 0, smoothFactor = 0,
              color = 'blue', opacity = .75, weight = 1.5)
}


