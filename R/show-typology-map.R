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

 wc <- typology %>% filter(GEOID %in% wc_tr)

pal <- leaflet::colorFactor(my_pal,levels = levels(typology$TYPO_FCT), ordered = TRUE,na.color = 'transparent')

vals <- levels(ordered(typology$TYPO_FCT))

typology %>%
  dplyr::mutate(POPUP= paste0(NAME_FULL,": ",TYPO_CHR)) %>%
  leaflet::leaflet() %>%
  addProviderTiles(provider = leaflet::providers$OpenStreetMap) %>%
        addPolygons(fillColor = ~pal(TYPO_FCT), fillOpacity = .75, smoothFactor = 0,
                    color = 'white', opacity = .85, weight = .5,
                    popup = ~POPUP) %>%
        addLegend(title = '',position = 'topright',pal = pal, values = ~TYPO_FCT, opacity = .75) %>%
  addPolygons(data = wc,
              fillOpacity = 0, smoothFactor = 0,
                    color = 'blue', opacity = .75, weight = 1.5) %>%
  addPolygons(data = white_center_place,
              fillOpacity = 0, smoothFactor = 0,
                    color = 'black', opacity = .75, weight = 3)

}


