#' @title SHow Typology Map
#' @description This is a temporary function.
#' @param model_pdx16 desc
#' @return a MULTIPOLYGON simple feature (class =  `sf`)
#' @export
show_typology_map <- function(model_pdx16){

  wc_tr <- c("53033026600",
             "53033026700",
             "53033026500",
             "53033026801",
             "53033026802",
             "53033027000")

  model_pdx16_sf <- model_pdx16 %>%
  dplyr::mutate(PDX18_TYPE_FCT = factor(PDX18_TYPE,
                                        levels = c("Susceptible",
                                                   "Early Type 1",
                                                   "Early Type 2",
                                                   "Dynamic",
                                                   "Late",
                                                   "Continued Loss"
                                        ))) %>%
  dplyr::left_join(census_tracts_2016_trimmed,
                   by = c("GEOGRAPHY_ID",
                          "GEOGRAPHY_ID_TYPE",
                          "GEOGRAPHY_NAME",
                          "GEOGRAPHY_TYPE")) %>%
  sf::st_sf() %>%
  sf::st_collection_extract("POLYGON")

my_pal <- c(
  RColorBrewer::brewer.pal(n = 9,'YlOrRd')[c(3,5,6)],
  RColorBrewer::brewer.pal(n = 9,'RdPu')[c(6)],
  RColorBrewer::brewer.pal(n = 9,'YlGnBu')[c(6,7)]
)

pal <- leaflet::colorFactor(my_pal,levels = levels(model_pdx16_sf$PDX18_TYPE_FCT), ordered = TRUE,na.color = 'transparent')

vals <- levels(ordered(model_pdx16_sf$PDX18_TYPE_FCT))

model_pdx16_sf %>%
  sf::st_transform(4326) %>%
  dplyr::mutate(POPUP= paste0(GEOGRAPHY_NAME,": ",GEOGRAPHY_ID)) %>%
  leaflet::leaflet() %>%
  leaflet::addProviderTiles(provider = leaflet::providers$OpenStreetMap) %>%
  leaflet::addPolygons(fillColor = ~pal(PDX18_TYPE_FCT), fillOpacity = .75, smoothFactor = 0,
                       color = 'white', opacity = .85, weight = .5,
                       popup = ~POPUP) %>%
  leaflet::addLegend(title = '',position = 'topright',pal = pal, values = ~PDX18_TYPE_FCT, opacity = .75)

}


