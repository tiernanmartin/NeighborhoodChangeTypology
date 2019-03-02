#' @title SHow Typology Map
#' @description This is a temporary function.
#' @param model_all desc
#' @param kc_boundary desc
#' @return a MULTIPOLYGON simple feature (class =  `sf`)
#' @export
show_typology_map <- function(model_all, kc_boundary){

  # assign one of the model objects to model_df

  model_colnames <- c("PDX18_TYPE",       # 1
                      "COO16_TYPE",       # 2
                      "COO18_TYPE",       # 3
                      "COOREV18_MF_TYPE", # 4
                      "COOREV18_SF_TYPE") # 5

  kc_boundary <- sf::st_transform(kc_boundary, 4326)

  model_all <- sf::st_transform(model_all, 4326)

  model_ready <- model_all %>%
    dplyr::select(dplyr::starts_with("GEOGRAPHY"),
                  tidyselect::vars_select(names(.),model_colnames[5])) %>%
    dplyr::mutate(COOREV18_SF_TYPE_FCT = factor(COOREV18_SF_TYPE,
                                        levels = c("Susceptible",
                                                   "Early Type 1",
                                                   "Early Type 2",
                                                   "Dynamic",
                                                   "Late",
                                                   "Continued Loss"
                                        )))

  wc_tr <- c("53033026600",
             "53033026700",
             "53033026500",
             "53033026801",
             "53033026802",
             "53033027000")

my_pal <- c(
  RColorBrewer::brewer.pal(n = 9,'YlOrRd')[c(3,5,6)],
  RColorBrewer::brewer.pal(n = 9,'RdPu')[c(6)],
  RColorBrewer::brewer.pal(n = 9,'YlGnBu')[c(6,7)]
)

my_pal_hex <- c("#ffff00",
                "#ffc800",
                "#ff9600",
                "#a30382",
                "#0794d0",
                "#1b5f8f")


pal <- leaflet::colorFactor(my_pal_hex,levels = levels(model_ready$COOREV18_SF_TYPE_FCT), ordered = TRUE,na.color = 'transparent')

vals <- levels(ordered(model_ready$COOREV18_SF_TYPE_FCT))

model_ready %>%
  sf::st_transform(4326) %>%
  dplyr::mutate(POPUP= paste0(GEOGRAPHY_NAME,": ",GEOGRAPHY_ID)) %>%
  leaflet::leaflet() %>%
  leaflet::addMapPane("background_map", zIndex = 410) %>%
  leaflet::addMapPane("polygons", zIndex = 420) %>%
  leaflet::addMapPane("lines", zIndex = 430) %>%
  leaflet::addMapPane("labels", zIndex = 440) %>%
  leaflet::addProviderTiles(provider = leaflet::providers$Stamen.TonerLite,
                            options = pathOptions(pane = "background_map")) %>%
  leaflet::addPolygons(fillColor = ~pal(COOREV18_SF_TYPE_FCT), fillOpacity = 1, smoothFactor = 0,
                       color = 'white', opacity = .85, weight = .5,
                       popup = ~POPUP,
                       options = pathOptions(pane = "polygons")) %>%
  leaflet::addPolygons(data = kc_boundary,
                       fillOpacity = 0, smoothFactor = 0,
                       color = 'black', opacity = .85, weight = 5,
                       options = pathOptions(pane = "polygons")) %>%
    leaflet::addProviderTiles(provider = leaflet::providers$Stamen.TonerLines,
                            options = pathOptions(pane = "lines")) %>%
  leaflet::addProviderTiles(provider = leaflet::providers$Stamen.TonerLabels,
                            options = pathOptions(pane = "labels"))
  # leaflet::addLegend(title = '',position = 'topright',pal = pal, values = ~COO16_TYPE_FCT, opacity = 1)

}


