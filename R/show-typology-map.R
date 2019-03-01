#' @title SHow Typology Map
#' @description This is a temporary function.
#' @param model_all desc
#' @return a MULTIPOLYGON simple feature (class =  `sf`)
#' @export
show_typology_map <- function(model_all){

  # assign one of the model objects to model_df

  model_colnames <- c("PDX18_TYPE",       # 1
                      "COO16_TYPE",       # 2
                      "COO18_TYPE",       # 3
                      "COOREV18_MF_TYPE", # 4
                      "COOREV18_SF_TYPE") # 5


  model_ready <- model_all %>%
    dplyr::select(dplyr::starts_with("GEOGRAPHY"),
                  tidyselect::vars_select(names(.),model_colnames[4])) %>%
    dplyr::mutate(COOREV18_MF_TYPE_FCT = factor(COOREV18_MF_TYPE,
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

pal <- leaflet::colorFactor(my_pal,levels = levels(model_ready$COOREV18_MF_TYPE_FCT), ordered = TRUE,na.color = 'transparent')

vals <- levels(ordered(model_ready$COOREV18_MF_TYPE_FCT))

model_ready %>%
  sf::st_transform(4326) %>%
  dplyr::mutate(POPUP= paste0(GEOGRAPHY_NAME,": ",GEOGRAPHY_ID)) %>%
  leaflet::leaflet() %>%
  leaflet::addProviderTiles(provider = leaflet::providers$OpenStreetMap) %>%
  leaflet::addPolygons(fillColor = ~pal(COOREV18_MF_TYPE_FCT), fillOpacity = .75, smoothFactor = 0,
                       color = 'white', opacity = .85, weight = .5,
                       popup = ~POPUP) %>%
  leaflet::addLegend(title = '',position = 'topright',pal = pal, values = ~COOREV18_MF_TYPE_FCT, opacity = .75)

}


