#' @title SHow Typology Map
#' @description This is a temporary function.
#' @param model_all desc
#' @param kc_boundary desc
#' @return a leaflet map

#' @rdname show-typology-map
#' @export
show_typology_map <- function(){

  # loadd the objects

  drake::loadd(model_all, kc_boundary)

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

my_pal_hex <- c("#f5eb12",
                "#fcc811",
                "#f89621",
                "#9e2382",
                "#0d94ce",
                "#1c6091")


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
                            options = leaflet::pathOptions(pane = "background_map")) %>%
  leaflet::addPolygons(fillColor = ~pal(COOREV18_SF_TYPE_FCT), fillOpacity = 1, smoothFactor = 0,
                       color = 'white', opacity = .85, weight = .5,
                       popup = ~POPUP,
                       options = leaflet::pathOptions(pane = "polygons")) %>%
  leaflet::addPolygons(data = kc_boundary,
                       fillOpacity = 0, smoothFactor = 0,
                       color = 'black', opacity = .85, weight = 5,
                       options = leaflet::pathOptions(pane = "polygons")) %>%
    leaflet::addProviderTiles(provider = leaflet::providers$Stamen.TonerLines,
                            options = leaflet::pathOptions(pane = "lines")) %>%
  leaflet::addProviderTiles(provider = leaflet::providers$Stamen.TonerLabels,
                            options = leaflet::pathOptions(pane = "labels"))
  # leaflet::addLegend(title = '',position = 'topright',pal = pal, values = ~COO16_TYPE_FCT, opacity = 1)

}

#' @rdname show-typology-map
#' @export
show_pdx18_sf_map <- function(){

  # loadd the objects

  drake::loadd(model_all, kc_boundary)

   # assign one of the model objects to model_df

  model_colnames <- c("PDX18_TYPE",       # 1
                      "PDX18_TYPE",       # 2
                      "PDX18_TYPE",       # 3
                      "COOREV18_MF_TYPE", # 4
                      "PDX18_TYPE") # 5

  kc_boundary <- sf::st_transform(kc_boundary, 4326)

  model_all <- sf::st_transform(model_all, 4326)

  model_ready <- model_all %>%
    dplyr::select(dplyr::starts_with("GEOGRAPHY"),
                  tidyselect::vars_select(names(.),model_colnames[1])) %>%
    dplyr::mutate(PDX18_TYPE_FCT = factor(PDX18_TYPE,
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


pal <- leaflet::colorFactor(my_pal_hex,levels = levels(model_ready$PDX18_TYPE_FCT), ordered = TRUE,na.color = 'transparent')

vals <- levels(ordered(model_ready$PDX18_TYPE_FCT))

model_ready %>%
  sf::st_transform(4326) %>%
  dplyr::mutate(POPUP= paste0(GEOGRAPHY_NAME,": ",GEOGRAPHY_ID)) %>%
  leaflet::leaflet() %>%
  leaflet::addMapPane("background_map", zIndex = 410) %>%
  leaflet::addMapPane("polygons", zIndex = 420) %>%
  leaflet::addMapPane("lines", zIndex = 430) %>%
  leaflet::addMapPane("labels", zIndex = 440) %>%
  leaflet::addProviderTiles(provider = leaflet::providers$Stamen.TonerLite,
                            options = leaflet::pathOptions(pane = "background_map")) %>%
  leaflet::addPolygons(fillColor = ~pal(PDX18_TYPE_FCT), fillOpacity = 1, smoothFactor = 0,
                       color = 'white', opacity = .85, weight = .5,
                       popup = ~POPUP,
                       options = leaflet::pathOptions(pane = "polygons")) %>%
  leaflet::addPolygons(data = kc_boundary,
                       fillOpacity = 0, smoothFactor = 0,
                       color = 'black', opacity = .85, weight = 5,
                       options = leaflet::pathOptions(pane = "polygons")) %>%
    leaflet::addProviderTiles(provider = leaflet::providers$Stamen.TonerLines,
                            options = leaflet::pathOptions(pane = "lines")) %>%
  leaflet::addProviderTiles(provider = leaflet::providers$Stamen.TonerLabels,
                            options = leaflet::pathOptions(pane = "labels")) %>%
  leaflet::addLegend(title = '',position = 'topright',pal = pal, values = ~PDX18_TYPE_FCT, opacity = 1)

}

#' @rdname show-typology-map
#' @export
show_coo16_sf_map <- function(){

  # loadd the objects

  drake::loadd(model_all, kc_boundary)

   # assign one of the model objects to model_df

  model_colnames <- c("PDX18_TYPE",       # 1
                      "COO16_TYPE",       # 2
                      "COO16_TYPE",       # 3
                      "COOREV18_MF_TYPE", # 4
                      "COO16_TYPE") # 5

  kc_boundary <- sf::st_transform(kc_boundary, 4326)

  model_all <- sf::st_transform(model_all, 4326)

  model_ready <- model_all %>%
    dplyr::select(dplyr::starts_with("GEOGRAPHY"),
                  tidyselect::vars_select(names(.),model_colnames[2])) %>%
    dplyr::mutate(COO16_TYPE_FCT = factor(COO16_TYPE,
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


pal <- leaflet::colorFactor(my_pal_hex,levels = levels(model_ready$COO16_TYPE_FCT), ordered = TRUE,na.color = 'transparent')

vals <- levels(ordered(model_ready$COO16_TYPE_FCT))

model_ready %>%
  sf::st_transform(4326) %>%
  dplyr::mutate(POPUP= paste0(GEOGRAPHY_NAME,": ",GEOGRAPHY_ID)) %>%
  leaflet::leaflet() %>%
  leaflet::addMapPane("background_map", zIndex = 410) %>%
  leaflet::addMapPane("polygons", zIndex = 420) %>%
  leaflet::addMapPane("lines", zIndex = 430) %>%
  leaflet::addMapPane("labels", zIndex = 440) %>%
  leaflet::addProviderTiles(provider = leaflet::providers$Stamen.TonerLite,
                            options = leaflet::pathOptions(pane = "background_map")) %>%
  leaflet::addPolygons(fillColor = ~pal(COO16_TYPE_FCT), fillOpacity = 1, smoothFactor = 0,
                       color = 'white', opacity = .85, weight = .5,
                       popup = ~POPUP,
                       options = leaflet::pathOptions(pane = "polygons")) %>%
  leaflet::addPolygons(data = kc_boundary,
                       fillOpacity = 0, smoothFactor = 0,
                       color = 'black', opacity = .85, weight = 5,
                       options = leaflet::pathOptions(pane = "polygons")) %>%
    leaflet::addProviderTiles(provider = leaflet::providers$Stamen.TonerLines,
                            options = leaflet::pathOptions(pane = "lines")) %>%
  leaflet::addProviderTiles(provider = leaflet::providers$Stamen.TonerLabels,
                            options = leaflet::pathOptions(pane = "labels")) %>%
  leaflet::addLegend(title = '',position = 'topright',pal = pal, values = ~COO16_TYPE_FCT, opacity = 1)

}

#' @rdname show-typology-map
#' @export
show_coo18_sf_map <- function(){

  # loadd the objects

  drake::loadd(model_all, kc_boundary)

   # assign one of the model objects to model_df

  model_colnames <- c("PDX18_TYPE",       # 1
                      "COO16_TYPE",       # 2
                      "COO18_TYPE",       # 3
                      "COOREV18_MF_TYPE", # 4
                      "COO18_TYPE") # 5

  kc_boundary <- sf::st_transform(kc_boundary, 4326)

  model_all <- sf::st_transform(model_all, 4326)

  model_ready <- model_all %>%
    dplyr::select(dplyr::starts_with("GEOGRAPHY"),
                  tidyselect::vars_select(names(.),model_colnames[3])) %>%
    dplyr::mutate(COO18_TYPE_FCT = factor(COO18_TYPE,
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


pal <- leaflet::colorFactor(my_pal_hex,levels = levels(model_ready$COO18_TYPE_FCT), ordered = TRUE,na.color = 'transparent')

vals <- levels(ordered(model_ready$COO18_TYPE_FCT))

model_ready %>%
  sf::st_transform(4326) %>%
  dplyr::mutate(POPUP= paste0(GEOGRAPHY_NAME,": ",GEOGRAPHY_ID)) %>%
  leaflet::leaflet() %>%
  leaflet::addMapPane("background_map", zIndex = 410) %>%
  leaflet::addMapPane("polygons", zIndex = 420) %>%
  leaflet::addMapPane("lines", zIndex = 430) %>%
  leaflet::addMapPane("labels", zIndex = 440) %>%
  leaflet::addProviderTiles(provider = leaflet::providers$Stamen.TonerLite,
                            options = leaflet::pathOptions(pane = "background_map")) %>%
  leaflet::addPolygons(fillColor = ~pal(COO18_TYPE_FCT), fillOpacity = 1, smoothFactor = 0,
                       color = 'white', opacity = .85, weight = .5,
                       popup = ~POPUP,
                       options = leaflet::pathOptions(pane = "polygons")) %>%
  leaflet::addPolygons(data = kc_boundary,
                       fillOpacity = 0, smoothFactor = 0,
                       color = 'black', opacity = .85, weight = 5,
                       options = leaflet::pathOptions(pane = "polygons")) %>%
    leaflet::addProviderTiles(provider = leaflet::providers$Stamen.TonerLines,
                            options = leaflet::pathOptions(pane = "lines")) %>%
  leaflet::addProviderTiles(provider = leaflet::providers$Stamen.TonerLabels,
                            options = leaflet::pathOptions(pane = "labels")) %>%
  leaflet::addLegend(title = '',position = 'topright',pal = pal, values = ~COO18_TYPE_FCT, opacity = 1)

}

#' @rdname show-typology-map
#' @export
show_coorev18_mf_map <- function(){

  # loadd the objects

  drake::loadd(model_all, kc_boundary)

  # assign one of the model objects to model_df

  coo_communities <- model_all %>%
    dplyr::select(GEOGRAPHY_ID,
                  dplyr::matches("GEOGRAPHY_COMMUNITY")) %>%
    dplyr::filter(! is.na(GEOGRAPHY_COMMUNITY_ID)) %>%
    dplyr::group_by(GEOGRAPHY_COMMUNITY_NAME, GEOGRAPHY_COMMUNITY_ID) %>%
    dplyr::summarise() %>%
    sf::st_transform(4326)

  model_colnames <- c("PDX18_TYPE",       # 1
                      "COO16_TYPE",       # 2
                      "COO18_TYPE",       # 3
                      "COOREV18_MF_TYPE", # 4
                      "COOREV18_SF_TYPE") # 5

  kc_boundary <- sf::st_transform(kc_boundary, 4326)

  model_all <- sf::st_transform(model_all, 4326)

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

my_pal_hex <- c("#ffff00",
                "#ffc800",
                "#ff9600",
                "#a30382",
                "#0794d0",
                "#1b5f8f")


pal <- leaflet::colorFactor(my_pal_hex,levels = levels(model_ready$COOREV18_MF_TYPE_FCT), ordered = TRUE,na.color = 'transparent')

vals <- levels(ordered(model_ready$COOREV18_MF_TYPE_FCT))

model_ready %>%
  sf::st_transform(4326) %>%
  dplyr::mutate(POPUP= paste0(GEOGRAPHY_NAME,": ",GEOGRAPHY_ID)) %>%
  leaflet::leaflet() %>%
  leaflet::addMapPane("background_map", zIndex = 410) %>%
  leaflet::addMapPane("polygons", zIndex = 420) %>%
  leaflet::addMapPane("lines", zIndex = 430) %>%
  leaflet::addMapPane("labels", zIndex = 440) %>%
  leaflet::addMapPane("communities", zIndex = 450) %>%
  leaflet::addProviderTiles(provider = leaflet::providers$Stamen.TonerLite,
                            options = leaflet::pathOptions(pane = "background_map")) %>%
  leaflet::addPolygons(fillColor = ~pal(COOREV18_MF_TYPE_FCT), fillOpacity = 1, smoothFactor = 0,
                       color = 'white', opacity = .85, weight = .5,
                       popup = ~POPUP,
                       options = leaflet::pathOptions(pane = "polygons")) %>%
  leaflet::addPolygons(data = kc_boundary,
                       fillOpacity = 0, smoothFactor = 0,
                       color = 'black', opacity = .85, weight = 5,
                       options = leaflet::pathOptions(pane = "polygons")) %>%
    leaflet::addProviderTiles(provider = leaflet::providers$Stamen.TonerLines,
                            options = leaflet::pathOptions(pane = "lines")) %>%
  leaflet::addProviderTiles(provider = leaflet::providers$Stamen.TonerLabels,
                            options = leaflet::pathOptions(pane = "labels")) %>%
    leaflet::addPolygons(data = coo_communities,
                       fillOpacity = 0, smoothFactor = 0,
                       color = 'black', opacity = .85, weight = 2.5,
                       options = leaflet::pathOptions(pane = "communities"))

}

#' @rdname show-typology-map
#' @export
show_coorev18_sf_map <- function(){

  # loadd the objects

  drake::loadd(model_all, kc_boundary)

  coo_communities <- model_all %>%
    dplyr::select(GEOGRAPHY_ID,
                  dplyr::matches("GEOGRAPHY_COMMUNITY")) %>%
    dplyr::filter(! is.na(GEOGRAPHY_COMMUNITY_ID)) %>%
    dplyr::group_by(GEOGRAPHY_COMMUNITY_NAME, GEOGRAPHY_COMMUNITY_ID) %>%
    dplyr::summarise() %>%
    sf::st_transform(4326)

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


my_pal_hex <- c("#ffff00",
                "#ffc800",
                "#ff9600",
                "#a30382",
                "#0794d0",
                "#1b5f8f")
# my_pal_hex <- c("#f5eb12",
#                 "#fcc811",
#                 "#f89621",
#                 "#9e2382",
#                 "#0d94ce",
#                 "#1c6091")


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
  leaflet::addMapPane("communities", zIndex = 450) %>%
  leaflet::addProviderTiles(provider = leaflet::providers$Stamen.TonerLite,
                            options = leaflet::pathOptions(pane = "background_map")) %>%
  leaflet::addPolygons(fillColor = ~pal(COOREV18_SF_TYPE_FCT), fillOpacity = 1, smoothFactor = 0,
                       color = 'white', opacity = .85, weight = .5,
                       popup = ~POPUP,
                       options = leaflet::pathOptions(pane = "polygons")) %>%
  leaflet::addPolygons(data = kc_boundary,
                       fillOpacity = 0, smoothFactor = 0,
                       color = 'black', opacity = .85, weight = 5,
                       options = leaflet::pathOptions(pane = "polygons")) %>%
    leaflet::addProviderTiles(provider = leaflet::providers$Stamen.TonerLines,
                            options = leaflet::pathOptions(pane = "lines")) %>%
  leaflet::addProviderTiles(provider = leaflet::providers$Stamen.TonerLabels,
                            options = leaflet::pathOptions(pane = "labels")) %>%
    leaflet::addPolygons(data = coo_communities,
                       fillOpacity = 0, smoothFactor = 0,
                       color = 'black', opacity = .85, weight = 5,
                       options = leaflet::pathOptions(pane = "communities"))
  # leaflet::addLegend(title = '',position = 'topright',pal = pal, values = ~COOREV18_SF_TYPE_FCT, opacity = 1)

}


