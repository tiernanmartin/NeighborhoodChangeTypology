#' @title Export Geographies As Files
#' @description Thes functions write several spatial objects.
#' @param kc_boundary desc
#' @param white_center_place desc
#' @param model_all desc
#' @param path desc
#' @return NULL

#' @rdname export-geographies
export_kc_boundary_gpkg <- function(kc_boundary, path){

  kc_boundary %>%
    sf::st_transform(4326) %>%
    sf::write_sf(path , driver = "GPKG",delete_dsn = TRUE,  layer_options = c("OVERWRITE=yes"))

  return(invisible(NULL))

}

#' @rdname export-geographies
export_white_center_place_gpkg <- function(white_center_place, path){

  white_center_place %>%
    sf::st_transform(4326) %>%
    sf::write_sf(path , driver = "GPKG",delete_dsn = TRUE,  layer_options = c("OVERWRITE=yes"))

  return(invisible(NULL))

}

#' @rdname export-geographies
export_coo_communities_gpkg <- function(model_all, path){

  coo_communities <- model_all %>%
    dplyr::select(GEOGRAPHY_ID,
                  dplyr::matches("GEOGRAPHY_COMMUNITY")) %>%
    dplyr::filter(! is.na(GEOGRAPHY_COMMUNITY_ID)) %>%
    dplyr::group_by(GEOGRAPHY_COMMUNITY_NAME, GEOGRAPHY_COMMUNITY_ID) %>%
    dplyr::summarise() %>%
    sf::st_transform(4326)

  coo_communities %>%
    sf::write_sf(path , driver = "GPKG",delete_dsn = TRUE,  layer_options = c("OVERWRITE=yes"))

  return(invisible(NULL))

}

