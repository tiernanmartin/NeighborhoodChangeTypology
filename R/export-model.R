#' @title Export Model As Files
#' @description Thes functions write the model object as csv files.
#' @param model_all desc
#' @param path desc
#' @return NULL

#' @rdname export-model
#' @export
export_model_all_csv <- function(model_all, path){

  model_ready <- model_all %>%
     sf::st_drop_geometry() %>%
    tibble::as_tibble()

 readr::write_csv(model_ready, path)

}

#' @rdname export-model
#' @export
export_model_all_rds <- function(model_all, path){

  model_ready <- model_all %>%
     sf::st_drop_geometry() %>%
    tibble::as_tibble()

 readr::write_rds(model_ready, path,compress = "gz")

}

#' @rdname export-model
export_model_all_gpkg <- function(model_all, path){

  stop("This function returns an error when it is run.\nSee the question posted on Stackoverflow: <https://gis.stackexchange.com/q/314500/49120>")

 sf::write_sf(model_all, path , driver = "GPKG",delete_dsn = TRUE,  layer_options = c("OVERWRITE=yes"))

}

#' @rdname export-model
#' @export
export_model_pdx18_gpkg <- function(model_all, path){

  model_all %>%
    dplyr::select(dplyr::matches("GEOGRAPHY"),
                  dplyr::matches("PDX18")) %>%
     sf::st_transform(4326) %>%
    sf::write_sf(path , driver = "GPKG",delete_dsn = TRUE,  layer_options = c("OVERWRITE=yes"))

}

#' @rdname export-model
#' @export
export_model_coo16_gpkg <- function(model_all, path){

  model_all %>%
    dplyr::select(dplyr::matches("GEOGRAPHY"),
                  dplyr::matches("COO16")) %>%
     sf::st_transform(4326) %>%
    sf::write_sf(path , driver = "GPKG",delete_dsn = TRUE,  layer_options = c("OVERWRITE=yes"))

}

#' @rdname export-model
#' @export
export_model_coo18_gpkg <- function(model_all, path){

  model_all %>%
    dplyr::select(dplyr::matches("GEOGRAPHY"),
                  dplyr::matches("COO18")) %>%
     sf::st_transform(4326) %>%
    sf::write_sf(path , driver = "GPKG",delete_dsn = TRUE,  layer_options = c("OVERWRITE=yes"))

}

#' @rdname export-model
#' @export
export_model_coorev18_gpkg <- function(model_all, path){

  model_all %>%
    dplyr::select(dplyr::matches("GEOGRAPHY"),
                  dplyr::matches("COOREV18")) %>%
     sf::st_transform(4326) %>%
    sf::write_sf(path , driver = "GPKG",delete_dsn = TRUE,  layer_options = c("OVERWRITE=yes"))

}


#' @rdname export-model
#' @export
export_model_typologies_csv <- function(model_all, path){

  model_colnames <- c("PDX18_TYPE",       # 1
                      "COO16_TYPE",       # 2
                      "COO18_TYPE",       # 3
                      "COOREV18_MF_TYPE", # 4
                      "COOREV18_SF_TYPE") # 5


  model_ready <- model_all %>%
    dplyr::select(dplyr::starts_with("GEOGRAPHY"),
                  tidyselect::vars_select(names(.),model_colnames)) %>%
     sf::st_drop_geometry() %>%
    tibble::as_tibble()

 readr::write_csv(model_ready, path)

}

#' @rdname export-model
#' @export
export_model_typologies_rds <- function(model_all, path){

  model_colnames <- c("PDX18_TYPE",       # 1
                      "COO16_TYPE",       # 2
                      "COO18_TYPE",       # 3
                      "COOREV18_MF_TYPE", # 4
                      "COOREV18_SF_TYPE") # 5


  model_ready <- model_all %>%
    dplyr::select(dplyr::starts_with("GEOGRAPHY"),
                  tidyselect::vars_select(names(.),model_colnames)) %>%
     sf::st_drop_geometry() %>%
    tibble::as_tibble()

 readr::write_rds(model_ready, path,compress = "gz")

}

#' @rdname export-model
#' @export
export_model_typologies_gpkg <- function(model_all, path){

  model_colnames <- c("PDX18_TYPE",       # 1
                      "COO16_TYPE",       # 2
                      "COO18_TYPE",       # 3
                      "COOREV18_MF_TYPE", # 4
                      "COOREV18_SF_TYPE") # 5


  model_ready <- model_all %>%
    dplyr::select(dplyr::starts_with("GEOGRAPHY"),
                  tidyselect::vars_select(names(.),model_colnames)) %>%
     sf::st_transform(4326)


 sf::write_sf(model_ready, path, driver = "GPKG")

}


