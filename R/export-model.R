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
export_model_all_gpkg <- function(model_all, path){


 sf::write_sf(model_all, path , driver = "GPKG")

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
export_model_typologies_gpkg <- function(model_all, path){

  model_colnames <- c("PDX18_TYPE",       # 1
                      "COO16_TYPE",       # 2
                      "COO18_TYPE",       # 3
                      "COOREV18_MF_TYPE", # 4
                      "COOREV18_SF_TYPE") # 5


  model_ready <- model_all %>%
    dplyr::select(dplyr::starts_with("GEOGRAPHY"),
                  tidyselect::vars_select(names(.),model_colnames))


 sf::write_sf(model_ready, path, driver = "GPKG")

}


