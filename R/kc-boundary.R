#' @title Make the Boundary of King County, WA
#' @description Get the polygon of King County, WA using the \code{\link[tigris]{tigris}} package.
#' @return a MULTIPOLYGON simple feature (class =  `sf`)

#' @rdname kc-boundary
#' @export
prepare_kc_boundary <- function(path){

  options(tigris_class = "sf")


# GET DATA ----------------------------------------------------------------

  counties <- tigris::counties(state = "53")

  kc <- counties %>%
    dplyr::select(NAME) %>%
    dplyr::filter(NAME %in% "King") %>%
    sf::st_transform(2926)

  kc_boundary_prep <- kc

# WRITE DATA --------------------------------------------------------------

  sf::st_write(kc_boundary_prep, path, driver = "GPKG", quiet = TRUE, layer_options = "OVERWRITE=true")


# RETURN ------------------------------------------------------------------

  kc_boundary_prep_status <- NeighborhoodChangeTypology::get_modified_time(path)

  return(kc_boundary_prep_status)

}

#' @rdname kc-boundary
#' @export
make_kc_boundary <- function(path){

  kc_boundary <- suppressWarnings(suppressMessages(sf::st_read(path,quiet = TRUE)))

  return(kc_boundary)

}
