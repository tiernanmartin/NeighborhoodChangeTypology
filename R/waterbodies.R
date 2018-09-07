#' @title Make King County's Major Waterbodies
#' @description Download the polygons for the major waterbodies in Washington State from osf.io
#'   and filter the data to include those waterbodies within King County.
#' @param kc_boundary Polygon, the boundary of King County, WA
#' @return a MULTIPOLYGON simple feature (class =  `sf`)

#' @rdname waterbodies
#' @export
prepare_waterbodies <- function(path){

# GET DATA ----------------------------------------------------------------

  wtr_url <- "https://fortress.wa.gov/ecy/gispublic/DataDownload/ECY_WAT_NHDWAMajor.zip"

  download.file(wtr_url, path, quiet = TRUE)


# RETURN ------------------------------------------------------------------

  waterbodies_prep_status <- NeighborhoodChangeTypology::get_modified_time(path)

  return(waterbodies_prep_status)

}

#' @rdname waterbodies
#' @export
make_waterbodies <- function(path){

  unzip(path, exdir = "extdata/osf")

  wtr_raw <- suppressWarnings(
    rgdal::readOGR(dsn = "extdata/osf/NHDMajor.gdb",
                            layer = "NHD_MajorWaterbodies",
                            verbose = FALSE,
                            pointDropZ = TRUE)
  )

  wtr_sf <- wtr_raw %>%
    sf::st_as_sf() %>%
    sf::st_transform(2926) %>%
    sf::st_buffer(dist = 0) %>%
    sf::st_cast("MULTIPOLYGON")

  wtr_kc_sf <- wtr_sf %>%
    NeighborhoodChangeTypology:::st_intersect_filter(kc_boundary) %>%
    dplyr::transmute(NAME = as.character(GNIS_Name))

  waterbodies <- wtr_kc_sf

  return(waterbodies)

}
