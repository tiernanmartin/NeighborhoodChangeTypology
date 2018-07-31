#' @title Make King County's Major Waterbodies
#' @description Download the polygons for the major waterbodies in Washington State from osf.io
#'   and filter the data to include those waterbodies within King County.
#' @param kc_boundary Polygon, the boundary of King County, WA
#' @return a MULTIPOLYGON simple feature (class =  `sf`)
#' @importFrom rgdal readOGR
#' @importFrom sf st_as_sf
#' @importFrom sf st_transform
#' @importFrom sf st_buffer
#' @importFrom sf st_cast
#' @importFrom dplyr transmute
#' @importFrom utils unzip
#' @export
make_waterbodies <- function(kc_boundary){

  # Original source: ftp://www.ecy.wa.gov/gis_a/inlandWaters/NHD/NHDmajor.gdb.zip

  data_osfr_id <- "9fp6q"

  data_fp <- "extdata/"

  data_fp_full <- osfr::download_files(id = data_osfr_id, path = data_fp)

  unzip(data_fp_full, exdir = "extdata")

  wtr_raw <- suppressWarnings(
    rgdal::readOGR(dsn = "extdata/NHDMajor.gdb",
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
    st_intersect_filter(kc_boundary) %>%
    dplyr::transmute(NAME = as.character(GNIS_Name))

  waterbodies <- wtr_kc_sf

}
