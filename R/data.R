
# KC BOUNDARY -------------------------------------------------------------

#' @title The Boundary of King County, WA
#' @description A polygon of the boundary of King County, WA.
#' @docType data
#' @format The data is a single record simple feature (class = `sf`) with
#'   one variable and a `MULTIPOLYGON` geometry column.
#'
#'   The object includes the following variables:
#' \describe{
#'
#'   \item{`• NAME`:}{The name of the county}
#'   \item{`• geometry`:}{The simple feature column (geometry type is `MULTIPOLYGON`)}
#'
#'   }
#'
#' @seealso \code{\link{get_external_data_plan}}
"kc_boundary"



# EXTERNAL DATA LIST ------------------------------------------------------

#' @title External Data List
#' @description This is a list of datasets that are included in the package workflow but
#'   are not stored in the \code{data/} directory.
#'
#'   The primary reason that a dataset is included in this list is that it exceeds Github's
#'   50 MB file size limit (see \href{https://help.github.com/articles/conditions-for-large-files/}{here} for details).
#'
#' @docType data
#' @format The data itself is simply a \code{NULL} value.
#'
#'  Its purpose is to provide some documentation of the datasets that are not directly
#'  included in the package.
#'
#'  The following external datasets are accessed by workflows in this package:
#'
#' \describe{
#'
#'   \item{`• waterbodies`:}{Multipolygons of the major waterbodies in King County, WA (\href{https://osf.io/9fp6q/}{osf.io/9fp6q})}
#'   \item{`• census_tracts_2009`:}{King County census tract boundaries (2009) (\href{https://osf.io/5gx8a/}{osf.io/5gx8a}).\cr
#'     Original source: \href{ftp://ftp.census.gov/geo/tiger/TIGER2009/53_WASHINGTON/53033_King_County/tl_2009_53033_tract00.zip}{ftp.census.gov} }
#'   \item{`• census_tracts_2016`:}{King County census tract boundaries (2009) \cr
#'     Downloaded with the \code{\link[tigris]{tigris}} package. }
#'
#'   }
#'
#' @seealso \code{\link{get_external_data_plan}}
"exteral_datasets_list"
