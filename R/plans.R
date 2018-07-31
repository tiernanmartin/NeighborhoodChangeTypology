
# EXTERNAL DATA -----------------------------------------------------------

#' @title Drake Plan: External Data
#' @description A \code{\link[drake]{drake_plan}} that loads several external datasets.
#' @docType data
#' @format A \code{\link[drake]{drake_plan}} object of class \code{tbl_df} that builds
#'   the following \code{\link[drake]{target}}s:
#'   \describe{
#'
#'   \item{`• kc_boundary`:}{A polygon of King County, WA}
#'
#'   }
#'
#' @seealso \code{\link{exteral_datasets_list}}
#' @keywords drake, plan
#' @examples
#'
#' # Print the plan
#'
#' print(plan_external_data)
#'
#'
#' # Make the plan, load a target, print the target
#'
#' \dontrun{
#'
#' make(plan_external_data)
#'
#' readd(kc_boundary)
#' }
"plan_external_data"
