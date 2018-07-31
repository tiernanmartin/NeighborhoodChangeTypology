
# EXTERNAL DATA -----------------------------------------------------------

#' @title Drake Plan: External Data
#' @description A \code{\link[drake]{drake_plan}} that loads several external datasets.
#' @docType data
#' @format A \code{\link[drake]{drake_plan}} object of class \code{tbl_df} that builds
#'   the following \code{\link[drake]{target}}s:
#'   \describe{
#'
#'   \item{`• item_one`:}{Description}
#'
#'   }
#'
#' @seealso \code{\link{exteral_datasets_list}}
#' @keywords drake, plan
#' @examples
#'
#' # Print the plan
#'
#' print("change this")
#'
#'
#' # Make the plan, load a target, print the target
#'
#' \dontrun{
#'
#' make(item_one)
#'
#' readd(item_one)
#' }
"plan_external_data"
