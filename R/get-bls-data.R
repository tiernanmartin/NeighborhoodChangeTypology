#' @title Get BLS Data
#' @description Allows users to request data for one or multiple series through the U.S. Bureau of Labor Statistics API.
#'   Users provide parameters as specified in <https://www.bls.gov/developers/api_signature.htm>
#'   and the function returns a JSON string or data frame.
#' @param ... Arguments to be passed on to \code{blsAPI::\link[blsAPI]{blsAPI}}
#' @param bls_api_key Character, the API key provided to you from the Census formated in quotes
#' @seealso \code{\link[blsAPI]{blsAPI}}
#' @return a JSON string or a data.frame
#' @importFrom blsAPI blsAPI
#' @export
get_bls_data <- function(..., bls_api_key = ""){

  if (Sys.getenv("BLS_API_KEY") != "") {

    bls_api_key <- Sys.getenv("BLS_API_KEY")

  } else if (is.null(bls_api_key)) {

    stop('A BLS API key is required.  Obtain one at https://data.bls.gov/registrationEngine/, and then supply the key to the `bls_api_key` function to use it throughout the session.')

  }

  arg_list <- list(...)

  arg_list$payload <- append(arg_list$payload, c("registrationkey" = bls_api_key))

  response <- do.call(what = blsAPI::blsAPI, args = arg_list)

  return(response)


}
