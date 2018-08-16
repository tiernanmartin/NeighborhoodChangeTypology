
#' @title Get OSF File Version Number
#' @description Get the version number of a file that is stored in a OSF project.
#' @return a double
#' @param id Character, OSF id (osf.io/XXXX) for the node (project or component)
#'   to get the file info for
#' @param filename Character, the extact name of the specifc file
#' @importFrom osfr get_files_info
#' @export
get_osf_version <- function(id = NULL, filename = NULL){


  files_info <- osfr:::get_files_info(id, private = FALSE)

  version <- files_info[files_info$name %in% filename,"version"][[1]]

  return(version)
}
