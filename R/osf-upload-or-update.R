#' @title Upload/Update A File On OSF
#' @param has_osf_access Logical, Whether the user has access to the project's OSF repository.
#' @param project_id Character, The id of the OSF project.
#' @param file_id Character, The id of the existing OSF file (if any).
#' @param path Character, The file path.
#' @param dest Character, The name of the destination file on OSF (if NULL, basename(path) will be used).
#' @return a logical
#' @export
osf_upload_or_update <- function(has_osf_access = FALSE, project_id = NULL, file_id = NULL, path, dest = NULL ){

  # Check that the user has access to the OSF project (i.e., they can upload files to it)

  if(!has_osf_access){stop("The user does not have access to the project's OSF files.")}

  project_id_valid <- suppressWarnings(!inherits(try(osfr::search_nodes(id = project_id)), "try-error"))

  # Check that project_id isn't NULL
  if(is.null(project_id)){
    stop("Please provide an OSF id to either `project_id`.")}

  # Check that the project_id exists
  if(!project_id_valid){
    stop("The id provided to `project_id` does not correspond to an OSF project.")}
  # Scenario One: new upload


  if(is.null(file_id)){

    uploaded_file_url <- osfr::upload_files(id = project_id, path = path, dest = dest)

    project_files <- osfr:::get_files_info(project_id, private = FALSE)

    filename <- project_files[which(uploaded_file_url %in% project_files$href), "name"]

    time_file_created <- project_files[which(uploaded_file_url %in% project_files$href), "created_utc"]

    message <- paste0(filename," created at: ", lubridate::as_datetime(time_file_created))

    return(message)
  }


  # Scenario Two: upload revision

  if(!is.null(file_id)){

    uploaded_file_url <- NeighborhoodChangeTypology:::upload_revision(id = file_id, path = path)

    project_files <- osfr:::get_files_info(project_id, private = FALSE)

    filename <- project_files[which(uploaded_file_url %in% project_files$href), "name"]

    time_file_modified <- project_files[which(uploaded_file_url %in% project_files$href), "modified_utc"]

    message <- paste0(filename," modified at: ", lubridate::as_datetime(time_file_modified))

    return(message)

  }

  stop("An error has occurred.")

}
