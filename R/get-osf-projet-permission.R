
#' @title Check Whether The User Has Access To An OSF Project
#' @description Check whether the user has access to a specific OSF project.
#' @return a logical
#' @param project_title Character, the extact title of the OSF project
#' @export
check_osf_access <- function(project_title = NULL){

   if (Sys.getenv("OSF_PAT") == "") {
            stop("Please login first using the osfr::login() function")
        }

  osf_api_call <- osfr::get_users(id = "me", nodes = TRUE)

  user_nodes_df <- data.frame(PROJECTS = purrr::map_chr(osf_api_call$data, ~ .x$attributes$title))

  has_access <- any(user_nodes_df$PROJECTS %in% project_title)



  return(has_access)
}
