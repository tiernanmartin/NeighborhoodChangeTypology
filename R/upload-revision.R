#' Upload Revision to OSF
#'
#' @param id Character, OSF id (osf.io/xxxx; just xxxx).
#' @param path Character, Path to file on local machine to upload.
#' @param ... Other arguments.
#'
#' @return the url of the revised file
upload_revision <- function (id, path,...)
{
    if (!file.exists(path)) {
        stop(sprintf("File %s does not exist on local machine.",
            path))
    }
    config <- osfr:::get_config(TRUE)
    typ <- osfr:::process_type(id, private = TRUE)
    if (typ == "nodes") {
        stop("Specify an OSF id referring to a file.")
    }
    else if (typ == "files") {
        url_osf <- osfr:::construct_link(paste(typ, id, sep = "/"))
        call <- httr::GET(url_osf, config)
        res <- osfr:::process_json(call)
        upload_osf <- res$data$links$upload
    }
    else {
        stop("Unknown error occurred. Please file issue on GitHub.")
    }
    call <- httr::PUT(upload_osf, body = httr::upload_file(path),
        encode = "raw", config = config)
    if (call$status_code != 200) {
        stop("Failed to upload revision")
    }

    res <- osfr:::process_json(call)
    return(res$data$links$download)
}
