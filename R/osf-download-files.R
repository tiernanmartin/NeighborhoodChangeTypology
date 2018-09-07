#' @title Download Files From OSF
#' @description Download files from the OSF.
#' @param id Character, OSF id (osf.io/xxxx; just xxxx).
#' @param path Character, Path to file on local machine to upload.
#' @param private Logcial, Whether the file is private.
#' @param version Character, The OSF version id.
#' @return the filepath of the downloaded file
#' @export
osf_download_files <- function (id, path = NULL, private = FALSE, version = NULL)
{
    config <- osfr:::get_config(private)
    url_osf <- osfr:::construct_link(paste0("guids/", id))
    call <- httr::GET(url_osf, config)
    if (!call$status_code == 200) {
        stop("Failed. Are you sure you have access to the file?")
    }
    res <- osfr:::process_json(call)
    if (is.null(path)) {
        txt <- res$data$attributes$name
        start <- utils::tail(gregexpr("/", txt)[[1]], 1)
        end <- nchar(txt)
        file <- substr(txt, start + 1, end)
    }
    else {
        file <- file.path(dirname(path), res$data$attributes$name)
    }
    message(paste0("Saving to filename: ", file))
    if (is.null(version)) {
        call <- httr::GET(res$data$links$download, config, httr::write_disk(file,
            overwrite = TRUE))
    }
    else {
        call <- httr::GET(paste0(res$data$links$download, "?revision=",
            version), config, httr::write_disk(file, overwrite = TRUE))
    }
    if (call$status_code == 404) {
        stop("Version of file does not exist.")
    }
    else if (call$status_code != 200) {
        stop("Failed to download file.")
    }
    message("Successfully downloaded file.")
    return(file)
}
