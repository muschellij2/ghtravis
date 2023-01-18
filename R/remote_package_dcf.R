#' Read Remote Package DESCRIPTION file
#'
#' @param remotes Repository name
#' @param pat GitHub Personal Authentication Token (PAT)
#' @param url URL for repo - github
#' @param ... additional arguments to pass to \code{\link{GET}}
#' @param output_directory Output directory for the description file to download to,
#' @param overwrite overwrite the file if exists.  Only really relevant if
#' \code{output_directory} is not \code{NULL}
#'
#' @return List of Package/Version DCF information
#' @export
#'
#' @examples
#' get_remote_package_dcf("stnava/ANTsRCore")
#' out = get_remote_package_dcf("stnava/ANTsRCore", output_directory = tempdir())
#' out = get_remote_package_dcf("stnava/ANTsRCore", output_directory = tempdir())
#' remote_package_dcf("stnava/ANTsRCore")
#' remote_package_dcf("stnava/ANTsRCore", output_directory = tempdir())
get_remote_package_dcf = function(
  remotes,
  pat = NULL,
  url = c("https://github.com", "https://api.github.com"),
  output_directory = NULL,
  overwrite = FALSE,
  ...) {
  if (is.null(pat)) {
    pat = github_pat(quiet = TRUE)
  }

  url = url[1]

  if (length(remotes) > 1) {
    res = vapply(remotes, get_remote_package_dcf, FUN.VALUE = NA_character_,
                 pat = pat, url = url, ...)
    names(res) = remotes
    res = vapply(res, unname, FUN.VALUE = NA_character_)
    return(res)
  }
  remote = parse_one_remote(remotes)

  if (is.null(output_directory)) {
    tmp <- tempfile()
  } else {
    dir.create(output_directory, showWarnings = FALSE, recursive = TRUE)
    xxfile = paste(
      remote$username, remote$repo,
      remote$ref, remote$subdir, "DESCRIPTION", sep = "_")
    tmp = file.path(output_directory, xxfile)
  }
  if (!file.exists(tmp) || overwrite) {
    api_call = grepl("api", tolower(url))
    if (api_call) {
      if (!requireNamespace("base64enc", quietly = TRUE)) {
        stop("base64enc needed for API calls")
      }
    }
    if (!api_call) {
      path <- paste(c(
        remote$username,
        remote$repo, "raw", remote$ref,
        remote$subdir, "DESCRIPTION"),
        collapse = "/")
      ctype = httr::content_type("text/plain")

    } else {
      path <- paste(c(
        "repos",
        remote$username,
        remote$repo, "contents",
        remote$subdir, "DESCRIPTION"),
        collapse = "/")
      path = paste0(path, "?ref=", remote$ref)
      ctype = httr::content_type_json()
    }
    req <- httr::GET(url, path = path,
                     if (api_call) github_auth(pat),
                     httr::write_disk(path = tmp),
                     ctype,
                     ...)
    if (httr::status_code(req) >= 400) {
      if (api_call) {
        tmp = get_remote_package_dcf(remotes,
                                     pat = pat, url = "https://github.com")
      } else {
        tmp = NA_character_
      }
    } else {
      if (api_call) {
        data = httr::content(req)$content
        data = base64enc::base64decode(what = data)
        data = rawToChar(data)
        writeLines(data, tmp)
      }
    }
  }
  if (is.na(tmp)) {
    warning("Request to GitHub API did not result in a DCF file download")
  }
  names(tmp) = remotes
  return(tmp)
}

#' @export
#' @rdname get_remote_package_dcf
remote_package_dcf = function(...) {
  dcfs = get_remote_package_dcf(...)
  get_pack = function(tmp) {
    if (is.na(tmp)) {
      L = list(Package = NA,
               Version = NA)
    } else {
      L = read_dcf(tmp)$dcf
    }
    return(L)
  }
  if (length(dcfs) > 1) {
    L = lapply(dcfs, get_pack)
  } else {
    L = get_pack(dcfs)
  }

  return(L)
}


#' @export
#' @rdname get_remote_package_dcf
has_remote_dcf = function(
  remotes,
  pat = NULL,
  url = "https://github.com",
  ...) {
  if (is.null(pat)) {
    pat = github_pat(quiet = TRUE)
  }
  if (length(remotes) > 1) {
    res = vapply(remotes, has_remote_dcf, FUN.VALUE = logical(1),
                 pat = pat, url = url, ...)
    names(res) = remotes
    return(res)
  }
  remote = ghtravis::parse_one_remote(remotes)

  path <- paste(c(remote$username,
                  remote$repo, "raw", remote$ref,
                  remote$subdir, "DESCRIPTION"),
                collapse = "/")
  req <- httr::HEAD(url, path = path,
                    github_auth(pat),
                    httr::content_type("text/plain"),
                    ...)
  code = httr::status_code(req)
  code == 200
}