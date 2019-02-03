#' @title Get Binaries from Remotes
#' @description Gets Remotes, gets Binaries form GH
#'
#' @param remotes Remotes to get the binaries for
#' @param path Path to DESCRIPTION field
#' @param package packages to subset if necessary
#' @param verbose print diagnostic messages
#' @param ... additional arguments passed to
#' \code{\link{latest_release_with_binary}}
#'
#' @return List of remote binaries
#' @export
#'
#'
#' @examples
#' remote_binaries(remotes = "muschellij2/neurobase")
#' path = example_description_file()
#' if (file.exists(path)) {
#'    remote_binaries(path = path)
#' }
remote_binaries = function(
  remotes = NULL,
  path = "DESCRIPTION",
  package = NULL,
  verbose = TRUE,
  ...
) {

  if (is.null(remotes)) {
    remotes = get_remotes(path)
  }
  remotes = sub(";$", "", remotes)
  remotes = remotes[ !trimws(remotes) %in% c("", ";") ]
  packs = parse_remotes(remotes)
  packs = sapply(packs, `[[`, "repo")
  urls = lapply(remotes, function(x) {
    if (verbose) {
      print(x)
    }
    latest_release_with_binary(x, verbose = verbose,
                               ...)
    })
  names(urls) = packs
  no_urls = !sapply(urls, is.na)
  urls = urls[no_urls]

  if (!is.null(package)) {
    keep = package %in% names(urls)
    package = package[keep]
    if (any(keep)) {
      urls = urls[package]
      return(urls)
    } else {
      urls = list()
    }
  }
  return(urls)
}
