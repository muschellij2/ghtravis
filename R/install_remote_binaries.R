#' @title Install Binaries from Remotes
#' @description Gets Remotes, gets Binaries form GH, and installs them, then
#' drops them from Remotes field
#'
#' @param path Path to DESCRIPTION file
#' passed to \code{\link{remote_binaries}}
#' @param remotes Remotes to get binaries for - in case not going from DESCRIPTION,
#' passed to \code{\link{remote_binaries}}
#' @param package Specific package to install
#' @param verbose Print diagnostic messages
#' @param method method to download, passed to \code{\link{download.file}}
#' @param check_r_version Check if R version is in the name of the
#' tarball
#' @param force_sha Should the SHA be checked when trying to get
#' binaries
#' @param ... additional arguments passed to \code{\link{install.packages}}
#'
#'
#' @return Invisible NULL
#' @export
#'
#' @examples
#' \dontrun{
#' path = example_description_file()
#' install_remote_binaries(path = path, method = "wget")
#' }
#' @importFrom utils install.packages installed.packages remove.packages
# @param update_only Should only packages needing update be installed?
# @param drop_all If TRUE, then will only drop packages that are installed
install_remote_binaries = function(
  path = "DESCRIPTION",
  remotes = NULL,
  package = NULL,
  check_r_version = TRUE,
  force_sha = TRUE,
  # update_only = FALSE,
  verbose = TRUE,
  method = ifelse(.Platform$OS.type == "unix", "wget", "auto"),
  ...
) {

  urls = remote_binaries(path = path, remotes = remotes,
                         verbose = verbose,
                         check_r_version = check_r_version,
                         force_sha = force_sha)
  if (!is.null(package)) {
    if (all(package != "")) {
      urls = urls[intersect(package, names(urls))]
    }
  }

  #urls are the only ones with binaries

  if (length(urls) > 0) {
    packs = names(urls)
    urls = unlist(urls)
    ## if only update them

    # if (update_only) {
    #   updaters = update_these_packages(path = path)
    #   packs = intersect(packs, updaters)
    #   urls = urls[packs]
    # }

    if (length(urls) > 0) {

      for (ipack in seq_along(urls)) {
        pack = packs[ipack]
        url = urls[ipack]
        if (verbose) {
          message(paste0("Installing ", pack,
                         " package binaries"))
        }

        destfile = file.path(tempdir(), basename(url))

        download.file(
          url, destfile,
          method = method,
          quiet = !verbose)

        install.packages(
          destfile,
          repos = NULL,
          type = .Platform$pkgType,
          ...)
      }
    }
    # if (!drop_all) {
    # ip = installed.packages()
    # need to check versions!
    if (is.null(remotes)) {

      keep_packs = update_these_packages(path = path)
      # utils::remove.packages(keep_packs)
      keep = (packs %in% keep_packs)
      if (any(keep)) {
        warning(
          paste0(
            "Packages: ", paste(packs[keep], collapse = ","),
            " have installed binary, but version not correct!")
        )
      }
      packs = packs[ !keep ]
      # }

      if (file.exists(path)) {
        drop_remotes(path = path, drop_remotes = packs)
      }
    } else {
      if (file.exists(path)) {
        drop_remotes(path = path, drop_remotes = remotes)
      }
    }
  } else {
    if (verbose) {
      message("No Binaries Found, exiting")
    }
  }
  return(invisible(NULL))
}
