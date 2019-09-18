#' @title Returns the specific package from Remotes
#' @description Subsets the remotes based on a package
#'
#' @param path Path to DESCRIPTION file
#' @param package Package to subset.  If NULL, then all will be installed
#' @param reorder should remotes be reordered before running
#' @param drop Remotes should be dropped after installing
#' @param verbose Print diagnostic message
#' @param mustInstall should the command run even if install_github fails?
#' @param ... arguments to pass to install_github
#' @return Character vector of remotes
#'
#' @return Output of \code{\link{install_github}}, indicator
#' of TRUE/FALSE for installation
#' @export
#'
#' @examples \dontrun{
#'   install_remotes_no_dep()
#' }
#' @importFrom remotes install_github
install_remotes_no_dep = function(
  path = "DESCRIPTION",
  package = NULL,
  reorder = TRUE,
  drop = TRUE,
  verbose = TRUE,
  mustInstall = FALSE,
  ...) {
  if (!is.null(package)) {
    if (all(package == "")) {
      package = NULL
    }
  }
  if (reorder) {
    message("Reordering Remotes")
    reorder_remotes(path = path, verbose = verbose)
  }
  remotes = subset_remote(path = path, package = package)
  if (length(remotes) == 0) {
    return(NULL)
  }
  if (all(remotes == "")) {
    return(NULL)
  }
  remotes = remotes[ !(remotes %in% "") ]


  res = sapply(remotes, function(x) {
    if (verbose) {
      message(paste0("Installing Remote (dep = FALSE): ", x))
    }
    # r = try({
    #   devtools::install_github(
    #   repo = x,
    #   auth_token = github_pat(quiet = TRUE),
    #   upgrade_dependencies = FALSE,
    #   force = TRUE, ...)
    # })
    r = try({
      remotes::install_github(
        repo = x,
        auth_token = github_pat(quiet = TRUE),
        upgrade_dependencies = FALSE,
        upgrade = FALSE,
        force = TRUE, ...)
    })
    if (!inherits(r, "try-error")) {
      if (drop) {
        if (verbose) {
          message(paste0("Dropping Remote: ", x))
        }
        drop_remotes(
          path = path,
          drop_remotes = x,
          reorder = FALSE,
          verbose = verbose)
      }
    } else {
      if (!mustInstall) {
        msg = paste0("Installing Remote: ", x,
                     " failed!")
        stop(msg)
      }
    }
    return(r)
  })
  names(res) = remotes
  return(res)
}