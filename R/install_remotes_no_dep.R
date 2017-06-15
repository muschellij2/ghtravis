#' @title Returns the specific package from Remotes
#' @description Subsets the remotes based on a package
#'
#' @param path Path to DESCRIPTION file
#' @param package Package to subset.  If NULL, then all will be installed
#' @param drop Remotes should be dropped after installing
#' @param ... arguments to pass to install_github
#' @return Character vector of remotes
#'
#' @importFrom devtools install_github
#' @return Output of \code{install_github}
#' @export
#'
#' @examples \dontrun{
#'   install_remotes_no_dep()
#' }
install_remotes_no_dep = function(
  path = "DESCRIPTION",
  package = NULL,
  drop = TRUE,
  ...) {
  if (!is.null(package)) {
    if (all(package == "")) {
      package = NULL
    }
  }
  remotes = subset_remote(path = path, package = package)
  if (length(remotes) == 0) {
    return(NULL)
  }
  if (remotes == "") {
    return(NULL)
  }
  lapply(remotes, devtools::install_github,
         upgrade_dependencies = FALSE, ...)
  if (drop) {
    drop_remotes(drop_remotes = remotes)
  }
}