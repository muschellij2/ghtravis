#' @title Get GitHub PAT
#' @description Try to retrieve GitHub PAT from local environment
#'
#' @param quiet If TRUE, suppress status messages (if any), and the progress bar
#'
#' @return string containing the GitHub PAT
#' @export
#'
#' @examples
#' github_pat = github_pat()

github_pat <- function(quiet = TRUE) {
  pat <- Sys.getenv("GITHUB_PAT")
  if (nzchar(pat)) {
    if (!quiet) {
      message("Using GitHub PAT from envvar GITHUB_PAT")
    }
    return(pat)
  }
  return(NULL)
}