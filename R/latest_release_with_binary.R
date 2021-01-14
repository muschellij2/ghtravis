#' @title Get the Latest Binary for a repository
#' @description Parses a remote and gets a URL for a binary release
#'
#' @param repo Remote repository name
#' @param pat GitHub Personal Authentication Token (PAT)
#' @param verbose print diagnostic messages
#' @param check_r_version Check if R version is in the name of the
#' tarball
#' @param force_sha Should the SHA be checked when trying to get
#' binaries
#' @param ... additional arguments to \code{\link[httr]{GET}}
#' @return URL of the binary
#' @export
#'
#' @examples
#' repo = "stnava/ANTsR"
#' res = latest_release_with_binary(repo)
#' res
#' repo = "neuroconductor/gifti"
#' res = latest_release_with_binary(repo)
#' res #'
#' @importFrom httr GET content stop_for_status authenticate
#' @importFrom devtools github_pat
latest_release_with_binary = function(
  repo,
  pat = NULL,
  verbose = TRUE,
  check_r_version = TRUE,
  force_sha = TRUE,
  ...){

  df = binary_release_table(repo = repo, pat = pat, verbose = verbose, ...)
  if (is.null(nrow(df))) {
    return(NA)
  }
  if (is.vector(df)) {
    if (all(is.na(df))) {
      return(NA)
    }
  }
  info = parse_one_remote(repo)
  user = info$username
  package = info$repo
  ref = info$ref
  repo = paste0(user, "/", package)


  ddf = df
  ddf = ddf[ grep(release_search_ext(), ddf$asset_name),]

  if (!(ref %in% c("master", ""))) {
    if (!(ref %in% ddf$commit.sha)) {
      warning(paste0(
        "SHA was given for repo: ", repo, ", but no release associated",
        " with it!",
        ifelse(force_sha,
               "not installing, returning NA",
               "")
        )
      )
      if (force_sha) {
        return(NA)
      }
    } else {
      ddf = ddf[ ddf$commit.sha %in% ref, ]
    }
  }

  # Neuroconductor versioning
  if (check_r_version) {
    check_str = paste0("_R", r_version(), sys_ext(), "$")
    check = grepl(pattern = check_str, ddf$asset_name)
    if (any(check)) {
      check[is.na(check)] = FALSE
      ddf = ddf[ check, ]
    } else {
      # change to fix ITKR installation future R version
      return(NA)
    }
  }
  # ord = order(ddf$asset_created_at, ddf$asset_updated_at, decreasing = TRUE)
  ord = order(ddf$asset_updated_at, ddf$asset_created_at, decreasing = TRUE)
  ddf = ddf[ord, ]
  if (verbose) {
    message("Table is")
    print(ddf)
  }
  if (nrow(ddf) > 0) {
    ddf = ddf[1,]
    url = ddf$asset_browser_download_url
  } else {
    url = NA
  }
  return(url)
}
