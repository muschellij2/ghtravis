#' @title Tag Table
#' @description Parses a remote and gets a table of tags
#'
#' @param repo Remote repository name
#' @param pat GitHub Personal Authentication Token (PAT)
#' @param ... additional arguments to \code{\link[httr]{GET}}
#' @param mustWork Does the command need to work?  If fails,
#' will error.  But \code{FALSE} will try to pass
#' appropriate missing data value
#' @return \code{data.frame} of tags
#' @export
#'
#' @examples
#' repo = "stnava/ANTsR"
#' tag_table(repo)
#' tag_table("muschellij2/ANTsR")
#' tag_table("cran/psych@084bdd0ae2630cf31c26d97a6e13e59d3f0f66e6")
#' @importFrom httr GET content stop_for_status authenticate message_for_status
tag_table = function(
  repo, pat = NULL,
  mustWork = TRUE,
  ...) {
  if (is.null(pat)) {
    pat = github_pat(quiet = TRUE)
  }
  info = parse_one_remote(repo)
  user = info$username
  package = info$repo
  # ref = info$ref
  repo = paste0(user, "/", package)

  tag_url = paste0("https://api.github.com/repos/", repo, "/tags")
  tag_res = httr::GET(tag_url, github_auth(pat), ...)


  # args = list(url = tag_url)
  get_tag_content = function(tag_res) {
    if (!mustWork) {
      if (httr::status_code(tag_res) > 400) {
        return(NULL)
      }
    }
    httr::stop_for_status(tag_res)
    httr::message_for_status(tag_res)
    tag_content = httr::content(tag_res)


    tag_content = bind_list(tag_content)
    if (!is.null(tag_content)) {
      if (ncol(tag_content) > 0) {
        cn = colnames(tag_content)
        cn[ cn == "name"] = "tag_name"
        colnames(tag_content) = cn
      }
    }
    return(tag_content)
  }

  tag_content = get_tag_content(tag_res)
  links = tag_res$headers$link
  next_link = get_next(links, ind = "next")
  while (!is.null(next_link)) {
    url = next_link
    res = httr::GET(url, github_auth(pat), ...)
    ddf = get_tag_content(res)
    tag_content = rbind(tag_content, ddf)
    links = res$headers$link
    next_link = get_next(links, ind = "next")
  }

  return(tag_content)
}