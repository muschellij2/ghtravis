library(gh)
library(ghtravis)

organization = "adv-datasci"
assignment = "homework1"
pat = NULL
github_auth = ghtravis:::github_auth

org_table = function(organization, pat = NULL, ...) {
  if (is.null(pat)) {
    pat = github_pat(quiet = TRUE)
  }

  tag_url = paste0("https://api.github.com/orgs/", organization, "/repos")
  tag_res = httr::GET(tag_url, github_auth(pat)
  )
                      # , ...)
  httr::stop_for_status(tag_res)
  httr::message_for_status(tag_res)
  tag_content = httr::content(tag_res)


  tag_content = bind_list(tag_content)
  # if (!is.null(tag_content)) {
  #   if (ncol(tag_content) > 0) {
  #     cn = colnames(tag_content)
  #     cn[ cn == "name"] = "tag_name"
  #     colnames(tag_content) = cn
  #   }
  # }
  return(tag_content)
}



