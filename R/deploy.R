#' Betty Deploy
#'
#' Publish a local site to our ropensci-docs github org. Make sure you have
#' a valid `GITHUB_PAT` token in your environment to use this non-interactively.
#'
#' @export
#' @rdname deploy
#' @param path local path of the website root directory
#' @param deploy_org name of github organization to publish gh-pages repo
#' @param deploy_url optional base domain name under which sites will be hosted.
#' Defaults to `https://{deploy_org}.github.io`.
deploy_site <- function(path, deploy_org, deploy_url = NULL){
  if(is.null(deploy_url))
    deploy_url <- sprintf("https://%s.github.io", deploy_org)

  # Load metadata
  info <- jsonlite::fromJSON(file.path(path, 'info.json'))
  commit_url <- paste0(info$remote, "/commit/", substring(info$commit$commit,1,7))
  commit_message <- sprintf('Render from %s (%s...)', commit_url,
                            substring(trimws(info$commit$message), 1, 25))

  # Change to the repo dir for gert
  pwd <- getwd()
  on.exit(setwd(pwd))
  setwd(path)

  # Metadata
  pkg <- info$pkg
  deploy_repo <- paste0(deploy_org, "/", pkg)

  # Init a git repo
  gert::git_init()
  gert::git_config_set('user.name', "Betty Builder")
  gert::git_config_set('user.email', "noreply@ropensci.org")
  gert::git_add(".")
  gert::git_commit_all(commit_message)
  gert::git_remote_add('origin', paste0('https://github.com/', deploy_repo))
  gert::git_branch_create("gh-pages")

  # Create repo if needed and push
  info <- tryCatch(gh::gh(paste0("/repos/", deploy_repo)), http_error_404 = function(e){
    cat(sprintf("Creating new repo %s\n", deploy_repo))
    gh::gh(paste0("/orgs/", deploy_org, "/repos"), .method = "POST",
           name = pkg,
           has_wiki = FALSE,
           has_issues = FALSE,
           has_projects = FALSE,
           has_downloads = FALSE,
           homepage = paste0(deploy_url, "/", pkg),
           description = paste0("auto-generated pkgdown website for:", pkg)
    )
  })
  gert::git_push('origin', '+refs/heads/gh-pages:refs/heads/gh-pages', verbose = TRUE)
}
