#' Betty Deploy
#'
#' Publish a local site or all sites in a given directory to a github org. Set
#' a valid `GITHUB_PAT` token in your environment to use this non-interactively.
#'
#' @export
#' @rdname deploy
#' @param path local path of the website root directory
#' @param deploy_org name of github organization to publish gh-pages repo
#' @param deploy_url optional base domain name under which sites will be hosted.
#' Defaults to `https://{deploy_org}.github.io/pkg`.
deploy_site <- function(path, deploy_org, deploy_url = NULL){
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
  if(is.null(deploy_url))
    deploy_url <- sprintf("https://%s.github.io/%s", deploy_org, pkg)
  deploy_remote <- paste0('https://github.com/', deploy_repo)

  # Init a git repo
  gert::git_init()
  gert::git_config_set('user.name', "Betty Builder")
  gert::git_config_set('user.email', "noreply@ropensci.org")
  gert::git_add(".")
  if(nrow(gert::git_status()) == 0){
    cat(sprintf("git repo %s already seems up-to-date\n", pkg), file = stderr())
    return()
  }
  gert::git_commit_all(commit_message)
  gert::git_remote_add('origin', deploy_remote)
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
           homepage = deploy_url,
           description = paste0("auto-generated pkgdown website for:", pkg)
    )
  })
  cat(sprintf("Pushing to %s\n", deploy_remote), file = stderr())
  gert::git_push('origin', '+refs/heads/gh-pages:refs/heads/gh-pages', verbose = TRUE)
  return(deploy_remote)
}

#' @export
#' @rdname deploy
#' @param doc_root directory in which your websites are stored
#' @param site_root base url under which sites are hosted
deploy_all_sites <- function(doc_root, deploy_org = 'ropensci-docs', site_root = 'https://docs.ropensci.org'){
  sites <- list.dirs(doc_root, recursive = FALSE)
  sites <- grep("_TMP$", sites, value = TRUE, invert = TRUE)
  lapply(sites, function(path){
    deploy_url <- paste0(site_root, "/", basename(path))
    deploy_site(path = path, deploy_org = deploy_org, deploy_url = deploy_url)
  })
}
