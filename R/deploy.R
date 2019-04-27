#' Deploy
#'
#' Publish a local site to our ropensci-docs github org. Make sure you have
#' a valid `GITHUB_PAT` token in your environment to use this non-interactively.
#'
#' @export
#' @name deploy
#' @rdname deploy
#' @param path local path of the website
deploy_site <- function(path = "."){
  # Load metadata
  info <- jsonlite::fromJSON(file.path(path, 'info.json'))
  pkg <- info$pkg
  commit_url <- paste0(info$remote, "/commit/", substring(info$commit$commit,1,7))
  commit_message <- sprintf('Render from %s (%s...)', commit_url,
                            substring(trimws(info$commit$message), 1, 25))

  # Change to the repo dir for gert
  pwd <- getwd()
  on.exit(setwd(pwd))
  setwd(path)

  # Init a git repo
  gert::git_init()
  gert::git_config_set('user.name', "Betty Builder")
  gert::git_config_set('user.email', "noreply@ropensci.org")
  gert::git_add(".")
  gert::git_commit_all(commit_message)
  gert::git_remote_add('origin', paste0('https://github.com/ropensci-docs/', pkg))
  gert::git_branch_create("gh-pages")

  # Create repo if needed and push
  info <- tryCatch(gh::gh(paste0("/repos/ropensci-docs/", pkg)), http_error_404 = function(e){
    cat(sprintf("Creating new repo ropensci-docs/%s\n", pkg))
    gh::gh("/orgs/ropensci-docs/repos", .method = "POST",
           name = pkg,
           has_wiki = FALSE,
           has_issues = FALSE,
           has_projects = FALSE,
           has_downloads = FALSE,
           homepage = paste0("https://docs.ropensci.org/", pkg),
           description = paste0("auto-generated pkgdown website for ropensci/", pkg)
    )
  })
  gert::git_push('origin', '+refs/heads/gh-pages:refs/heads/gh-pages', verbose = TRUE)
}
